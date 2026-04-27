use std::cell::RefCell;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::thread;

use emacs::{defun, Env, IntoLisp, Result, Value};
use nucleo_matcher::pattern::{CaseMatching, Normalization, Pattern};
use nucleo_matcher::{Config, Matcher, Utf32Str};

emacs::plugin_is_GPL_compatible!();

#[emacs::module(
    name = "nucleo-completion-module",
    defun_prefix = "nucleo-completion",
    mod_in_name = false
)]
fn init(_: &Env) -> Result<()> {
    Ok(())
}

struct ScoredIndex {
    index: usize,
    score: u32,
}

const PARALLEL_BATCH_SIZE: usize = 2048;

fn case_matching(ignore_case: bool) -> CaseMatching {
    if ignore_case {
        CaseMatching::Ignore
    } else {
        CaseMatching::Respect
    }
}

thread_local! {
    static MATCHER: RefCell<Matcher> = RefCell::new(Matcher::new(Config::DEFAULT.match_paths()));
}

fn with_matcher<T>(function: impl FnOnce(&mut Matcher) -> T) -> T {
    MATCHER.with(|matcher| function(&mut matcher.borrow_mut()))
}

fn collect_candidates<'e>(candidates: Value<'e>) -> Result<(Vec<Value<'e>>, Vec<String>)> {
    let mut values = Vec::new();
    let mut texts = Vec::new();
    let mut list = candidates;

    while list.is_not_nil() {
        let value: Value<'e> = list.car()?;
        let text: String = value.into_rust()?;
        values.push(value);
        texts.push(text);
        list = list.cdr()?;
    }

    Ok((values, texts))
}

fn score_items(pattern: &Pattern, texts: &[String]) -> Vec<ScoredIndex> {
    if texts.len() < PARALLEL_BATCH_SIZE {
        return sort_scored(with_matcher(|matcher| {
            score_item_range(pattern, texts, 0, matcher)
        }));
    }

    let workers = parallel_worker_count(
        texts.len(),
        thread::available_parallelism().map(usize::from).unwrap_or(1),
    );

    if workers == 1 {
        return sort_scored(with_matcher(|matcher| {
            score_item_range(pattern, texts, 0, matcher)
        }));
    }

    let batch_count = texts.len().div_ceil(PARALLEL_BATCH_SIZE);
    let next_batch = AtomicUsize::new(0);
    let scored = thread::scope(|scope| {
        let mut handles = Vec::with_capacity(workers);
        for _ in 0..workers {
            let next_batch = &next_batch;
            handles.push(scope.spawn(move || {
                let mut matcher = Matcher::new(Config::DEFAULT.match_paths());
                let mut scored = Vec::new();
                loop {
                    let batch_index = next_batch.fetch_add(1, Ordering::Relaxed);
                    if batch_index >= batch_count {
                        break;
                    }

                    let start = batch_index * PARALLEL_BATCH_SIZE;
                    let end = (start + PARALLEL_BATCH_SIZE).min(texts.len());
                    scored.extend(score_item_range(
                        pattern,
                        &texts[start..end],
                        start,
                        &mut matcher,
                    ));
                }
                scored
            }));
        }

        let mut scored = Vec::new();
        for handle in handles {
            scored.extend(handle.join().expect("nucleo worker thread panicked"));
        }
        scored
    });

    sort_scored(scored)
}

fn parallel_worker_count(item_count: usize, available_parallelism: usize) -> usize {
    let batches = item_count.div_ceil(PARALLEL_BATCH_SIZE).max(1);
    available_parallelism.max(1).min(batches)
}

fn sort_scored(mut scored: Vec<ScoredIndex>) -> Vec<ScoredIndex> {
    scored.sort_unstable_by(|a, b| b.score.cmp(&a.score).then_with(|| a.index.cmp(&b.index)));
    scored
}

fn score_item_range(
    pattern: &Pattern,
    texts: &[String],
    offset: usize,
    matcher: &mut Matcher,
) -> Vec<ScoredIndex> {
    let mut buf = Vec::new();
    texts
        .iter()
        .enumerate()
        .filter_map(|(index, text)| {
            pattern
                .score(Utf32Str::new(text, &mut buf), matcher)
                .map(|score| ScoredIndex {
                    index: offset + index,
                    score,
                })
        })
        .collect()
}

#[defun]
fn score(pattern: String, candidate: String, ignore_case: Value) -> Result<Option<u32>> {
    let pattern = Pattern::parse(
        &pattern,
        case_matching(ignore_case.is_not_nil()),
        Normalization::Smart,
    );

    Ok(with_matcher(|matcher| {
        pattern
            .match_list([candidate], matcher)
            .into_iter()
            .next()
            .map(|(_, score)| score)
    }))
}

#[defun]
fn filter<'e>(
    env: &'e Env,
    pattern: String,
    candidates: Value<'e>,
    ignore_case: Value<'e>,
) -> Result<Value<'e>> {
    let (values, texts) = collect_candidates(candidates)?;
    let pattern = Pattern::parse(
        &pattern,
        case_matching(ignore_case.is_not_nil()),
        Normalization::Smart,
    );
    let matches = score_items(&pattern, &texts);

    let mut result = env.intern("nil")?;
    for scored in matches.into_iter().rev() {
        result = env.cons(values[scored.index], result)?;
    }

    Ok(result)
}

#[defun]
fn scored_filter<'e>(
    env: &'e Env,
    pattern: String,
    candidates: Value<'e>,
    ignore_case: Value<'e>,
) -> Result<Value<'e>> {
    let (values, texts) = collect_candidates(candidates)?;
    let pattern = Pattern::parse(
        &pattern,
        case_matching(ignore_case.is_not_nil()),
        Normalization::Smart,
    );
    let matches = score_items(&pattern, &texts);

    let mut result = env.intern("nil")?;
    for scored in matches.into_iter().rev() {
        let pair = env.cons(values[scored.index], scored.score.into_lisp(env)?)?;
        result = env.cons(pair, result)?;
    }

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parallel_worker_count_caps_to_batches() {
        assert_eq!(parallel_worker_count(PARALLEL_BATCH_SIZE, 8), 1);
        assert_eq!(parallel_worker_count(PARALLEL_BATCH_SIZE + 1, 8), 2);
        assert_eq!(parallel_worker_count(PARALLEL_BATCH_SIZE * 3, 8), 3);
    }

    #[test]
    fn parallel_worker_count_caps_to_available_parallelism() {
        assert_eq!(parallel_worker_count(PARALLEL_BATCH_SIZE * 8, 2), 2);
        assert_eq!(parallel_worker_count(PARALLEL_BATCH_SIZE * 8, 0), 1);
    }
}
