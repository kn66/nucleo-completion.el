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

#[derive(Clone, Copy)]
struct SortOptions {
    ties_by_length: bool,
    ties_alphabetically: bool,
    ignore_case: bool,
}

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
    score_items_with_sort(
        pattern,
        texts,
        SortOptions {
            ties_by_length: false,
            ties_alphabetically: false,
            ignore_case: false,
        },
    )
}

fn score_items_with_sort(
    pattern: &Pattern,
    texts: &[String],
    sort_options: SortOptions,
) -> Vec<ScoredIndex> {
    if texts.len() < PARALLEL_BATCH_SIZE {
        return sort_scored(
            with_matcher(|matcher| score_item_range(pattern, texts, 0, matcher)),
            texts,
            sort_options,
        );
    }

    let workers = parallel_worker_count(
        texts.len(),
        thread::available_parallelism()
            .map(usize::from)
            .unwrap_or(1),
    );

    if workers == 1 {
        return sort_scored(
            with_matcher(|matcher| score_item_range(pattern, texts, 0, matcher)),
            texts,
            sort_options,
        );
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

    sort_scored(scored, texts, sort_options)
}

fn parallel_worker_count(item_count: usize, available_parallelism: usize) -> usize {
    let batches = item_count.div_ceil(PARALLEL_BATCH_SIZE).max(1);
    available_parallelism.max(1).min(batches)
}

fn sort_scored(
    mut scored: Vec<ScoredIndex>,
    texts: &[String],
    sort_options: SortOptions,
) -> Vec<ScoredIndex> {
    let lengths = sort_options.ties_by_length.then(|| {
        texts
            .iter()
            .map(|text| text.chars().count())
            .collect::<Vec<_>>()
    });
    let folded_texts = (sort_options.ties_alphabetically && sort_options.ignore_case).then(|| {
        texts
            .iter()
            .map(|text| text.to_lowercase())
            .collect::<Vec<_>>()
    });

    scored.sort_unstable_by(|a, b| {
        b.score
            .cmp(&a.score)
            .then_with(|| match &lengths {
                Some(lengths) => lengths[a.index].cmp(&lengths[b.index]),
                None => std::cmp::Ordering::Equal,
            })
            .then_with(|| {
                if sort_options.ties_alphabetically {
                    match &folded_texts {
                        Some(folded_texts) => folded_texts[a.index].cmp(&folded_texts[b.index]),
                        None => texts[a.index].cmp(&texts[b.index]),
                    }
                } else {
                    std::cmp::Ordering::Equal
                }
            })
            .then_with(|| a.index.cmp(&b.index))
    });
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

#[defun]
fn sorted_filter<'e>(
    env: &'e Env,
    pattern: String,
    candidates: Value<'e>,
    ignore_case: Value<'e>,
    sort_ties_by_length: Value<'e>,
    sort_ties_alphabetically: Value<'e>,
) -> Result<Value<'e>> {
    let (values, texts) = collect_candidates(candidates)?;
    let ignore_case = ignore_case.is_not_nil();
    let pattern = Pattern::parse(&pattern, case_matching(ignore_case), Normalization::Smart);
    let matches = score_items_with_sort(
        &pattern,
        &texts,
        SortOptions {
            ties_by_length: sort_ties_by_length.is_not_nil(),
            ties_alphabetically: sort_ties_alphabetically.is_not_nil(),
            ignore_case,
        },
    );

    let mut result = env.intern("nil")?;
    for scored in matches.into_iter().rev() {
        result = env.cons(values[scored.index], result)?;
    }

    Ok(result)
}

#[defun]
fn sorted_scored_filter<'e>(
    env: &'e Env,
    pattern: String,
    candidates: Value<'e>,
    ignore_case: Value<'e>,
    sort_ties_by_length: Value<'e>,
    sort_ties_alphabetically: Value<'e>,
) -> Result<Value<'e>> {
    let (values, texts) = collect_candidates(candidates)?;
    let ignore_case = ignore_case.is_not_nil();
    let pattern = Pattern::parse(&pattern, case_matching(ignore_case), Normalization::Smart);
    let matches = score_items_with_sort(
        &pattern,
        &texts,
        SortOptions {
            ties_by_length: sort_ties_by_length.is_not_nil(),
            ties_alphabetically: sort_ties_alphabetically.is_not_nil(),
            ignore_case,
        },
    );

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

    #[test]
    fn sort_scored_can_break_ties_by_length() {
        let texts = vec!["alphabet".into(), "alpha".into(), "alpaca".into()];
        let scored = vec![
            ScoredIndex {
                index: 0,
                score: 10,
            },
            ScoredIndex {
                index: 1,
                score: 10,
            },
            ScoredIndex {
                index: 2,
                score: 11,
            },
        ];

        let sorted = sort_scored(
            scored,
            &texts,
            SortOptions {
                ties_by_length: true,
                ties_alphabetically: false,
                ignore_case: false,
            },
        );

        assert_eq!(
            sorted
                .into_iter()
                .map(|scored| scored.index)
                .collect::<Vec<_>>(),
            vec![2, 1, 0]
        );
    }

    #[test]
    fn sort_scored_can_break_ties_alphabetically() {
        let texts = vec!["beta".into(), "alpha".into(), "aardvark".into()];
        let scored = vec![
            ScoredIndex {
                index: 0,
                score: 10,
            },
            ScoredIndex {
                index: 1,
                score: 10,
            },
            ScoredIndex { index: 2, score: 9 },
        ];

        let sorted = sort_scored(
            scored,
            &texts,
            SortOptions {
                ties_by_length: false,
                ties_alphabetically: true,
                ignore_case: false,
            },
        );

        assert_eq!(
            sorted
                .into_iter()
                .map(|scored| scored.index)
                .collect::<Vec<_>>(),
            vec![1, 0, 2]
        );
    }

    #[test]
    fn sort_scored_applies_length_before_alphabetical() {
        let texts = vec!["bbb".into(), "aa".into(), "ccc".into(), "aaa".into()];
        let scored = vec![
            ScoredIndex {
                index: 0,
                score: 10,
            },
            ScoredIndex {
                index: 1,
                score: 10,
            },
            ScoredIndex {
                index: 2,
                score: 10,
            },
            ScoredIndex {
                index: 3,
                score: 10,
            },
        ];

        let sorted = sort_scored(
            scored,
            &texts,
            SortOptions {
                ties_by_length: true,
                ties_alphabetically: true,
                ignore_case: false,
            },
        );

        assert_eq!(
            sorted
                .into_iter()
                .map(|scored| scored.index)
                .collect::<Vec<_>>(),
            vec![1, 3, 0, 2]
        );
    }
}
