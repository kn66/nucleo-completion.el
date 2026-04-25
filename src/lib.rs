use emacs::{defun, Env, IntoLisp, Result, Value};
use nucleo_matcher::pattern::{CaseMatching, Normalization, Pattern};
use nucleo_matcher::{Config, Matcher};

emacs::plugin_is_GPL_compatible!();

#[emacs::module(
    name = "nucleo-completion-module",
    defun_prefix = "nucleo-completion",
    mod_in_name = false
)]
fn init(_: &Env) -> Result<()> {
    Ok(())
}

struct Candidate<'e> {
    value: Value<'e>,
    text: String,
}

impl AsRef<str> for Candidate<'_> {
    fn as_ref(&self) -> &str {
        &self.text
    }
}

fn case_matching(ignore_case: bool) -> CaseMatching {
    if ignore_case {
        CaseMatching::Ignore
    } else {
        CaseMatching::Respect
    }
}

fn new_matcher() -> Matcher {
    Matcher::new(Config::DEFAULT.match_paths())
}

#[defun]
fn score(pattern: String, candidate: String, ignore_case: Value) -> Result<Option<u32>> {
    let mut matcher = new_matcher();
    let pattern = Pattern::parse(
        &pattern,
        case_matching(ignore_case.is_not_nil()),
        Normalization::Smart,
    );

    Ok(pattern
        .match_list([candidate], &mut matcher)
        .into_iter()
        .next()
        .map(|(_, score)| score))
}

#[defun]
fn filter<'e>(
    env: &'e Env,
    pattern: String,
    candidates: Value<'e>,
    ignore_case: Value<'e>,
) -> Result<Value<'e>> {
    let mut items = Vec::new();
    let mut list = candidates;

    while list.is_not_nil() {
        let value: Value<'e> = list.car()?;
        let text: String = value.into_rust()?;
        items.push(Candidate { value, text });
        list = list.cdr()?;
    }

    let mut matcher = new_matcher();
    let pattern = Pattern::parse(
        &pattern,
        case_matching(ignore_case.is_not_nil()),
        Normalization::Smart,
    );
    let matches = pattern.match_list(items, &mut matcher);

    let mut result = env.intern("nil")?;
    for (candidate, _) in matches.into_iter().rev() {
        result = env.cons(candidate.value, result)?;
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
    let mut items = Vec::new();
    let mut list = candidates;

    while list.is_not_nil() {
        let value: Value<'e> = list.car()?;
        let text: String = value.into_rust()?;
        items.push(Candidate { value, text });
        list = list.cdr()?;
    }

    let mut matcher = new_matcher();
    let pattern = Pattern::parse(
        &pattern,
        case_matching(ignore_case.is_not_nil()),
        Normalization::Smart,
    );
    let matches = pattern.match_list(items, &mut matcher);

    let mut result = env.intern("nil")?;
    for (candidate, score) in matches.into_iter().rev() {
        let pair = env.cons(candidate.value, score.into_lisp(env)?)?;
        result = env.cons(pair, result)?;
    }

    Ok(result)
}
