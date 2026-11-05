mod languages;
mod regexp;

use regexp::RegexpGroups;

use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::HashMap;

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

struct CandidateScan<'e, T> {
    matches: Vec<T>,
    regexp_only: Vec<RegexpOnlyCandidate<'e>>,
}

struct RegexpOnlyCandidate<'e> {
    value: Value<'e>,
    non_ascii: bool,
}

struct ScoredCandidate<'e> {
    index: usize,
    value: Value<'e>,
    score: u32,
}

struct ScoredCandidateWithTies<'e> {
    value: Value<'e>,
    sort_data: CandidateSortData,
}

trait ScoredCandidateEntry<'e> {
    fn value(&self) -> Value<'e>;
    fn score(&self) -> u32;
}

impl<'e> ScoredCandidateEntry<'e> for ScoredCandidate<'e> {
    fn value(&self) -> Value<'e> {
        self.value
    }

    fn score(&self) -> u32 {
        self.score
    }
}

impl<'e> ScoredCandidateEntry<'e> for ScoredCandidateWithTies<'e> {
    fn value(&self) -> Value<'e> {
        self.value
    }

    fn score(&self) -> u32 {
        self.sort_data.score
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct CandidateSortData {
    index: usize,
    score: u32,
    history_rank: Option<usize>,
    length: Option<usize>,
    alphabetical_key: Option<String>,
}

#[derive(Default)]
struct MatchIndicesScratch {
    utf32_buf: Vec<char>,
    indices: Vec<u32>,
}

const MODULE_VERSION: &str = env!("CARGO_PKG_VERSION");
const INPUT_PENDING_CHECK_INTERVAL: usize = 256;

#[derive(Clone, Copy)]
struct SortOptions {
    ties_by_length: bool,
    ties_alphabetically: bool,
    ignore_case: bool,
}

impl SortOptions {
    fn from_lisp<'e>(
        sort_ties_by_length: Value<'e>,
        sort_ties_alphabetically: Value<'e>,
        ignore_case: bool,
    ) -> Self {
        Self {
            ties_by_length: sort_ties_by_length.is_not_nil(),
            ties_alphabetically: sort_ties_alphabetically.is_not_nil(),
            ignore_case,
        }
    }
}

fn case_matching(ignore_case: bool) -> CaseMatching {
    if ignore_case {
        CaseMatching::Ignore
    } else {
        CaseMatching::Respect
    }
}

thread_local! {
    /// Matcher cached on the main Emacs thread for serial scoring and
    /// for the small post-sort indices pass.  Reused across module
    /// invocations so that we do not pay for `Matcher::new` per call.
    static MATCHER: RefCell<Matcher> =
        RefCell::new(Matcher::new(Config::DEFAULT.match_paths()));
}

fn with_matcher<T>(function: impl FnOnce(&mut Matcher) -> T) -> T {
    MATCHER.with(|matcher| function(&mut matcher.borrow_mut()))
}

fn collect_history_ranks<'e>(history_ranks: Value<'e>) -> Result<Vec<Option<usize>>> {
    let mut ranks = Vec::new();
    let mut list = history_ranks;

    while list.is_not_nil() {
        let value: Value<'e> = list.car()?;
        ranks.push(if value.is_not_nil() {
            Some(value.into_rust()?)
        } else {
            None
        });
        list = list.cdr()?;
    }

    Ok(ranks)
}

enum HistoryRanks<'a> {
    ByIndex(&'a [Option<usize>]),
    ByText(&'a HashMap<String, usize>),
}

impl HistoryRanks<'_> {
    fn lookup(&self, index: usize, text: &str) -> Option<usize> {
        match self {
            Self::ByIndex(ranks) => ranks.get(index).copied().flatten(),
            Self::ByText(ranks) => ranks.get(text).copied(),
        }
    }
}

fn collect_history_entries<'e>(
    env: &Env,
    mut list: Value<'e>,
) -> Result<Option<HashMap<String, usize>>> {
    let mut ranks = HashMap::new();
    let mut count = 0;
    while list.is_not_nil() {
        let entry: Value<'e> = list.car()?;
        let key: String = entry.car::<Value>()?.into_rust()?;
        let rank: usize = entry.cdr::<Value>()?.into_rust()?;
        ranks.entry(key).or_insert(rank);
        list = list.cdr()?;
        count += 1;
        if interrupt_after_processed(env, count)? {
            return Ok(None);
        }
    }
    Ok(Some(ranks))
}

fn score_lisp_candidates<'e>(
    env: &Env,
    pattern: &Pattern,
    candidates: Value<'e>,
    regexp_groups: Option<&RegexpGroups>,
) -> Result<Option<CandidateScan<'e, ScoredCandidate<'e>>>> {
    with_matcher(|matcher| {
        let mut matches = Vec::new();
        let mut regexp_only = Vec::new();
        let mut list = candidates;
        let mut index = 0;
        let mut utf32_buf = Vec::new();

        while list.is_not_nil() {
            let value: Value<'e> = list.car()?;
            let text: String = value.into_rust()?;
            if let Some(candidate) =
                scored_lisp_candidate(pattern, value, index, &text, matcher, &mut utf32_buf)
            {
                matches.push(candidate);
            } else if regexp_groups.is_some_and(|groups| groups.is_match(&text)) {
                regexp_only.push(RegexpOnlyCandidate {
                    value,
                    non_ascii: !text.is_ascii(),
                });
            }
            list = list.cdr()?;
            index += 1;
            if input_pending_check_due(index) && input_pending(env)? {
                return Ok(None);
            }
        }

        Ok(Some(CandidateScan {
            matches,
            regexp_only,
        }))
    })
}

fn scored_lisp_candidate<'e>(
    pattern: &Pattern,
    value: Value<'e>,
    index: usize,
    text: &str,
    matcher: &mut Matcher,
    utf32_buf: &mut Vec<char>,
) -> Option<ScoredCandidate<'e>> {
    pattern
        .score(Utf32Str::new(text, utf32_buf), matcher)
        .map(|score| ScoredCandidate {
            index,
            value,
            score,
        })
}

fn sort_scored_candidates<'e>(mut matches: Vec<ScoredCandidate<'e>>) -> Vec<ScoredCandidate<'e>> {
    matches.sort_unstable_by(compare_scored_candidates);
    matches
}

fn compare_scored_candidates(a: &ScoredCandidate, b: &ScoredCandidate) -> Ordering {
    b.score.cmp(&a.score).then_with(|| a.index.cmp(&b.index))
}

fn score_lisp_candidates_with_ties<'e>(
    env: &Env,
    pattern: &Pattern,
    candidates: Value<'e>,
    sort_options: SortOptions,
    history_ranks: Option<&HistoryRanks>,
    regexp_groups: Option<&RegexpGroups>,
) -> Result<Option<CandidateScan<'e, ScoredCandidateWithTies<'e>>>> {
    with_matcher(|matcher| {
        let mut matches = Vec::new();
        let mut regexp_only = Vec::new();
        let mut list = candidates;
        let mut index = 0;
        let mut utf32_buf = Vec::new();

        while list.is_not_nil() {
            let value: Value<'e> = list.car()?;
            let text: String = value.into_rust()?;
            if let Some(candidate) = scored_lisp_candidate_with_ties(
                pattern,
                value,
                index,
                &text,
                matcher,
                sort_options,
                history_ranks,
                &mut utf32_buf,
            ) {
                matches.push(candidate);
            } else if regexp_groups.is_some_and(|groups| groups.is_match(&text)) {
                regexp_only.push(RegexpOnlyCandidate {
                    value,
                    non_ascii: !text.is_ascii(),
                });
            }
            list = list.cdr()?;
            index += 1;
            if input_pending_check_due(index) && input_pending(env)? {
                return Ok(None);
            }
        }

        Ok(Some(CandidateScan {
            matches,
            regexp_only,
        }))
    })
}

#[allow(
    clippy::too_many_arguments,
    reason = "Candidate scoring keeps hot-path data explicit"
)]
fn scored_lisp_candidate_with_ties<'e>(
    pattern: &Pattern,
    value: Value<'e>,
    index: usize,
    text: &str,
    matcher: &mut Matcher,
    sort_options: SortOptions,
    history_ranks: Option<&HistoryRanks>,
    utf32_buf: &mut Vec<char>,
) -> Option<ScoredCandidateWithTies<'e>> {
    pattern
        .score(Utf32Str::new(text, utf32_buf), matcher)
        .map(|score| ScoredCandidateWithTies {
            value,
            sort_data: CandidateSortData {
                index,
                score,
                history_rank: history_ranks.and_then(|ranks| ranks.lookup(index, text)),
                length: sort_options.ties_by_length.then(|| text.chars().count()),
                alphabetical_key: candidate_alphabetical_key(text, sort_options),
            },
        })
}

fn candidate_alphabetical_key(text: &str, sort_options: SortOptions) -> Option<String> {
    if !sort_options.ties_alphabetically {
        return None;
    }

    Some(if sort_options.ignore_case {
        text.to_lowercase()
    } else {
        text.to_owned()
    })
}

fn sort_scored_candidates_with_ties<'e>(
    mut matches: Vec<ScoredCandidateWithTies<'e>>,
) -> Vec<ScoredCandidateWithTies<'e>> {
    matches.sort_unstable_by(|a, b| compare_candidate_sort_data(&a.sort_data, &b.sort_data));
    matches
}

fn compare_candidate_sort_data(a: &CandidateSortData, b: &CandidateSortData) -> Ordering {
    b.score
        .cmp(&a.score)
        .then_with(|| compare_candidate_history_tie(a, b))
        .then_with(|| compare_candidate_length_tie(a, b))
        .then_with(|| compare_candidate_alphabetical_tie(a, b))
        .then_with(|| a.index.cmp(&b.index))
}

fn compare_candidate_history_tie(a: &CandidateSortData, b: &CandidateSortData) -> Ordering {
    match (a.history_rank, b.history_rank) {
        (Some(rank_a), Some(rank_b)) => rank_a.cmp(&rank_b),
        (Some(_), None) => Ordering::Less,
        (None, Some(_)) => Ordering::Greater,
        (None, None) => Ordering::Equal,
    }
}

fn compare_candidate_alphabetical_tie(a: &CandidateSortData, b: &CandidateSortData) -> Ordering {
    match (&a.alphabetical_key, &b.alphabetical_key) {
        (Some(key_a), Some(key_b)) => key_a.cmp(key_b),
        _ => Ordering::Equal,
    }
}

fn compare_candidate_length_tie(a: &CandidateSortData, b: &CandidateSortData) -> Ordering {
    match (a.length, b.length) {
        (Some(length_a), Some(length_b)) => length_a.cmp(&length_b),
        _ => Ordering::Equal,
    }
}

fn needs_candidate_tie_data(
    sort_options: SortOptions,
    history_ranks: Option<&[Option<usize>]>,
) -> bool {
    history_ranks.is_some() || sort_options.ties_by_length || sort_options.ties_alphabetically
}

/// Compute the highlight indices for one already-matched candidate.
///
/// `pattern.indices` re-runs the dynamic-programming match in order to
/// record positions, but the call only happens for the at most
/// `highlight_limit` top-ranked candidates and so its cost is bounded
/// by a small constant (default 25) regardless of the candidate-set
/// size.  The scratch vectors are reused across top-info entries so
/// the bounded second pass does not allocate fresh UTF-32 or index
/// buffers per highlighted candidate.  Score and indices share the
/// underlying algorithm; coalescing them into a single per-candidate
/// call would force storing indices for every match instead of just for
/// the top-ranked few, so the deliberate two-phase shape is retained.
fn matched_indices_into<'a>(
    pattern: &Pattern,
    text: &str,
    matcher: &mut Matcher,
    scratch: &'a mut MatchIndicesScratch,
) -> Option<&'a [u32]> {
    scratch.indices.clear();
    pattern.indices(
        Utf32Str::new(text, &mut scratch.utf32_buf),
        matcher,
        &mut scratch.indices,
    )?;
    scratch.indices.sort_unstable();
    scratch.indices.dedup();
    Some(&scratch.indices)
}

#[cfg(test)]
fn matched_indices(pattern: &Pattern, text: &str, matcher: &mut Matcher) -> Option<Vec<u32>> {
    let mut scratch = MatchIndicesScratch::default();
    matched_indices_into(pattern, text, matcher, &mut scratch).map(Vec::from)
}

fn indices_to_lisp<'e>(env: &'e Env, indices: &[u32]) -> Result<Value<'e>> {
    let mut result = env.intern("nil")?;
    for &index in indices.iter().rev() {
        result = env.cons(index.into_lisp(env)?, result)?;
    }
    Ok(result)
}

/// Ask Emacs whether new input is waiting.
///
/// The Emacs Lisp `input-pending-p` predicate lets the module cooperate with
/// `while-no-input`.  Callers abandon the current pass and return an explicit
/// interrupted bundle, so Lisp can reuse the last compatible filtered result
/// without confusing interruption with an ordinary search that found no matches.
fn input_pending(env: &Env) -> Result<bool> {
    let args: [Value; 0] = [];
    Ok(env.call("input-pending-p", args)?.is_not_nil())
}

fn input_pending_check_due(processed_count: usize) -> bool {
    processed_count.checked_rem(INPUT_PENDING_CHECK_INTERVAL) == Some(0)
}

fn interrupt_after_processed(env: &Env, processed_count: usize) -> Result<bool> {
    Ok(input_pending_check_due(processed_count) && input_pending(env)?)
}

fn build_list_3<'e>(env: &'e Env, a: Value<'e>, b: Value<'e>, c: Value<'e>) -> Result<Value<'e>> {
    let nil = env.intern("nil")?;
    env.cons(a, env.cons(b, env.cons(c, nil)?)?)
}

fn interrupted_bundle<'e>(env: &'e Env) -> Result<Value<'e>> {
    let nil = env.intern("nil")?;
    let sentinel = env.intern("nucleo-completion-interrupted")?;
    build_list_3(env, sentinel, nil, nil)
}

fn build_scored_candidate_list<'e, T>(env: &'e Env, matches: &[T]) -> Result<Option<Value<'e>>>
where
    T: ScoredCandidateEntry<'e>,
{
    let nil = env.intern("nil")?;
    let mut candidates_list = nil;
    let mut processed_count = 0;
    for scored in matches.iter().rev() {
        candidates_list = env.cons(scored.value(), candidates_list)?;
        processed_count += 1;
        if interrupt_after_processed(env, processed_count)? {
            return Ok(None);
        }
    }
    Ok(Some(candidates_list))
}

fn build_scored_top_info<'e, T>(
    env: &'e Env,
    pattern: &Pattern,
    matches: &[T],
    highlight_limit: usize,
) -> Result<Option<Value<'e>>>
where
    T: ScoredCandidateEntry<'e>,
{
    build_scored_top_info_onto(env, pattern, matches, highlight_limit, env.intern("nil")?)
}

fn build_scored_top_info_onto<'e, T>(
    env: &'e Env,
    pattern: &Pattern,
    matches: &[T],
    highlight_limit: usize,
    tail: Value<'e>,
) -> Result<Option<Value<'e>>>
where
    T: ScoredCandidateEntry<'e>,
{
    let nil = env.intern("nil")?;
    let mut top_info = tail;
    let mut scratch = MatchIndicesScratch::default();
    let mut processed_count = 0;
    for scored in matches.iter().take(highlight_limit).rev() {
        let value = scored.value();
        let text: String = value.into_rust()?;
        let indices_value = with_matcher(|matcher| {
            match matched_indices_into(pattern, &text, matcher, &mut scratch) {
                Some(indices) => indices_to_lisp(env, indices),
                None => Ok(nil),
            }
        })?;
        let entry = env.cons(
            value,
            env.cons(
                scored.score().into_lisp(env)?,
                env.cons(indices_value, nil)?,
            )?,
        )?;
        top_info = env.cons(entry, top_info)?;
        processed_count += 1;
        if interrupt_after_processed(env, processed_count)? {
            return Ok(None);
        }
    }
    Ok(Some(top_info))
}

fn build_scored_full_scores<'e, T>(
    env: &'e Env,
    matches: &[T],
    return_all_scores: bool,
) -> Result<Option<Value<'e>>>
where
    T: ScoredCandidateEntry<'e>,
{
    let nil = env.intern("nil")?;
    if !return_all_scores {
        return Ok(Some(nil));
    }

    let mut full_scores = nil;
    let mut processed_count = 0;
    for scored in matches.iter().rev() {
        full_scores = env.cons(scored.score().into_lisp(env)?, full_scores)?;
        processed_count += 1;
        if interrupt_after_processed(env, processed_count)? {
            return Ok(None);
        }
    }
    Ok(Some(full_scores))
}

fn build_scored_candidate_bundle<'e, T>(
    env: &'e Env,
    pattern: &Pattern,
    matches: &[T],
    highlight_limit: usize,
    return_all_scores: bool,
    regexp_only: Option<&[RegexpOnlyCandidate<'e>]>,
) -> Result<Value<'e>>
where
    T: ScoredCandidateEntry<'e>,
{
    let Some(candidates_list) = build_scored_candidate_list(env, matches)? else {
        return interrupted_bundle(env);
    };
    if input_pending(env)? {
        return interrupted_bundle(env);
    }
    let Some(top_info) = build_scored_top_info(env, pattern, matches, highlight_limit)? else {
        return interrupted_bundle(env);
    };
    if input_pending(env)? {
        return interrupted_bundle(env);
    }
    let Some(full_scores) = build_scored_full_scores(env, matches, return_all_scores)? else {
        return interrupted_bundle(env);
    };
    if input_pending(env)? {
        return interrupted_bundle(env);
    }
    if let Some(regexp_only) = regexp_only {
        let mut extras = env.intern("nil")?;
        for (index, candidate) in regexp_only.iter().rev().enumerate() {
            extras = env.cons(candidate.value, extras)?;
            if interrupt_after_processed(env, index + 1)? {
                return interrupted_bundle(env);
            }
        }
        if input_pending(env)? {
            return interrupted_bundle(env);
        }
        let tail = env.cons(extras, env.intern("nil")?)?;
        env.cons(
            candidates_list,
            env.cons(top_info, env.cons(full_scores, tail)?)?,
        )
    } else {
        build_list_3(env, candidates_list, top_info, full_scores)
    }
}

#[allow(clippy::too_many_arguments, reason = "Emacs module API is positional")]
fn candidates_impl<'e>(
    env: &'e Env,
    pattern: String,
    candidates: Value<'e>,
    ignore_case: Value<'e>,
    sort_ties_by_length: Value<'e>,
    sort_ties_alphabetically: Value<'e>,
    history_ranks: Option<Value<'e>>,
    highlight_limit: Value<'e>,
    return_all_scores: Value<'e>,
    regexp_groups: Option<&RegexpGroups>,
) -> Result<Value<'e>> {
    if input_pending(env)? {
        return interrupted_bundle(env);
    }

    let ignore_case = ignore_case.is_not_nil();
    let sort_options =
        SortOptions::from_lisp(sort_ties_by_length, sort_ties_alphabetically, ignore_case);
    let history_ranks = match history_ranks {
        Some(value) => Some(collect_history_ranks(value)?),
        None => None,
    };
    let pattern = Pattern::parse(&pattern, case_matching(ignore_case), Normalization::Smart);
    let highlight_limit = highlight_limit.into_rust::<usize>()?;
    let return_all_scores = return_all_scores.is_not_nil();
    if needs_candidate_tie_data(sort_options, history_ranks.as_deref()) {
        let matches = match score_lisp_candidates_with_ties(
            env,
            &pattern,
            candidates,
            sort_options,
            history_ranks.as_deref().map(HistoryRanks::ByIndex).as_ref(),
            regexp_groups,
        )? {
            Some(matches) => matches,
            None => return interrupted_bundle(env),
        };

        if input_pending(env)? {
            return interrupted_bundle(env);
        }

        let regexp_only = matches.regexp_only;
        let matches = sort_scored_candidates_with_ties(matches.matches);

        if input_pending(env)? {
            return interrupted_bundle(env);
        }

        build_scored_candidate_bundle(
            env,
            &pattern,
            &matches,
            highlight_limit,
            return_all_scores,
            regexp_groups.map(|_| regexp_only.as_slice()),
        )
    } else {
        let matches = match score_lisp_candidates(env, &pattern, candidates, regexp_groups)? {
            Some(matches) => matches,
            None => return interrupted_bundle(env),
        };

        if input_pending(env)? {
            return interrupted_bundle(env);
        }

        let regexp_only = matches.regexp_only;
        let matches = sort_scored_candidates(matches.matches);

        if input_pending(env)? {
            return interrupted_bundle(env);
        }

        build_scored_candidate_bundle(
            env,
            &pattern,
            &matches,
            highlight_limit,
            return_all_scores,
            regexp_groups.map(|_| regexp_only.as_slice()),
        )
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum RegexpPriority {
    Before,
    After,
    NonAscii,
}

impl RegexpPriority {
    fn promotes(self, non_ascii: bool) -> bool {
        match self {
            Self::Before => true,
            Self::After => false,
            Self::NonAscii => non_ascii,
        }
    }
}

// Construct each result directly in reverse order, keeping the fuzzy highlight
// limit independent of the number of promoted regexp matches.
#[allow(
    clippy::too_many_arguments,
    reason = "Bundle assembly keeps options explicit"
)]
fn build_merged_bundle<'e, T: ScoredCandidateEntry<'e>>(
    env: &'e Env,
    pattern: &Pattern,
    matches: &[T],
    extras: &[RegexpOnlyCandidate<'e>],
    priority: RegexpPriority,
    highlight_limit: usize,
    return_all_scores: bool,
    include_top_info: bool,
) -> Result<Value<'e>> {
    let mut before = Vec::new();
    let mut after = Vec::new();
    for (index, candidate) in extras.iter().enumerate() {
        if priority.promotes(candidate.non_ascii) {
            before.push(candidate.value);
        } else {
            after.push(candidate.value);
        }
        if interrupt_after_processed(env, index + 1)? {
            return interrupted_bundle(env);
        }
    }
    let nil = env.intern("nil")?;
    let score = matches.first().map_or(1, |candidate| candidate.score());
    // The compatibility merge extends full scores only when fuzzy scores exist.
    let return_all_scores = return_all_scores && !matches.is_empty();
    let mut candidates = nil;
    let mut scores = nil;
    let mut count = 0;
    let mut tail = nil;
    if include_top_info {
        for &value in after.iter().rev() {
            let entry = build_list_3(env, value, score.into_lisp(env)?, nil)?;
            tail = env.cons(entry, tail)?;
            count += 1;
            if interrupt_after_processed(env, count)? {
                return interrupted_bundle(env);
            }
        }
    }
    let Some(mut top) = build_scored_top_info_onto(env, pattern, matches, highlight_limit, tail)?
    else {
        return interrupted_bundle(env);
    };
    if include_top_info {
        for &value in before.iter().rev() {
            let entry = build_list_3(env, value, score.into_lisp(env)?, nil)?;
            top = env.cons(entry, top)?;
            count += 1;
            if interrupt_after_processed(env, count)? {
                return interrupted_bundle(env);
            }
        }
    }
    for &value in after.iter().rev() {
        candidates = env.cons(value, candidates)?;
        if return_all_scores {
            scores = env.cons(score.into_lisp(env)?, scores)?;
        }
        count += 1;
        if interrupt_after_processed(env, count)? {
            return interrupted_bundle(env);
        }
    }
    for candidate in matches.iter().rev() {
        candidates = env.cons(candidate.value(), candidates)?;
        if return_all_scores {
            scores = env.cons(candidate.score().into_lisp(env)?, scores)?;
        }
        count += 1;
        if interrupt_after_processed(env, count)? {
            return interrupted_bundle(env);
        }
    }
    for &value in before.iter().rev() {
        candidates = env.cons(value, candidates)?;
        if return_all_scores {
            scores = env.cons(score.into_lisp(env)?, scores)?;
        }
        count += 1;
        if interrupt_after_processed(env, count)? {
            return interrupted_bundle(env);
        }
    }
    if input_pending(env)? {
        return interrupted_bundle(env);
    }
    let fuzzy_span = env.cons(before.len().into_lisp(env)?, matches.len().into_lisp(env)?)?;
    let tail = env.cons(fuzzy_span, nil)?;
    env.cons(candidates, env.cons(top, env.cons(scores, tail)?)?)
}

/// Return merged candidates, top information, scores, and a fuzzy match span.
/// The fourth slot is (BEFORE-COUNT . FUZZY-COUNT), identifying the fuzzy
/// candidates within the merged result.
/// HISTORY-ENTRIES is an alist of normalized candidate strings and integer ranks.
/// Non-nil REGEXP-GROUPS adds regexp-only matches according to REGEXP-PRIORITY;
/// INCLUDE-REGEXP-TOP-INFO adds synthetic top entries without reducing the
/// number of highlighted fuzzy matches.  Unsupported regexp semantics return
/// `nucleo-completion-unsupported-regexp' for the Lisp fallback.
#[allow(clippy::too_many_arguments, reason = "Emacs module API is positional")]
#[defun]
fn candidates_with_options<'e>(
    env: &'e Env,
    pattern: String,
    candidates: Value<'e>,
    ignore_case: Value<'e>,
    sort_ties_by_length: Value<'e>,
    sort_ties_alphabetically: Value<'e>,
    history_entries: Value<'e>,
    highlight_limit: Value<'e>,
    return_all_scores: Value<'e>,
    regexp_groups: Value<'e>,
    regexp_priority: Value<'e>,
    include_regexp_top_info: Value<'e>,
) -> Result<Value<'e>> {
    if input_pending(env)? {
        return interrupted_bundle(env);
    }
    let groups = if regexp_groups.is_not_nil() {
        if ignore_case.is_not_nil() {
            return env.intern("nucleo-completion-unsupported-regexp");
        }
        let Some(groups) = RegexpGroups::from_lisp(regexp_groups)? else {
            return env.intern("nucleo-completion-unsupported-regexp");
        };
        Some(groups)
    } else {
        None
    };
    let Some(history) = collect_history_entries(env, history_entries)? else {
        return interrupted_bundle(env);
    };
    let priority = if regexp_priority == env.intern("before")? {
        RegexpPriority::Before
    } else if regexp_priority == env.intern("after")? {
        RegexpPriority::After
    } else {
        RegexpPriority::NonAscii
    };
    let ignore_case = ignore_case.is_not_nil();
    let options =
        SortOptions::from_lisp(sort_ties_by_length, sort_ties_alphabetically, ignore_case);
    let pattern = Pattern::parse(&pattern, case_matching(ignore_case), Normalization::Smart);
    let limit: usize = highlight_limit.into_rust()?;
    let all_scores = return_all_scores.is_not_nil();
    let include_top = include_regexp_top_info.is_not_nil();
    if !history.is_empty() || options.ties_by_length || options.ties_alphabetically {
        let resolver = HistoryRanks::ByText(&history);
        let Some(scan) = score_lisp_candidates_with_ties(
            env,
            &pattern,
            candidates,
            options,
            Some(&resolver),
            groups.as_ref(),
        )?
        else {
            return interrupted_bundle(env);
        };
        if input_pending(env)? {
            return interrupted_bundle(env);
        }
        let matches = sort_scored_candidates_with_ties(scan.matches);
        if input_pending(env)? {
            return interrupted_bundle(env);
        }
        build_merged_bundle(
            env,
            &pattern,
            &matches,
            &scan.regexp_only,
            priority,
            limit,
            all_scores,
            include_top,
        )
    } else {
        let Some(scan) = score_lisp_candidates(env, &pattern, candidates, groups.as_ref())? else {
            return interrupted_bundle(env);
        };
        if input_pending(env)? {
            return interrupted_bundle(env);
        }
        let matches = sort_scored_candidates(scan.matches);
        if input_pending(env)? {
            return interrupted_bundle(env);
        }
        build_merged_bundle(
            env,
            &pattern,
            &matches,
            &scan.regexp_only,
            priority,
            limit,
            all_scores,
            include_top,
        )
    }
}

/// Return an Emacs regexp for TERM using the bundled Japanese dictionary.
/// Return nil for unsupported terms or expansions exceeding the size limit.
#[defun]
fn migemo_regexp<'e>(env: &'e Env, term: String) -> Result<Value<'e>> {
    languages::migemo_regexp(&term).into_lisp(env)
}

/// Return an Emacs regexp for toneless pinyin TERM.
/// Accept ü, v, or u: and apostrophe syllable boundaries.  Include
/// alternative readings and a partial final syllable; return nil
/// for unsupported terms or expansions exceeding the size limit.
#[defun]
fn pinyin_regexp<'e>(env: &'e Env, term: String) -> Result<Value<'e>> {
    languages::pinyin_regexp(&term).into_lisp(env)
}

/// Return an Emacs regexp for Hangul initial-consonant TERM.
/// Allow mixed syllables and ASCII alphanumeric literals; return nil when
/// there is no initial consonant or TERM contains unsupported characters.
#[defun]
fn hangul_regexp<'e>(env: &'e Env, term: String) -> Result<Value<'e>> {
    languages::hangul_regexp(&term).into_lisp(env)
}

#[defun]
fn module_version<'e>(env: &'e Env) -> Result<Value<'e>> {
    MODULE_VERSION.into_lisp(env)
}

#[allow(clippy::too_many_arguments, reason = "Emacs module API is positional")]
#[defun]
fn candidates<'e>(
    env: &'e Env,
    pattern: String,
    candidates: Value<'e>,
    ignore_case: Value<'e>,
    sort_ties_by_length: Value<'e>,
    sort_ties_alphabetically: Value<'e>,
    highlight_limit: Value<'e>,
    return_all_scores: Value<'e>,
) -> Result<Value<'e>> {
    candidates_impl(
        env,
        pattern,
        candidates,
        ignore_case,
        sort_ties_by_length,
        sort_ties_alphabetically,
        None,
        highlight_limit,
        return_all_scores,
        None,
    )
}

#[allow(clippy::too_many_arguments, reason = "Emacs module API is positional")]
#[defun]
fn candidates_with_history<'e>(
    env: &'e Env,
    pattern: String,
    candidates: Value<'e>,
    ignore_case: Value<'e>,
    sort_ties_by_length: Value<'e>,
    sort_ties_alphabetically: Value<'e>,
    history_ranks: Value<'e>,
    highlight_limit: Value<'e>,
    return_all_scores: Value<'e>,
) -> Result<Value<'e>> {
    candidates_impl(
        env,
        pattern,
        candidates,
        ignore_case,
        sort_ties_by_length,
        sort_ties_alphabetically,
        Some(history_ranks),
        highlight_limit,
        return_all_scores,
        None,
    )
}

/// Return fuzzy matches and candidates matching every regexp group but not fuzzy.
/// Return `nucleo-completion-unsupported-regexp` when native matching cannot
/// preserve Emacs regexp semantics, allowing Lisp to use its original path.
#[allow(clippy::too_many_arguments, reason = "Emacs module API is positional")]
#[defun]
fn candidates_with_regexp_groups<'e>(
    env: &'e Env,
    pattern: String,
    candidates: Value<'e>,
    ignore_case: Value<'e>,
    sort_ties_by_length: Value<'e>,
    sort_ties_alphabetically: Value<'e>,
    history_ranks: Value<'e>,
    highlight_limit: Value<'e>,
    return_all_scores: Value<'e>,
    regexp_groups: Value<'e>,
) -> Result<Value<'e>> {
    if input_pending(env)? {
        return interrupted_bundle(env);
    }
    if ignore_case.is_not_nil() {
        return env.intern("nucleo-completion-unsupported-regexp");
    }
    let Some(groups) = RegexpGroups::from_lisp(regexp_groups)? else {
        return env.intern("nucleo-completion-unsupported-regexp");
    };
    candidates_impl(
        env,
        pattern,
        candidates,
        ignore_case,
        sort_ties_by_length,
        sort_ties_alphabetically,
        history_ranks.is_not_nil().then_some(history_ranks),
        highlight_limit,
        return_all_scores,
        Some(&groups),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sort_data(
        index: usize,
        score: u32,
        history_rank: Option<usize>,
        length: Option<usize>,
        alphabetical_key: Option<&str>,
    ) -> CandidateSortData {
        CandidateSortData {
            index,
            score,
            history_rank,
            length,
            alphabetical_key: alphabetical_key.map(ToOwned::to_owned),
        }
    }

    fn sorted_indices(mut items: Vec<CandidateSortData>) -> Vec<usize> {
        items.sort_unstable_by(compare_candidate_sort_data);
        items.into_iter().map(|item| item.index).collect()
    }

    #[test]
    fn history_lookup_uses_exact_candidate_text_or_legacy_index() {
        let ranks = HashMap::from([("Alpha".to_owned(), 2), ("beta".to_owned(), 0)]);
        let by_text = HistoryRanks::ByText(&ranks);
        assert_eq!(by_text.lookup(99, "Alpha"), Some(2));
        assert_eq!(by_text.lookup(0, "alpha"), None);
        let legacy = [None, Some(3)];
        let by_index = HistoryRanks::ByIndex(&legacy);
        assert_eq!(by_index.lookup(1, "anything"), Some(3));
        assert_eq!(by_index.lookup(2, "anything"), None);
    }

    #[test]
    fn regexp_priority_partitions_in_source_order() {
        let values = ["ascii", "日本語", "ascii", "é"];
        for (priority, expected_before, expected_after) in [
            (
                RegexpPriority::Before,
                vec!["ascii", "日本語", "ascii", "é"],
                vec![],
            ),
            (
                RegexpPriority::After,
                vec![],
                vec!["ascii", "日本語", "ascii", "é"],
            ),
            (
                RegexpPriority::NonAscii,
                vec!["日本語", "é"],
                vec!["ascii", "ascii"],
            ),
        ] {
            let (before, after): (Vec<_>, Vec<_>) = values
                .iter()
                .copied()
                .partition(|s| priority.promotes(!s.is_ascii()));
            assert_eq!(before, expected_before);
            assert_eq!(after, expected_after);
        }
    }

    #[test]
    fn candidate_sort_data_can_break_ties_by_length() {
        assert_eq!(
            sorted_indices(vec![
                sort_data(0, 10, None, Some(8), None),
                sort_data(1, 10, None, Some(5), None),
                sort_data(2, 11, None, Some(6), None),
            ]),
            vec![2, 1, 0]
        );
    }

    #[test]
    fn candidate_sort_data_can_break_ties_alphabetically() {
        assert_eq!(
            sorted_indices(vec![
                sort_data(0, 10, None, None, Some("beta")),
                sort_data(1, 10, None, None, Some("alpha")),
                sort_data(2, 9, None, None, Some("aardvark")),
            ]),
            vec![1, 0, 2]
        );
    }

    #[test]
    fn candidate_sort_data_applies_length_before_alphabetical() {
        assert_eq!(
            sorted_indices(vec![
                sort_data(0, 10, None, Some(3), Some("bbb")),
                sort_data(1, 10, None, Some(2), Some("aa")),
                sort_data(2, 10, None, Some(3), Some("ccc")),
                sort_data(3, 10, None, Some(3), Some("aaa")),
            ]),
            vec![1, 3, 0, 2]
        );
    }

    #[test]
    fn candidate_sort_data_applies_history_before_length_and_alphabetical() {
        assert_eq!(
            sorted_indices(vec![
                sort_data(0, 10, Some(1), Some(3), Some("bbb")),
                sort_data(1, 10, None, Some(2), Some("aa")),
                sort_data(2, 9, Some(0), Some(3), Some("ccc")),
                sort_data(3, 10, Some(0), Some(3), Some("aaa")),
            ]),
            vec![3, 0, 1, 2]
        );
    }

    #[test]
    fn candidate_sort_data_treats_missing_history_ranks_as_unranked() {
        assert_eq!(
            sorted_indices(vec![
                sort_data(0, 10, Some(1), None, None),
                sort_data(1, 10, Some(0), None, None),
                sort_data(2, 10, None, None, None),
            ]),
            vec![1, 0, 2]
        );
    }

    #[test]
    fn candidate_alphabetical_key_honors_ignore_case() {
        assert_eq!(
            candidate_alphabetical_key(
                "Beta",
                SortOptions {
                    ties_by_length: false,
                    ties_alphabetically: true,
                    ignore_case: true,
                },
            ),
            Some("beta".to_owned())
        );
        assert_eq!(
            candidate_alphabetical_key(
                "Beta",
                SortOptions {
                    ties_by_length: false,
                    ties_alphabetically: true,
                    ignore_case: false,
                },
            ),
            Some("Beta".to_owned())
        );
        assert_eq!(
            candidate_alphabetical_key(
                "Beta",
                SortOptions {
                    ties_by_length: false,
                    ties_alphabetically: false,
                    ignore_case: true,
                },
            ),
            None
        );
    }

    #[test]
    fn matched_indices_returns_sorted_unique_positions() {
        let pattern = Pattern::parse("foo", CaseMatching::Respect, Normalization::Smart);
        let indices = with_matcher(|matcher| matched_indices(&pattern, "xfoox", matcher));

        assert_eq!(indices, Some(vec![1, 2, 3]));
    }

    #[test]
    fn matched_indices_scratch_clears_between_candidates() {
        let pattern = Pattern::parse("foo", CaseMatching::Respect, Normalization::Smart);
        let mut scratch = MatchIndicesScratch::default();

        let first = with_matcher(|matcher| {
            matched_indices_into(&pattern, "xfoox", matcher, &mut scratch).map(Vec::from)
        });
        let second = with_matcher(|matcher| {
            matched_indices_into(&pattern, "foo", matcher, &mut scratch).map(Vec::from)
        });

        assert_eq!(first, Some(vec![1, 2, 3]));
        assert_eq!(second, Some(vec![0, 1, 2]));
    }

    #[test]
    fn input_pending_check_due_runs_at_interval() {
        assert!(!input_pending_check_due(1));
        assert!(!input_pending_check_due(INPUT_PENDING_CHECK_INTERVAL - 1));
        assert!(input_pending_check_due(INPUT_PENDING_CHECK_INTERVAL));
        assert!(input_pending_check_due(INPUT_PENDING_CHECK_INTERVAL * 2));
    }
}
