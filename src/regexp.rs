//! Native candidate matching for regexp and language alternatives.
//!
//! Emacs and Rust regexp syntax differ.  Anything outside this small shared
//! subset must use Emacs instead; in particular this does not implement Emacs
//! character classes, word boundaries, backreferences, or general case
//! folding.  Built-in fuzzy alternatives can carry their Emacs case
//! equivalence classes explicitly.
use emacs::{Result, Value};
use regex::{Regex, RegexBuilder};

use crate::languages::LanguageMatcher;

// Limit both the input retained and each compiled automaton.  Broad language
// expansions should fall back to Emacs rather than consume unbounded memory.
const MAX_TOTAL_BYTES: usize = 1_000_000;
const MAX_PATTERNS: usize = 256;
const MAX_COMPILED_BYTES: usize = 2_000_000;

enum CandidateMatcher<'e> {
    Regexp(Regex),
    Language(&'e LanguageMatcher),
    FoldedFuzzy(Vec<Vec<char>>),
    #[cfg(test)]
    Counted(&'e std::cell::Cell<usize>, Option<u8>, bool),
}

impl CandidateMatcher<'_> {
    fn match_quality(&self, text: &str, ignore_case: bool) -> Option<u8> {
        match self {
            Self::Regexp(regexp) => regexp.is_match(text).then_some(0),
            Self::Language(matcher) => matcher.match_quality(text, ignore_case),
            Self::FoldedFuzzy(classes) => {
                let mut characters = text.chars();
                classes
                    .iter()
                    .all(|class| characters.any(|character| class.contains(&character)))
                    .then_some(0)
            }
            #[cfg(test)]
            Self::Counted(count, result, _) => {
                count.set(count.get() + 1);
                if text == "miss" {
                    None
                } else {
                    *result
                }
            }
        }
    }

    fn has_filename_priority(&self) -> bool {
        match self {
            Self::Language(matcher) => matcher.has_filename_priority(),
            #[cfg(test)]
            Self::Counted(_, _, rankable) => *rankable,
            _ => false,
        }
    }
}

/// Reused across candidates; cached results never survive a scan.
pub struct RegexpScratch {
    results: Vec<Option<Option<u8>>>,
}

pub struct RegexpGroups<'e> {
    matchers: Vec<CandidateMatcher<'e>>,
    groups: Vec<Vec<usize>>,
    rankable: Vec<usize>,
    memoize: bool,
    ignore_case: bool,
}

impl<'e> RegexpGroups<'e> {
    pub fn from_lisp(groups: Value<'e>, ignore_case: bool) -> Result<Option<Self>> {
        let mut list = groups;
        let mut compiled = Vec::new();
        let mut matchers = Vec::new();
        // Emacs eq compares object identity for strings, cons cells, and user
        // pointers.  Reused handles can therefore share a single scan result.
        let mut identities: Vec<Value<'e>> = Vec::new();
        let mut count = 0;
        let mut bytes = 0;
        let user_ptr = groups.env.intern("user-ptr")?;
        let cons = groups.env.intern("cons")?;
        let folded_fuzzy = groups.env.intern("nucleo-completion--folded-fuzzy")?;
        while list.is_not_nil() {
            let mut alternatives: Value = list.car()?;
            let mut group = Vec::new();
            while alternatives.is_not_nil() {
                count += 1;
                if count > MAX_PATTERNS {
                    return Ok(None);
                }
                let value: Value = alternatives.car()?;
                if let Some(index) = identities.iter().position(|identity| *identity == value) {
                    group.push(index);
                    alternatives = alternatives.cdr()?;
                    continue;
                }
                let value_type = value.env.type_of(value)?;
                let matcher = if value_type == user_ptr {
                    CandidateMatcher::Language(value.into_rust()?)
                } else if ignore_case {
                    if value_type != cons || value.car::<Value>()? != folded_fuzzy {
                        return Ok(None);
                    }
                    let mut classes: Value = value.cdr()?;
                    let mut fuzzy = Vec::new();
                    while classes.is_not_nil() {
                        let class: String = classes.car()?;
                        bytes += class.len();
                        if class.is_empty() || bytes > MAX_TOTAL_BYTES {
                            return Ok(None);
                        }
                        fuzzy.push(class.chars().collect());
                        classes = classes.cdr()?;
                    }
                    if fuzzy.is_empty() {
                        return Ok(None);
                    }
                    CandidateMatcher::FoldedFuzzy(fuzzy)
                } else {
                    let source: String = value.into_rust()?;
                    bytes += source.len();
                    if bytes > MAX_TOTAL_BYTES {
                        return Ok(None);
                    }
                    let Some(regex) = compile(&source) else {
                        return Ok(None);
                    };
                    CandidateMatcher::Regexp(regex)
                };
                group.push(matchers.len());
                identities.push(value);
                matchers.push(matcher);
                alternatives = alternatives.cdr()?;
            }
            compiled.push(group);
            list = list.cdr()?;
        }
        let rankable: Vec<usize> = matchers
            .iter()
            .enumerate()
            .filter_map(|(index, matcher)| matcher.has_filename_priority().then_some(index))
            .collect();
        let memoize = count > matchers.len() || !rankable.is_empty();
        Ok(Some(Self {
            groups: compiled,
            matchers,
            rankable,
            memoize,
            ignore_case,
        }))
    }

    pub fn scratch(&self) -> RegexpScratch {
        RegexpScratch {
            results: vec![None; if self.memoize { self.matchers.len() } else { 0 }],
        }
    }

    fn evaluate(&self, index: usize, text: &str, scratch: &mut RegexpScratch) -> Option<u8> {
        if self.memoize {
            *scratch.results[index]
                .get_or_insert_with(|| self.matchers[index].match_quality(text, self.ignore_case))
        } else {
            self.matchers[index].match_quality(text, self.ignore_case)
        }
    }

    /// Match an AND of OR groups, returning the best matching filename priority.
    pub fn match_quality(&self, text: &str, scratch: &mut RegexpScratch) -> Option<u8> {
        scratch.results.fill(None);
        for group in &self.groups {
            if !group
                .iter()
                .any(|&index| self.evaluate(index, text, scratch).is_some())
            {
                return None;
            }
        }
        let mut quality = 0;
        for &index in &self.rankable {
            if let Some(priority) = self.evaluate(index, text, scratch) {
                quality = quality.max(priority);
            }
        }
        Some(quality)
    }

    #[cfg(test)]
    fn is_match(&self, text: &str) -> bool {
        self.match_quality(text, &mut self.scratch()).is_some()
    }
}

fn compile(source: &str) -> Option<Regex> {
    let translated = translate(source)?;
    RegexBuilder::new(&translated)
        .size_limit(MAX_COMPILED_BYTES)
        .dfa_size_limit(MAX_COMPILED_BYTES)
        .build()
        .ok()
}

// Bare parentheses, braces, and vertical bars are literals in Emacs, while
// escaped parentheses and vertical bars introduce groups and alternatives.
fn translate(source: &str) -> Option<String> {
    let mut output = String::with_capacity(source.len());
    let mut chars = source.chars().peekable();
    while let Some(ch) = chars.next() {
        match ch {
            '\\' => match chars.next()? {
                '(' => {
                    output.push('(');
                    if chars.peek() == Some(&'?') {
                        chars.next();
                        if chars.next()? != ':' {
                            return None;
                        }
                        output.push_str("?:");
                    }
                }
                ')' => output.push(')'),
                '|' => output.push('|'),
                '`' => output.push_str("\\A"),
                c @ ('.' | '^' | '$' | '*' | '+' | '?' | '[' | ']' | '\\') => {
                    output.push('\\');
                    output.push(c);
                }
                _ => return None,
            },
            '[' => {
                output.push('[');
                // Character sets generated by the backends contain literals
                // and ranges.  Exclude nested/POSIX sets and Rust set algebra.
                let mut has_char = false;
                let mut first = true;
                loop {
                    let c = chars.next()?;
                    if first && c == '^' {
                        output.push(c);
                        first = false;
                        continue;
                    }
                    if c == ']' {
                        if !has_char {
                            return None;
                        }
                        output.push(c);
                        break;
                    }
                    if matches!(c, '[' | '\\' | '&' | '~' | '|')
                        || (c == '-' && chars.peek() == Some(&'-'))
                    {
                        return None;
                    }
                    output.push(c);
                    has_char = true;
                    first = false;
                }
            }
            // These context-sensitive Emacs operators are deliberately not
            // translated.  Our generators only need dot and star.
            '^' | '$' | '+' | '?' => return None,
            c @ ('(' | ')' | '|' | '{' | '}' | ']') => {
                output.push('\\');
                output.push(c);
            }
            c => output.push(c),
        }
    }
    Some(output)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn generated_patterns_and_literal_punctuation() {
        let regex = compile(r"\(?:搜索\|[ㄱ가-깋][ㅅ사-싷]\)").unwrap();
        assert!(regex.is_match("x搜索y"));
        assert!(regex.is_match("가사"));
        assert!(!regex.is_match("가나"));
        assert!(compile(r"\`a(b)|{c}\+\?").unwrap().is_match("a(b)|{c}+?"));
        assert!(!compile(r"\`abc").unwrap().is_match("xabc"));
    }

    #[test]
    fn fuzzy_dot_and_newline_alternative() {
        assert!(!compile("a.*b").unwrap().is_match("a\nb"));
        assert!(compile("a\\(?:.\\|\n\\)*b").unwrap().is_match("a\nb"));
    }

    #[test]
    fn groups_are_and_of_or_and_empty_group_matches_nothing() {
        let regexp = |source| CandidateMatcher::Regexp(compile(source).unwrap());
        let groups = RegexpGroups {
            matchers: vec![regexp("a"), regexp("b"), regexp("c")],
            groups: vec![vec![0, 1], vec![2]],
            rankable: vec![],
            memoize: false,
            ignore_case: false,
        };
        assert!(groups.is_match("bc"));
        assert!(!groups.is_match("b"));
        assert!(!RegexpGroups {
            matchers: vec![],
            groups: vec![vec![]],
            rankable: vec![],
            memoize: false,
            ignore_case: false,
        }
        .is_match("anything"));
    }

    #[test]
    fn folded_fuzzy_uses_the_supplied_character_equivalences() {
        let groups = RegexpGroups {
            matchers: vec![CandidateMatcher::FoldedFuzzy(vec![
                vec!['k', 'K'],
                vec!['σ', 'ς', 'Σ'],
            ])],
            groups: vec![vec![0]],
            rankable: vec![],
            memoize: false,
            ignore_case: true,
        };
        assert!(groups.is_match("日本-K\nς"));
        assert!(!groups.is_match("日本-K\nς"));
        assert!(!groups.is_match("Σ-K"));
    }

    #[test]
    fn shared_alternatives_are_evaluated_once_per_candidate() {
        use std::cell::Cell;
        let count = Cell::new(0);
        let unused = Cell::new(0);
        let missing = Cell::new(0);
        let groups = RegexpGroups {
            matchers: vec![
                CandidateMatcher::Counted(&count, Some(0), false),
                CandidateMatcher::Counted(&unused, Some(0), false),
                CandidateMatcher::Counted(&missing, None, false),
            ],
            groups: vec![vec![2, 0, 1], vec![2, 0]],
            rankable: vec![],
            memoize: true,
            ignore_case: false,
        };
        let mut scratch = groups.scratch();
        assert_eq!(groups.match_quality("hit", &mut scratch), Some(0));
        assert_eq!(count.get(), 1);
        assert_eq!(missing.get(), 1);
        assert_eq!(unused.get(), 0);
        assert_eq!(groups.match_quality("miss", &mut scratch), None);
        assert_eq!(count.get(), 2);
        assert_eq!(missing.get(), 2);
        assert_eq!(unused.get(), 1);
        assert_eq!(groups.match_quality("hit", &mut scratch), Some(0));
        assert_eq!(count.get(), 3);
        assert_eq!(missing.get(), 3);
    }

    #[test]
    fn ranked_matches_are_checked_after_groups_pass() {
        use std::cell::Cell;
        let normal = Cell::new(0);
        let priority = Cell::new(0);
        let groups = RegexpGroups {
            matchers: vec![
                CandidateMatcher::Counted(&normal, Some(0), false),
                CandidateMatcher::Counted(&priority, Some(2), true),
            ],
            groups: vec![vec![0, 1], vec![0, 1]],
            rankable: vec![1],
            memoize: true,
            ignore_case: false,
        };
        let mut scratch = groups.scratch();
        assert_eq!(groups.match_quality("hit", &mut scratch), Some(2));
        assert_eq!((normal.get(), priority.get()), (1, 1));
        assert_eq!(groups.match_quality("miss", &mut scratch), None);
        assert_eq!((normal.get(), priority.get()), (2, 2));
    }

    #[test]
    fn unsupported_syntax_fails_closed() {
        for source in [
            r"\bword",
            r"\s-",
            r"\1",
            r"\{2\}",
            "[[:alpha:]]",
            "[a&&b]",
            "[a||b]",
            "a+",
            "^a",
            r"\(?=x\)",
            "[abc",
            "\\",
        ] {
            assert!(compile(source).is_none(), "{source:?}");
        }
    }
}
