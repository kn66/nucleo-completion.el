//! Language-specific expansions used by the completion regexp pipeline.
use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::sync::OnceLock;

use crate::hangul::{HangulMatcher, SYLLABLES_PER_INITIAL};
use crate::migemo::MigemoMatcher;
use pinyin::ToPinyinMulti;

pub(crate) const MAX_TERM_CHARACTERS: usize = 32;
// Bound ambiguous pinyin suffix expansion and broad dictionary queries before
// passing their results back to Emacs.  Lisp also rejects invalid regexps.
pub(crate) const MAX_REGEXP_BYTES: usize = 100_000;
const CJK_IDEOGRAPH_START: u32 = 0x3400;
const CJK_IDEOGRAPH_END: u32 = 0x3134f;
static PINYIN: OnceLock<BTreeMap<String, String>> = OnceLock::new();

// Index all readings, including heteronyms, once; each value is safe inside an
// Emacs bracket expression because the indexed characters are CJK ideographs.
fn pinyin_index() -> BTreeMap<String, String> {
    let mut readings: BTreeMap<String, BTreeSet<char>> = BTreeMap::new();
    for code in CJK_IDEOGRAPH_START..=CJK_IDEOGRAPH_END {
        if let Some(character) = char::from_u32(code) {
            if let Some(pronunciations) = character.to_pinyin_multi() {
                for pronunciation in pronunciations {
                    readings
                        .entry(pronunciation.plain().replace('ü', "v"))
                        .or_default()
                        .insert(character);
                }
            }
        }
    }
    readings
        .into_iter()
        .map(|(syllable, chars)| (syllable, chars.into_iter().collect()))
        .collect()
}

// Normalize before indexing or slicing: all syllable and separator offsets are
// ASCII byte offsets, including when the input contains a multibyte ü.
fn normalize_pinyin(term: &str) -> Option<String> {
    if term.is_empty() || term.chars().count() > MAX_TERM_CHARACTERS {
        return None;
    }
    let mut normalized = String::new();
    let mut chars = term.chars().peekable();
    while let Some(character) = chars.next() {
        match character {
            'ü' | 'Ü' => normalized.push('v'),
            'u' | 'U' if chars.peek() == Some(&':') => {
                chars.next();
                normalized.push('v');
            }
            c if c.is_ascii_alphabetic() => normalized.push(c.to_ascii_lowercase()),
            '\'' if !normalized.is_empty() && !normalized.ends_with('\'') => {
                normalized.push('\'');
            }
            _ => return None,
        }
    }
    if normalized.ends_with('\'') || normalized.len() > MAX_TERM_CHARACTERS {
        return None;
    }
    Some(normalized)
}

pub fn pinyin_regexp(term: &str) -> Option<String> {
    let term = normalize_pinyin(term)?;
    expand_pinyin_suffixes(&term, PINYIN.get_or_init(pinyin_index))
}

enum PinyinStep {
    Complete(usize),
    Partial,
}

fn pinyin_step(syllable: &str, segment: &str, final_segment: bool) -> Option<PinyinStep> {
    if segment.starts_with(syllable) {
        // Consume the apostrophe only after the complete syllable before it.
        let separator = usize::from(!final_segment && syllable.len() == segment.len());
        Some(PinyinStep::Complete(syllable.len() + separator))
    } else if final_segment && syllable.starts_with(segment) {
        Some(PinyinStep::Partial)
    } else {
        None
    }
}

pub enum LanguageMatcher {
    Migemo(MigemoMatcher),
    Pinyin(PinyinMatcher),
    Hangul(HangulMatcher),
}

impl emacs::Transfer for LanguageMatcher {}

impl LanguageMatcher {
    pub fn find(&self, text: &str, start: usize, ignore_case: bool) -> Option<(usize, usize)> {
        match self {
            Self::Migemo(matcher) => matcher.find(text, start, ignore_case),
            Self::Pinyin(matcher) => matcher.find(text, start),
            Self::Hangul(matcher) => matcher.find(text, start, ignore_case),
        }
    }
}

pub struct PinyinMatcher {
    transitions: HashMap<String, Vec<(u64, u64)>>,
    terminal: u64,
}

impl PinyinMatcher {
    pub fn new(term: &str) -> Option<Self> {
        let term = normalize_pinyin(term)?;
        // Each bit is a byte position in the normalized query. Sharing suffix
        // states avoids duplicating character classes for every syllable split.
        let terminal = 1 << term.len();
        let mut reachable = terminal;
        let mut transitions = HashMap::<_, Vec<_>>::new();
        let index = PINYIN.get_or_init(pinyin_index);
        for start in (0..term.len()).rev() {
            if term.as_bytes()[start] == b'\'' {
                continue;
            }
            let remaining = &term[start..];
            let segment = remaining.split('\'').next().unwrap();
            for syllable in index.keys() {
                let end = match pinyin_step(syllable, segment, segment == remaining) {
                    Some(PinyinStep::Complete(length)) => start + length,
                    Some(PinyinStep::Partial) => term.len(),
                    None => continue,
                };
                let from = 1 << start;
                let to = 1 << end;
                if reachable & to != 0 {
                    transitions
                        .entry(syllable.clone())
                        .or_default()
                        .push((from, to));
                    reachable |= from;
                }
            }
        }
        (reachable & 1 != 0).then_some(Self {
            transitions,
            terminal,
        })
    }

    pub fn find(&self, text: &str, start: usize) -> Option<(usize, usize)> {
        let mut suffix = text.chars();
        for _ in 0..start {
            suffix.next()?;
        }
        let mut offset = start;
        while let Some(first) = suffix.next() {
            let mut states = 1;
            let mut end = None;
            for (index, character) in std::iter::once(first).chain(suffix.clone()).enumerate() {
                states = self.advance(states, character);
                if states == 0 {
                    break;
                }
                if states & self.terminal != 0 {
                    end = Some(offset + index + 1);
                }
                if states == self.terminal {
                    break;
                }
            }
            if let Some(end) = end {
                return Some((offset, end));
            }
            offset += 1;
        }
        None
    }

    fn advance(&self, states: u64, character: char) -> u64 {
        let mut next = 0;
        if let Some(readings) = character.to_pinyin_multi() {
            for reading in readings {
                let plain = reading.plain();
                let reading = if plain.contains('ü') {
                    Cow::Owned(plain.replace('ü', "v"))
                } else {
                    Cow::Borrowed(plain)
                };
                if let Some(edges) = self.transitions.get(reading.as_ref()) {
                    for &(from, to) in edges {
                        if states & from != 0 {
                            next |= to;
                        }
                    }
                }
            }
        }
        next
    }
}

// Normalization guarantees ASCII, so syllable and apostrophe positions are
// valid UTF-8 byte boundaries. Work backwards so alternatives with the same
// suffix share its computation.
fn expand_pinyin_suffixes(term: &str, index: &BTreeMap<String, String>) -> Option<String> {
    let mut suffixes = vec![None::<String>; term.len() + 1];
    suffixes[term.len()] = Some(String::new());
    for start in (0..term.len()).rev() {
        if term.as_bytes()[start] == b'\'' {
            continue;
        }
        let remaining = &term[start..];
        let segment = remaining.split('\'').next().unwrap();
        let final_segment = segment.len() == remaining.len();
        let mut alternatives = Vec::new();
        let mut partial = BTreeSet::new();
        for (syllable, characters) in index {
            match pinyin_step(syllable, segment, final_segment) {
                Some(PinyinStep::Complete(length)) => {
                    if let Some(suffix) = &suffixes[start + length] {
                        alternatives.push(format!("[{characters}]{suffix}"));
                    }
                }
                Some(PinyinStep::Partial) => partial.extend(characters.chars()),
                None => {}
            }
        }
        if !partial.is_empty() {
            alternatives.push(format!("[{}]", partial.into_iter().collect::<String>()));
        }
        if !alternatives.is_empty() {
            // Check both before joining and after adding grouping syntax, so an
            // oversized intermediate suffix is never retained for later starts.
            let length: usize = alternatives.iter().map(String::len).sum();
            if length > MAX_REGEXP_BYTES {
                return None;
            }
            let result = if alternatives.len() == 1 {
                alternatives.pop().unwrap()
            } else {
                format!("\\(?:{}\\)", alternatives.join("\\|"))
            };
            if result.len() > MAX_REGEXP_BYTES {
                return None;
            }
            suffixes[start] = Some(result);
        }
    }
    suffixes[0].take()
}

pub fn hangul_regexp(term: &str) -> Option<String> {
    const INITIALS: &str = "ㄱㄲㄴㄷㄸㄹㅁㅂㅃㅅㅆㅇㅈㅉㅊㅋㅌㅍㅎ";
    let mut result = String::new();
    let mut has_initial = false;
    if term.chars().count() > MAX_TERM_CHARACTERS {
        return None;
    }
    for character in term.chars() {
        let index = INITIALS.chars().position(|c| c == character).or_else(|| {
            let code = character as u32;
            (0x1100..=0x1112)
                .contains(&code)
                .then(|| (code - 0x1100) as usize)
        });
        if let Some(index) = index {
            has_initial = true;
            let first = char::from_u32(0xac00 + index as u32 * SYLLABLES_PER_INITIAL).unwrap();
            let last = char::from_u32(first as u32 + SYLLABLES_PER_INITIAL - 1).unwrap();
            result.push_str(&format!("[{character}{first}-{last}]"));
        } else if ('가'..='힣').contains(&character) || character.is_ascii_alphanumeric() {
            result.push(character);
        } else {
            return None;
        }
    }
    has_initial.then_some(result)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn pinyin_matches_ambiguous_terms_and_character_ranges_directly() {
        let matcher = PinyinMatcher::new("yinhang").unwrap();
        assert_eq!(matcher.find("😀銀行 银行", 0), Some((1, 3)));
        assert_eq!(matcher.find("😀銀行 银行", 3), Some((4, 6)));
        assert_eq!(matcher.find("银-行", 0), None);
        let term = "yinhang".repeat(4);
        assert_eq!(
            PinyinMatcher::new(&term)
                .unwrap()
                .find("银行银行银行银行", 0),
            Some((0, 8))
        );
    }

    #[test]
    fn pinyin_direct_matching_preserves_notation_and_boundaries() {
        for term in ["LÜ", "lv", "lu:"] {
            assert_eq!(
                PinyinMatcher::new(term).unwrap().find("绿", 0),
                Some((0, 1))
            );
        }
        let separated = PinyinMatcher::new("xi'an").unwrap();
        assert_eq!(separated.find("先", 0), None);
        assert_eq!(separated.find("西安", 0), Some((0, 2)));
        let partial = PinyinMatcher::new("zhongg").unwrap();
        assert_eq!(partial.find("中國", 0), Some((0, 2)));
        assert!(PinyinMatcher::new("txt").is_none());
        assert!(PinyinMatcher::new("zhong1guo2").is_none());
    }

    #[test]
    fn pinyin_suffix_expansion_respects_boundaries_and_size_limit() {
        let index = BTreeMap::from([
            ("ab".to_owned(), "甲".to_owned()),
            ("b".to_owned(), "乙".to_owned()),
        ]);
        // A prefix may stand in for an unfinished final syllable, but not one
        // followed by an explicit syllable boundary.
        assert!(expand_pinyin_suffixes("a", &index).is_some());
        assert!(expand_pinyin_suffixes("a'b", &index).is_none());
        assert!(expand_pinyin_suffixes("ab'b", &index).is_some());

        let oversized = BTreeMap::from([("a".to_owned(), "甲".repeat(MAX_REGEXP_BYTES))]);
        assert!(expand_pinyin_suffixes("a", &oversized).is_none());
    }
    #[test]
    fn pinyin_notation_and_boundaries() {
        let lv = pinyin_regexp("lv").unwrap();
        assert!(lv.contains('绿'));
        assert_eq!(lv, pinyin_regexp("LÜ").unwrap());
        assert_eq!(lv, pinyin_regexp("lu:").unwrap());
        assert_eq!(pinyin_regexp("lüe"), pinyin_regexp("lve"));
        let joined = pinyin_regexp("xian").unwrap();
        assert!(joined.contains('先') && joined.contains('西') && joined.contains('安'));
        let separated = pinyin_regexp("xi'an").unwrap();
        assert!(separated.contains('西') && separated.contains('安'));
        assert!(!separated.contains('先'));
        assert!(pinyin_regexp("xi'a").unwrap().contains('安'));
        for invalid in ["'xi", "xi'", "xi''an", "x:i", "ü😀", "xī", "u::"] {
            assert!(pinyin_regexp(invalid).is_none(), "{invalid}");
        }
        assert!(pinyin_regexp(&"ü".repeat(33)).is_none());
    }
    #[test]
    fn pinyin_handles_joined_and_partial_readings() {
        let regex = pinyin_regexp("zhongguo").unwrap();
        assert!(regex.contains('中') && regex.contains('国'));
        assert!(pinyin_regexp("zhongg").unwrap().contains('国'));
        assert!(pinyin_regexp("a*").is_none());
        assert!(pinyin_regexp("").is_none());
        assert!(pinyin_regexp("xian").unwrap().contains("\\|"));
        assert!(pinyin_regexp(&"a".repeat(33)).is_none());
        let first = PINYIN.get().unwrap() as *const _;
        pinyin_regexp("guo");
        assert_eq!(first, PINYIN.get().unwrap() as *const _);
    }
    #[test]
    fn hangul_initials_and_mixed_literals() {
        assert_eq!(hangul_regexp("ㄱㅅ").unwrap(), "[ㄱ가-깋][ㅅ사-싷]");
        assert_eq!(hangul_regexp("가ᄂ").unwrap(), "가[ᄂ나-닣]");
        assert!(hangul_regexp("가나").is_none());
        assert!(hangul_regexp("ㄱ*").is_none());
        assert!(hangul_regexp("").is_none());
        assert!(hangul_regexp(&"ㄱ".repeat(33)).is_none());
    }
}
