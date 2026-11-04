//! Language-specific expansions used by the completion regexp pipeline.
use std::collections::{BTreeMap, BTreeSet};
use std::sync::OnceLock;

use pinyin::ToPinyinMulti;
use rustmigemo::migemo::{
    compact_dictionary::CompactDictionary,
    query::query_a_word_with_generator,
    regex_generator::{RegexGeneratorTrait, RegexOperator},
    ternary_regex_generator::TernaryRegexGenerator,
};

const MAX_TERM_CHARACTERS: usize = 32;
// Bound ambiguous pinyin suffix expansion and broad dictionary queries before
// passing their results back to Emacs.  Lisp also rejects invalid regexps.
const MAX_REGEXP_BYTES: usize = 100_000;
// Unicode orders modern syllables by initial, then 21 vowels and 28 finals.
const HANGUL_SYLLABLES_PER_INITIAL: u32 = 21 * 28;
const CJK_IDEOGRAPH_START: u32 = 0x3400;
const CJK_IDEOGRAPH_END: u32 = 0x3134f;
static MIGEMO: OnceLock<CompactDictionary> = OnceLock::new();
static PINYIN: OnceLock<BTreeMap<String, String>> = OnceLock::new();

fn roman_term(term: &str) -> bool {
    !term.is_empty()
        && term.len() <= MAX_TERM_CHARACTERS
        && term.bytes().all(|b| b.is_ascii_alphabetic())
}

// The dictionary can contain differently cased ASCII words.  Leave ASCII
// matching to the original literal and Emacs's case-fold-search setting.
struct LiteralPreservingMigemoGenerator {
    literal: Vec<char>,
    inner: TernaryRegexGenerator,
}

impl RegexGeneratorTrait for LiteralPreservingMigemoGenerator {
    fn add(&mut self, word: &[char]) {
        if word == self.literal || !word.iter().all(char::is_ascii) {
            self.inner.add(word);
        }
    }

    fn generate(&self, operator: &RegexOperator) -> String {
        self.inner.generate(operator)
    }
}

pub fn migemo_regexp(term: &str) -> Option<String> {
    if !roman_term(term) {
        return None;
    }
    let dictionary = MIGEMO.get_or_init(|| {
        CompactDictionary::new(&include_bytes!("../data/migemo-compact-dict").to_vec())
    });
    let mut generator = LiteralPreservingMigemoGenerator {
        literal: term.chars().collect(),
        inner: TernaryRegexGenerator::new(),
    };
    let result = query_a_word_with_generator(
        term,
        dictionary,
        &RegexOperator::EmacsNonNewline,
        &mut generator,
    );
    (result.len() <= MAX_REGEXP_BYTES).then_some(result)
}

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
            if segment.starts_with(syllable) {
                let mut end = start + syllable.len();
                if end < term.len() && term.as_bytes()[end] == b'\'' {
                    end += 1;
                }
                if let Some(suffix) = &suffixes[end] {
                    alternatives.push(format!("[{characters}]{suffix}"));
                }
            } else if final_segment && syllable.starts_with(segment) {
                partial.extend(characters.chars());
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
            let first =
                char::from_u32(0xac00 + index as u32 * HANGUL_SYLLABLES_PER_INITIAL).unwrap();
            let last = char::from_u32(first as u32 + HANGUL_SYLLABLES_PER_INITIAL - 1).unwrap();
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
    fn migemo_is_cached_and_rejects_invalid_terms() {
        assert!(migemo_regexp("kensaku").unwrap().contains("検索"));
        let first = MIGEMO.get().unwrap() as *const _;
        assert!(migemo_regexp("nihon").is_some());
        assert_eq!(first, MIGEMO.get().unwrap() as *const _);
        assert!(migemo_regexp("a.*").is_none());
        assert!(migemo_regexp("").is_none());
        assert!(migemo_regexp(&"a".repeat(33)).is_none());
    }
    #[test]
    fn migemo_preserves_literal_case_and_japanese_lookup() {
        let upper = migemo_regexp("KENSAKU").unwrap();
        assert!(upper.contains("KENSAKU"));
        assert!(upper.contains("検索"));
        assert!(!upper.contains("kensaku"));
        let mixed = migemo_regexp("KenSaku").unwrap();
        assert!(mixed.contains("KenSaku"));
        assert!(mixed.contains("検索"));

        let mut generator = LiteralPreservingMigemoGenerator {
            literal: "FOO".chars().collect(),
            inner: TernaryRegexGenerator::new(),
        };
        for word in ["FOO", "foo", "ＦＯＯ", "日本"] {
            generator.add(&word.chars().collect::<Vec<_>>());
        }
        let result = generator.generate(&RegexOperator::EmacsNonNewline);
        assert!(result.contains("FOO"));
        assert!(!result.contains("foo"));
        assert!(result.contains("ＦＯＯ") && result.contains("日本"));
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
