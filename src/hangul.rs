//! Direct matching for modern Hangul initial-consonant queries.

use std::str::Chars;

use crate::languages::MAX_TERM_CHARACTERS;

// Unicode orders modern syllables by initial, then 21 vowels and 28 finals.
pub(crate) const SYLLABLES_PER_INITIAL: u32 = 21 * 28;
const INITIALS: &str = "ㄱㄲㄴㄷㄸㄹㅁㅂㅃㅅㅆㅇㅈㅉㅊㅋㅌㅍㅎ";

enum QueryPart {
    Initial { index: u32, literal: char },
    Syllable(u32),
    Ascii(char),
}

pub struct HangulMatcher {
    parts: Vec<QueryPart>,
}

// Return the composed syllable and its length in the original text.
fn syllable_at(mut chars: Chars<'_>) -> Option<(u32, usize)> {
    let code = chars.next()? as u32;
    if (0xac00..=0xd7a3).contains(&code) {
        if (code - 0xac00) % 28 == 0 {
            // An LV syllable and a separate trailing jamo can also compose.
            if let Some(trailing) = trailing_jamo(chars.next()) {
                return Some((code + trailing, 2));
            }
        }
        return Some((code, 1));
    }
    if !(0x1100..=0x1112).contains(&code) {
        return None;
    }
    let vowel = chars.next()? as u32;
    if !(0x1161..=0x1175).contains(&vowel) {
        return None;
    }
    let trailing = trailing_jamo(chars.next());
    let syllable = 0xac00
        + (code - 0x1100) * SYLLABLES_PER_INITIAL
        + (vowel - 0x1161) * 28
        + trailing.unwrap_or(0);
    Some((syllable, if trailing.is_some() { 3 } else { 2 }))
}

fn trailing_jamo(character: Option<char>) -> Option<u32> {
    let code = character? as u32;
    (0x11a8..=0x11c2).contains(&code).then(|| code - 0x11a7)
}

impl HangulMatcher {
    pub fn new(term: &str) -> Option<Self> {
        if term.chars().count() > MAX_TERM_CHARACTERS {
            return None;
        }
        let mut parts = Vec::new();
        let mut has_initial = false;
        let mut chars = term.chars();
        while let Some(character) = chars.clone().next() {
            if let Some((syllable, length)) = syllable_at(chars.clone()) {
                parts.push(QueryPart::Syllable(syllable));
                chars.nth(length - 1);
            } else if let Some(index) =
                INITIALS.chars().position(|c| c == character).or_else(|| {
                    let code = character as u32;
                    (0x1100..=0x1112)
                        .contains(&code)
                        .then(|| (code - 0x1100) as usize)
                })
            {
                has_initial = true;
                parts.push(QueryPart::Initial {
                    index: index as u32,
                    literal: character,
                });
                chars.next();
            } else if character.is_ascii_alphanumeric() {
                parts.push(QueryPart::Ascii(character));
                chars.next();
            } else {
                return None;
            }
        }
        has_initial.then_some(Self { parts })
    }

    /// Find the first match at or after START, using character offsets.
    pub fn find(&self, text: &str, start: usize, ignore_case: bool) -> Option<(usize, usize)> {
        let mut suffix = text.chars();
        if start > 0 {
            suffix.nth(start - 1)?;
        }
        let mut begin = start;
        while !suffix.as_str().is_empty() {
            if let Some(length) = self.match_prefix(suffix.clone(), ignore_case) {
                return Some((begin, begin + length));
            }
            // Skip the whole canonical syllable, including any final jamo.
            let length = syllable_at(suffix.clone()).map_or(1, |(_, length)| length);
            suffix.nth(length - 1);
            begin += length;
        }
        None
    }

    fn match_prefix(&self, mut chars: Chars<'_>, ignore_case: bool) -> Option<usize> {
        let mut total = 0;
        for part in &self.parts {
            let length = match *part {
                QueryPart::Initial { index, literal } => {
                    if let Some((syllable, length)) = syllable_at(chars.clone()) {
                        ((syllable - 0xac00) / SYLLABLES_PER_INITIAL == index).then_some(length)
                    } else {
                        (chars.clone().next()? == literal).then_some(1)
                    }
                }
                QueryPart::Syllable(expected) => {
                    let (actual, length) = syllable_at(chars.clone())?;
                    (actual == expected).then_some(length)
                }
                QueryPart::Ascii(expected) => {
                    let actual = chars.clone().next()?;
                    (actual == expected || (ignore_case && actual.eq_ignore_ascii_case(&expected)))
                        .then_some(1)
                }
            }?;
            chars.nth(length - 1);
            total += length;
        }
        Some(total)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn matches_initials_and_entire_decomposed_syllables() {
        let matcher = HangulMatcher::new("ㄱㅅ").unwrap();
        assert_eq!(matcher.find("x각사 각사", 0, false), Some((1, 3)));
        assert_eq!(matcher.find("x각사 각사", 3, false), Some((4, 9)));
        assert_eq!(matcher.find("각사", 1, false), None);
        assert_eq!(matcher.find("ㄱㅅ", 0, false), Some((0, 2)));
        assert_eq!(matcher.find("ᄀᄉ", 0, false), None);
        assert_eq!(
            HangulMatcher::new("ᄀᄉ").unwrap().find("ᄀᄉ", 0, false),
            Some((0, 2))
        );
    }

    #[test]
    fn literal_syllables_compare_in_both_normalizations() {
        for query in ["ㄱ가", "ㄱ가"] {
            let matcher = HangulMatcher::new(query).unwrap();
            assert_eq!(matcher.find("가가", 0, false), Some((0, 2)));
            assert_eq!(matcher.find("가가", 0, false), Some((0, 4)));
            assert_eq!(matcher.find("가각", 0, false), None);
            assert_eq!(matcher.find("가각", 0, false), None);
        }
        let matcher = HangulMatcher::new("각ㄴ").unwrap();
        assert_eq!(matcher.find("각나", 0, false), Some((0, 4)));
        assert_eq!(matcher.find("각나", 0, false), Some((0, 3)));
        assert_eq!(
            HangulMatcher::new("각ㄴ").unwrap().find("각나", 0, false),
            Some((0, 2))
        );
    }

    #[test]
    fn restrictions_and_ascii_case() {
        for query in ["", "가나", "가나", "ㄱ*", "ㄱᅡᆨ", "ᅡㄱ"] {
            assert!(HangulMatcher::new(query).is_none(), "{query}");
        }
        assert!(HangulMatcher::new(&"ㄱ".repeat(33)).is_none());
        let matcher = HangulMatcher::new("ㄱAb").unwrap();
        assert_eq!(matcher.find("가ab", 0, false), None);
        assert_eq!(matcher.find("가ab", 0, true), Some((0, 3)));
        assert_eq!(matcher.find("가Ab", 20, false), None);
    }
}
