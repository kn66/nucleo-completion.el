//! Language-specific expansions used by the completion regexp pipeline.
use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::sync::OnceLock;

use crate::hangul::HangulMatcher;
use crate::language_filename::LanguageFilenameMatcher;
use crate::migemo::MigemoMatcher;
use pinyin::{Pinyin, ToPinyinMulti};

pub(crate) const MAX_TERM_CHARACTERS: usize = 32;
// Bound ambiguous pinyin suffix expansion and broad dictionary queries before
// passing their results back to Emacs.  Lisp also rejects invalid regexps.
pub(crate) const MAX_REGEXP_BYTES: usize = 100_000;
const CJK_IDEOGRAPH_START: u32 = 0x3400;
const CJK_IDEOGRAPH_END: u32 = 0x323af;
const TONE_MARKS: &[u8] = b"!@#$";
static PINYIN: OnceLock<BTreeMap<String, String>> = OnceLock::new();
static PINYIN_TONES: OnceLock<BTreeMap<String, String>> = OnceLock::new();

fn pinyin_key(reading: Pinyin, with_tone: bool) -> Cow<'static, str> {
    let value = if with_tone {
        reading.with_tone_num_end()
    } else {
        reading.plain()
    };
    let mut key = if value.is_ascii() {
        Cow::Borrowed(value)
    } else {
        Cow::Owned(value.replace('ü', "v").replace('ê', "E"))
    };
    if with_tone && !value.as_bytes().last().is_some_and(u8::is_ascii_digit) {
        key.to_mut().push('5');
    }
    key
}

// Index all readings, including heteronyms, once; each value is safe inside an
// Emacs bracket expression because the indexed characters are CJK ideographs.
fn pinyin_index(with_tone: bool) -> BTreeMap<String, String> {
    let mut readings: BTreeMap<String, BTreeSet<char>> = BTreeMap::new();
    // U+3007 has readings in the pinyin data and precedes the ideograph blocks.
    for code in std::iter::once('〇' as u32).chain(CJK_IDEOGRAPH_START..=CJK_IDEOGRAPH_END) {
        if let Some(character) = char::from_u32(code) {
            if let Some(pronunciations) = character.to_pinyin_multi() {
                for pronunciation in pronunciations {
                    readings
                        .entry(pinyin_key(pronunciation, with_tone).into_owned())
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

fn has_tone(term: &str) -> bool {
    term.bytes()
        .any(|byte| byte.is_ascii_digit() || TONE_MARKS.contains(&byte))
}

fn indexed_pinyin(
    with_tone: bool,
) -> impl Iterator<Item = (&'static String, &'static String)> + Clone {
    let plain = PINYIN.get_or_init(|| pinyin_index(false));
    let toned = with_tone.then(|| PINYIN_TONES.get_or_init(|| pinyin_index(true)));
    plain.iter().chain(toned.into_iter().flatten())
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
        let marked = match character {
            'ā' | 'Ā' => Some(('a', '!')),
            'á' | 'Á' => Some(('a', '@')),
            'ǎ' | 'Ǎ' => Some(('a', '#')),
            'à' | 'À' => Some(('a', '$')),
            'ē' | 'Ē' => Some(('e', '!')),
            'é' | 'É' => Some(('e', '@')),
            'ě' | 'Ě' => Some(('e', '#')),
            'è' | 'È' => Some(('e', '$')),
            'ī' | 'Ī' => Some(('i', '!')),
            'í' | 'Í' => Some(('i', '@')),
            'ǐ' | 'Ǐ' => Some(('i', '#')),
            'ì' | 'Ì' => Some(('i', '$')),
            'ō' | 'Ō' => Some(('o', '!')),
            'ó' | 'Ó' => Some(('o', '@')),
            'ǒ' | 'Ǒ' => Some(('o', '#')),
            'ò' | 'Ò' => Some(('o', '$')),
            'ū' | 'Ū' => Some(('u', '!')),
            'ú' | 'Ú' => Some(('u', '@')),
            'ǔ' | 'Ǔ' => Some(('u', '#')),
            'ù' | 'Ù' => Some(('u', '$')),
            'ǖ' | 'Ǖ' => Some(('v', '!')),
            'ǘ' | 'Ǘ' => Some(('v', '@')),
            'ǚ' | 'Ǚ' => Some(('v', '#')),
            'ǜ' | 'Ǜ' => Some(('v', '$')),
            'ế' | 'Ế' => Some(('E', '@')),
            'ề' | 'Ề' => Some(('E', '$')),
            'ḿ' | 'Ḿ' => Some(('m', '@')),
            'ń' | 'Ń' => Some(('n', '@')),
            'ň' | 'Ň' => Some(('n', '#')),
            'ǹ' | 'Ǹ' => Some(('n', '$')),
            _ => None,
        };
        if let Some((vowel, tone)) = marked {
            normalized.push(vowel);
            normalized.push(tone);
            continue;
        }
        match character {
            'ü' | 'Ü' => normalized.push('v'),
            'ê' | 'Ê' => normalized.push('E'),
            'u' | 'U' if chars.peek() == Some(&':') => {
                chars.next();
                normalized.push('v');
            }
            // Keep the accent on its vowel in either combining order.
            '\u{308}' | '\u{302}' => {
                let (base, replacement) = if character == '\u{308}' {
                    (b'u', "v")
                } else {
                    (b'e', "E")
                };
                let marked = normalized
                    .as_bytes()
                    .last()
                    .is_some_and(|byte| TONE_MARKS.contains(byte));
                let index = normalized.len().checked_sub(1 + usize::from(marked))?;
                if normalized.as_bytes()[index] != base {
                    return None;
                }
                normalized.replace_range(index..=index, replacement);
            }
            '\u{304}' | '\u{301}' | '\u{30c}' | '\u{300}'
                if normalized.ends_with(['a', 'e', 'i', 'o', 'u', 'v', 'E', 'm', 'n']) =>
            {
                normalized.push(match character {
                    '\u{304}' => '!',
                    '\u{301}' => '@',
                    '\u{30c}' => '#',
                    _ => '$',
                });
            }
            c if c.is_ascii_alphabetic() => normalized.push(c.to_ascii_lowercase()),
            '1'..='5' => normalized.push(character),
            '\'' if !normalized.is_empty() && !normalized.ends_with('\'') => normalized.push('\''),
            _ => return None,
        }
    }
    if normalized.ends_with('\'') {
        return None;
    }
    Some(normalized)
}

pub fn pinyin_regexp(term: &str) -> Option<String> {
    let term = normalize_pinyin(term)?;
    expand_pinyin_suffixes(&term, indexed_pinyin(has_tone(&term)))
}

enum PinyinStep {
    Complete(usize),
    Partial,
}

fn pinyin_step(reading: &str, segment: &str, final_segment: bool) -> Option<PinyinStep> {
    let reading_bytes = reading.as_bytes();
    let Some(tone) = reading_bytes.last().copied().filter(u8::is_ascii_digit) else {
        return if segment.starts_with(reading) {
            let separator = usize::from(!final_segment && reading.len() == segment.len());
            Some(PinyinStep::Complete(reading.len() + separator))
        } else if final_segment && reading.starts_with(segment) {
            Some(PinyinStep::Partial)
        } else {
            None
        };
    };
    let syllable = &reading_bytes[..reading_bytes.len() - 1];
    let input = segment.as_bytes();
    let mut pos = 0;
    let mut marked_tone = None;
    for &letter in syllable {
        if pos == input.len() {
            return None;
        }
        if input[pos] != letter {
            return None;
        }
        pos += 1;
        if pos < input.len() {
            if let Some(mark) = TONE_MARKS.iter().position(|&c| c == input[pos]) {
                if marked_tone.replace(b'1' + mark as u8).is_some() {
                    return None;
                }
                pos += 1;
            }
        }
    }
    if pos < input.len() && input[pos].is_ascii_digit() {
        if marked_tone.is_some() {
            return None;
        }
        marked_tone = Some(input[pos]);
        pos += 1;
    }
    if marked_tone != Some(tone) {
        return None;
    }
    // An explicit tone closes the syllable; remaining letters start another.
    let separator = usize::from(!final_segment && pos == input.len());
    Some(PinyinStep::Complete(pos + separator))
}

pub enum LanguageMatcher {
    Migemo(MigemoMatcher),
    Pinyin(PinyinMatcher),
    Hangul(HangulMatcher),
    Filename(LanguageFilenameMatcher),
}

impl emacs::Transfer for LanguageMatcher {}

impl LanguageMatcher {
    pub fn has_filename_priority(&self) -> bool {
        match self {
            Self::Migemo(matcher) => matcher.has_filename_priority(),
            Self::Filename(matcher) => matcher.has_filename_priority(),
            _ => false,
        }
    }

    pub fn match_quality(&self, text: &str, ignore_case: bool) -> Option<u8> {
        match self {
            Self::Migemo(matcher) => matcher.match_quality(text, ignore_case),
            Self::Filename(matcher) if matcher.has_filename_priority() => {
                matcher.match_quality(text, ignore_case)
            }
            _ => self.find(text, 0, ignore_case).map(|_| 0),
        }
    }

    pub fn find(&self, text: &str, start: usize, ignore_case: bool) -> Option<(usize, usize)> {
        match self {
            Self::Migemo(matcher) => matcher.find(text, start, ignore_case),
            Self::Pinyin(matcher) => matcher.find(text, start),
            Self::Hangul(matcher) => matcher.find(text, start, ignore_case),
            Self::Filename(matcher) => matcher.find(text, start, ignore_case),
        }
    }
}

pub struct PinyinMatcher {
    transitions: HashMap<char, Vec<(u128, u128)>>,
    terminal: u128,
}

impl PinyinMatcher {
    pub fn new(term: &str) -> Option<Self> {
        let term = normalize_pinyin(term)?;
        // Each bit is a byte position in the normalized query. Tone marks may
        // each add a byte, so 32 input characters can require up to 65 states.
        // Sharing suffix states avoids duplicating them for every syllable split.
        let terminal = 1 << term.len();
        let mut reachable = terminal;
        let mut transitions = HashMap::<char, Vec<_>>::new();
        let readings = indexed_pinyin(has_tone(&term));
        for start in (0..term.len()).rev() {
            if term.as_bytes()[start] == b'\'' {
                continue;
            }
            let remaining = &term[start..];
            let segment = remaining.split('\'').next().unwrap();
            for (syllable, characters) in readings.clone() {
                let end = match pinyin_step(syllable, segment, segment == remaining) {
                    Some(PinyinStep::Complete(length)) => start + length,
                    Some(PinyinStep::Partial) => term.len(),
                    None => continue,
                };
                let from = 1 << start;
                let to = 1 << end;
                if reachable & to != 0 {
                    // Resolve readings while compiling the term, so scanning
                    // candidates only needs one lookup per Han character.
                    for character in characters.chars() {
                        let edges = transitions.entry(character).or_default();
                        if !edges.contains(&(from, to)) {
                            edges.push((from, to));
                        }
                    }
                    reachable |= from;
                }
            }
        }
        (reachable & 1 != 0).then_some(Self {
            transitions,
            terminal,
        })
    }

    /// Return every matching prefix end, measured in characters.
    pub fn prefix_ends<'a>(&'a self, text: &'a str) -> impl Iterator<Item = usize> + 'a {
        let mut states = 1;
        let mut characters = text.chars().enumerate();
        std::iter::from_fn(move || {
            while states != 0 && states != self.terminal {
                let (index, character) = characters.next()?;
                states = self.advance(states, character);
                if states & self.terminal != 0 {
                    return Some(index + 1);
                }
            }
            None
        })
    }

    pub fn find(&self, text: &str, start: usize) -> Option<(usize, usize)> {
        for (offset, (byte_offset, _)) in text.char_indices().enumerate().skip(start) {
            if let Some(end) = self.prefix_ends(&text[byte_offset..]).last() {
                return Some((offset, offset + end));
            }
        }
        None
    }

    fn advance(&self, states: u128, character: char) -> u128 {
        if character != '〇'
            && !(CJK_IDEOGRAPH_START..=CJK_IDEOGRAPH_END).contains(&(character as u32))
        {
            return 0;
        }
        let mut next = 0;
        if let Some(edges) = self.transitions.get(&character) {
            for &(from, to) in edges {
                if states & from != 0 {
                    next |= to;
                }
            }
        }
        next
    }
}

// Normalization guarantees ASCII, so syllable and apostrophe positions are
// valid UTF-8 byte boundaries. Work backwards so alternatives with the same
// suffix share its computation.
fn expand_pinyin_suffixes<'a>(
    term: &str,
    index: impl Iterator<Item = (&'a String, &'a String)> + Clone,
) -> Option<String> {
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
        for (syllable, characters) in index.clone() {
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
    Some(HangulMatcher::new(term)?.regexp())
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
        assert_eq!(
            PinyinMatcher::new("ling").unwrap().find("〇", 0),
            Some((0, 1))
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
        assert_eq!(
            PinyinMatcher::new("zhong1guo2").unwrap().find("中国", 0),
            Some((0, 2))
        );
    }

    #[test]
    fn pinyin_tones_require_a_complete_matching_syllable() {
        for term in [
            "zhōngguó",
            "zho\u{304}ngguo\u{301}",
            "zhong1guo2",
            "ZHONG1'GUO2",
        ] {
            let matcher = PinyinMatcher::new(term).unwrap();
            assert_eq!(matcher.find("😀中国", 0), Some((1, 3)), "{term}");
            assert!(pinyin_regexp(term).unwrap().contains('国'), "{term}");
        }
        for term in ["lv4", "lǜ", "lu\u{308}\u{300}", "lu\u{300}\u{308}"] {
            assert_eq!(
                PinyinMatcher::new(term).unwrap().find("绿", 0),
                Some((0, 1)),
                "{term}"
            );
            assert!(pinyin_regexp(term).unwrap().contains('绿'), "{term}");
        }
        assert_eq!(PinyinMatcher::new("ma1").unwrap().find("马", 0), None);
        assert!(!pinyin_regexp("ma1").unwrap().contains('马'));
        assert_eq!(
            PinyinMatcher::new("ma5").unwrap().find("吗", 0),
            Some((0, 1))
        );
        assert!(pinyin_regexp("ma5").unwrap().contains('吗'));
        for (term, candidate) in [("ế", "欸"), ("e\u{302}\u{301}", "欸"), ("ń", "嗯")] {
            assert_eq!(
                PinyinMatcher::new(term).unwrap().find(candidate, 0),
                Some((0, 1)),
                "{term}"
            );
            assert!(pinyin_regexp(term).unwrap().contains(candidate));
        }
        assert_eq!(
            PinyinMatcher::new("zhong1g").unwrap().find("中国", 0),
            Some((0, 2))
        );
        for invalid in [
            "zhong11",
            "zhong0",
            "zhōng1",
            "zho1ng",
            "mā\u{300}",
            "zhong 1",
        ] {
            assert!(PinyinMatcher::new(invalid).is_none(), "{invalid}");
            assert!(pinyin_regexp(invalid).is_none(), "{invalid}");
        }
        assert_eq!(
            PinyinMatcher::new("zhongguo")
                .unwrap()
                .prefix_ends("中国.txt")
                .collect::<Vec<_>>(),
            vec![2]
        );
    }

    #[test]
    fn pinyin_suffix_expansion_respects_boundaries_and_size_limit() {
        let index = BTreeMap::from([
            ("ab".to_owned(), "甲".to_owned()),
            ("b".to_owned(), "乙".to_owned()),
        ]);
        // A prefix may stand in for an unfinished final syllable, but not one
        // followed by an explicit syllable boundary.
        assert!(expand_pinyin_suffixes("a", index.iter()).is_some());
        assert!(expand_pinyin_suffixes("a'b", index.iter()).is_none());
        assert!(expand_pinyin_suffixes("ab'b", index.iter()).is_some());

        let oversized = BTreeMap::from([("a".to_owned(), "甲".repeat(MAX_REGEXP_BYTES))]);
        assert!(expand_pinyin_suffixes("a", oversized.iter()).is_none());
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
        for invalid in ["'xi", "xi'", "xi''an", "x:i", "ü😀", "u::"] {
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
        assert_eq!(
            hangul_regexp("ㄱㅅ").unwrap(),
            "\\(?:ᄀ[ᅡ-ᅵ][ᆨ-ᇂ]?\\|[ㄱ가-깋][ᆨ-ᇂ]?\\)\\(?:ᄉ[ᅡ-ᅵ][ᆨ-ᇂ]?\\|[ㅅ사-싷][ᆨ-ᇂ]?\\)"
        );
        assert_eq!(
            hangul_regexp("가ᄂ").unwrap(),
            "\\(?:가\\|가\\)\\(?:ᄂ[ᅡ-ᅵ][ᆨ-ᇂ]?\\|[ᄂ나-닣][ᆨ-ᇂ]?\\)"
        );
        assert_eq!(hangul_regexp("감사"), hangul_regexp("감사"));
        assert!(hangul_regexp("abc").is_none());
        assert!(hangul_regexp("ㄱ*").is_none());
        assert!(hangul_regexp("").is_none());
        assert!(hangul_regexp(&"ㄱ".repeat(33)).is_none());
    }
}
