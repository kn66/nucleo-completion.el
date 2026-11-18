//! Japanese reading expansion shared by native matching and the regexp API.
use std::collections::{BTreeSet, HashMap};
use std::sync::OnceLock;

use rustmigemo::migemo::{
    compact_dictionary::CompactDictionary,
    compact_dictionary_builder,
    query::query_a_word_with_generator,
    regex_generator::{RegexGeneratorTrait, RegexOperator},
    ternary_regex_generator::TernaryRegexGenerator,
};

use crate::languages::{MAX_REGEXP_BYTES, MAX_TERM_CHARACTERS};

static BUNDLED: OnceLock<CompactDictionary> = OnceLock::new();
static UNIDIC: OnceLock<CompactDictionary> = OnceLock::new();
static ABBREVIATIONS: OnceLock<CompactDictionary> = OnceLock::new();

pub struct MigemoDictionary(pub CompactDictionary);

impl emacs::Transfer for MigemoDictionary {}

fn valid_reading(term: &str) -> bool {
    let mut bytes = term.bytes();
    if !bytes.next().is_some_and(|byte| byte.is_ascii_alphabetic()) {
        return false;
    }
    term.bytes().zip(bytes).all(|(previous, byte)| {
        byte.is_ascii_alphabetic()
            || byte == b'-'
            || (byte == b'\'' && previous.eq_ignore_ascii_case(&b'n'))
    })
}

struct MigemoTerm<'a> {
    literal: &'a str,
    leading: &'a str,
    reading: &'a str,
    attached: &'a str,
    extension: &'a str,
}

impl<'a> MigemoTerm<'a> {
    fn parse(term: &'a str) -> Option<Self> {
        if term.len() > MAX_TERM_CHARACTERS {
            return None;
        }
        // A leading number (and optional separator) belongs to the filename,
        // not the romanized reading. Keep it adjacent to each expansion.
        let digits = term.bytes().take_while(u8::is_ascii_digit).count();
        let leading_end = if digits > 0 && matches!(term.as_bytes().get(digits), Some(b'-' | b'_'))
        {
            digits + 1
        } else {
            digits
        };
        let (leading, rest) = term.split_at(leading_end);
        let suffix_start = rest
            .find(|c: char| c.is_ascii_digit() || c == '.' || c == '_')
            .unwrap_or(rest.len());
        let (reading, suffix) = rest.split_at(suffix_start);
        if !valid_reading(reading)
            || !suffix
                .bytes()
                .all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'.' | b'_' | b'-'))
        {
            return None;
        }
        // A leading dot permits a longer filename stem. Other attached
        // characters, including digits and separators, stay adjacent.
        let extension_start = suffix.find('.').unwrap_or(suffix.len());
        let (attached, extension) = suffix.split_at(extension_start);
        Some(Self {
            literal: &term[..term.len() - extension.len()],
            leading,
            reading,
            attached,
            extension,
        })
    }

    fn readings(&self) -> impl Iterator<Item = (&'a str, &'a str)> {
        // A hyphen before digits can be a long vowel or a filename separator.
        let separated = if self
            .attached
            .as_bytes()
            .first()
            .is_some_and(u8::is_ascii_digit)
        {
            self.reading
                .strip_suffix('-')
                .map(|reading| (reading, &self.literal[self.leading.len() + reading.len()..]))
        } else {
            None
        };
        std::iter::once((self.reading, self.attached)).chain(separated)
    }
}

struct MigemoWords<'a> {
    leading: &'a str,
    reading: &'a str,
    attached: &'a str,
    words: BTreeSet<String>,
}

impl RegexGeneratorTrait for MigemoWords<'_> {
    fn add(&mut self, word: &[char]) {
        if !word.iter().all(char::is_ascii) || word.iter().copied().eq(self.reading.chars()) {
            // When a number is attached, preserve both 会議2026 and 会議室2026.
            let mut spelling = self.leading.to_owned();
            spelling.extend(word);
            spelling.push_str(self.attached);
            self.words.insert(spelling);
        }
    }

    fn generate(&self, _: &RegexOperator) -> String {
        String::new()
    }
}

fn expand(term: &MigemoTerm<'_>, extra: &[&MigemoDictionary]) -> Vec<String> {
    let bundled = BUNDLED.get_or_init(|| {
        CompactDictionary::new(&include_bytes!("../data/migemo-compact-dict").to_vec())
    });
    let unidic = UNIDIC.get_or_init(|| {
        CompactDictionary::new(&include_bytes!("../data/migemo-unidic-extra-dict").to_vec())
    });
    let abbreviations = ABBREVIATIONS.get_or_init(|| {
        // The abbreviated candidate can itself be the entire completion text.
        let entries = HashMap::from([("けいじ".to_owned(), vec!["軽自".to_owned()])]);
        CompactDictionary::new(&compact_dictionary_builder::build(entries))
    });
    let mut collector = MigemoWords {
        leading: term.leading,
        reading: term.reading,
        attached: term.attached,
        words: BTreeSet::new(),
    };
    let dictionaries = [bundled, unidic, abbreviations]
        .into_iter()
        .chain(extra.iter().map(|dictionary| &dictionary.0));
    for (reading, attached) in term.readings() {
        collector.reading = reading;
        collector.attached = attached;
        for dictionary in dictionaries.clone() {
            query_a_word_with_generator(
                reading,
                dictionary,
                &RegexOperator::EmacsNonNewline,
                &mut collector,
            );
        }
    }
    // Shorter complete alternatives already match longer ones at the same
    // position; retaining them keeps kana prefixes such as パート visible.
    let mut prefixes: Vec<String> = Vec::new();
    for word in collector.words {
        if prefixes
            .last()
            .is_none_or(|prefix| !word.starts_with(prefix))
        {
            prefixes.push(word);
        }
    }
    prefixes
}

pub fn migemo_regexp(term: &str) -> Option<String> {
    let term = MigemoTerm::parse(term)?;
    let mut generator = TernaryRegexGenerator::new();
    let mut literal_alternatives = Vec::new();
    for word in expand(&term, &[]) {
        if word.contains([
            '(', ')', '{', '}', '|', '^', '$', '.', '*', '+', '?', '[', ']', '\\', '-',
        ]) {
            // Emacs escapes a different set of characters from the upstream
            // generator. Keep punctuation out of its character classes.
            let mut quoted = String::new();
            for character in word.chars() {
                if "$^.*+?[\\]".contains(character) {
                    quoted.push('\\');
                }
                quoted.push(character);
            }
            literal_alternatives.push(quoted);
        } else {
            generator.add(&word.chars().collect::<Vec<_>>());
        }
    }
    let generated = generator.generate(&RegexOperator::EmacsNonNewline);
    if !generated.is_empty() {
        literal_alternatives.push(generated);
    }
    let mut result = match literal_alternatives.as_slice() {
        [only] => only.clone(),
        _ => format!("\\(?:{}\\)", literal_alternatives.join("\\|")),
    };
    if !term.extension.is_empty() {
        // Match only within this filename component, and stop at the first
        // suffix so legacy regexp highlighting agrees with the native path.
        if term.attached.is_empty() {
            result.push_str("[^/\\\\\n\r]*?");
        }
        for character in term.extension.chars() {
            if character == '.' {
                result.push('\\');
            }
            result.push(character);
        }
    }
    (result.len() <= MAX_REGEXP_BYTES).then_some(result)
}

pub struct MigemoMatcher {
    literal: String,
    extension: String,
    allow_extension_gap: bool,
    words: HashMap<char, Vec<String>>,
}

impl MigemoMatcher {
    pub fn new(term: &str, extra: &[&MigemoDictionary]) -> Option<Self> {
        let parsed = MigemoTerm::parse(term)?;
        let mut by_initial = HashMap::<_, Vec<_>>::new();
        for word in expand(&parsed, extra) {
            if let Some(initial) = word.chars().next().filter(|_| word != parsed.literal) {
                by_initial
                    .entry(initial.to_lowercase().next().unwrap())
                    .or_default()
                    .push(word);
            }
        }
        Some(Self {
            literal: parsed.literal.to_owned(),
            extension: parsed.extension.to_owned(),
            allow_extension_gap: parsed.attached.is_empty(),
            words: by_initial,
        })
    }

    /// Whether dotted filename queries can use the optional relevance tier.
    pub fn has_filename_priority(&self) -> bool {
        !self.extension.is_empty()
    }

    /// Return the best filename tier for any matching span, or None on a miss.
    pub fn match_quality(&self, text: &str, ignore_case: bool) -> Option<u8> {
        if !self.has_filename_priority() {
            return self.find(text, 0, ignore_case).map(|_| 0);
        }
        let mut best = None;
        let literal_initial = self.literal.as_bytes()[0].to_ascii_lowercase() as char;
        for (offset, first) in text.char_indices() {
            let initial = first.to_lowercase().next().unwrap();
            let tail = &text[offset..];
            let at_component_start =
                offset == 0 || text[..offset].ends_with(['/', '\\', '\n', '\r']);
            let candidates = self
                .words
                .get(&initial)
                .into_iter()
                .flatten()
                .map(String::as_str)
                .chain((initial == literal_initial).then_some(self.literal.as_str()));
            for stem in candidates {
                if let Some(quality) =
                    self.filename_quality(tail, stem, ignore_case, at_component_start)
                {
                    best = Some(best.map_or(quality, |value: u8| value.max(quality)));
                    if best == Some(3) {
                        return best;
                    }
                }
            }
        }
        best
    }

    pub fn find(&self, text: &str, start: usize, ignore_case: bool) -> Option<(usize, usize)> {
        let literal_initial = self.literal.as_bytes()[0].to_ascii_lowercase() as char;
        for (begin, (offset, first)) in text.char_indices().enumerate().skip(start) {
            let suffix = &text[offset..];
            let initial = first.to_lowercase().next().unwrap();
            if initial == literal_initial {
                if let Some(length) = self.match_filename(suffix, &self.literal, ignore_case) {
                    return Some((begin, begin + length));
                }
            }
            if let Some(length) = self.words.get(&initial).and_then(|words| {
                words
                    .iter()
                    .find_map(|word| self.match_filename(suffix, word, ignore_case))
            }) {
                return Some((begin, begin + length));
            }
        }
        None
    }
    // A dotted suffix alone can follow extra characters in the same filename.
    // Return character offsets to match the module's highlighting API.
    fn match_filename(&self, text: &str, stem: &str, ignore_case: bool) -> Option<usize> {
        if !matches_prefix(text, stem, ignore_case) {
            return None;
        }
        let stem_len = stem.chars().count();
        let extension = &self.extension;
        if extension.is_empty() {
            return Some(stem_len);
        }
        let offset = text
            .char_indices()
            .nth(stem_len)
            .map_or(text.len(), |(offset, _)| offset);
        let tail = &text[offset..];
        for (count, (byte_offset, character)) in tail.char_indices().enumerate() {
            if matches_prefix(&tail[byte_offset..], extension, ignore_case) {
                return Some(stem_len + count + extension.len());
            }
            if !self.allow_extension_gap || matches!(character, '/' | '\\' | '\n' | '\r') {
                break;
            }
        }
        None
    }

    // Consider later suffixes in case the first occurrence is part of .txt.bak.
    fn filename_quality(
        &self,
        text: &str,
        stem: &str,
        ignore_case: bool,
        at_component_start: bool,
    ) -> Option<u8> {
        if !matches_prefix(text, stem, ignore_case) {
            return None;
        }
        let stem_len = stem.chars().count();
        let offset = text
            .char_indices()
            .nth(stem_len)
            .map_or(text.len(), |(offset, _)| offset);
        let tail = &text[offset..];
        let extension = &self.extension;
        let mut best = None;
        for (position, (byte_offset, character)) in tail.char_indices().enumerate() {
            let suffix = &tail[byte_offset..];
            if matches_prefix(suffix, extension, ignore_case) {
                let terminal = matches!(
                    suffix.chars().nth(extension.len()),
                    None | Some('/' | '\\' | '\n' | '\r')
                );
                let quality = match (position == 0, terminal) {
                    (true, true) if at_component_start => 3,
                    (_, true) => 2,
                    (true, false) => 1,
                    (false, false) => 0,
                };
                best = Some(best.map_or(quality, |value: u8| value.max(quality)));
            }
            if !self.allow_extension_gap || matches!(character, '/' | '\\' | '\n' | '\r') {
                break;
            }
        }
        best
    }
}

fn matches_prefix(text: &str, prefix: &str, ignore_case: bool) -> bool {
    if text.starts_with(prefix) {
        return true;
    }
    if !ignore_case {
        return false;
    }
    let mut text = text.chars();
    prefix.chars().all(|expected| {
        text.next()
            .is_some_and(|actual| actual.to_lowercase().eq(expected.to_lowercase()))
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn attached_separators_and_leading_numbers() {
        for (query, text) in [
            ("kaigi-2026", "会議-2026.org"),
            ("nihon_report", "日本_report.txt"),
            ("2026kaigi", "2026会議.org"),
            ("2026-kaigi", "2026-会議.org"),
            ("2026_kaigi", "2026_会議.org"),
            ("pa-2026", "パー2026"),
            ("ko-hi-2026", "コーヒー2026"),
        ] {
            assert!(
                MigemoMatcher::new(query, &[])
                    .unwrap()
                    .find(text, 0, false)
                    .is_some(),
                "{query}"
            );
            assert!(migemo_regexp(query).is_some(), "{query}");
        }
        assert!(MigemoMatcher::new("kaigi-2026", &[])
            .unwrap()
            .find("会議2026", 0, false)
            .is_none());
    }

    #[test]
    fn filename_priority_uses_best_matching_span() {
        let matcher = MigemoMatcher::new("nihon.txt", &[]).unwrap();
        assert!(matcher.has_filename_priority());
        for (text, expected) in [
            ("日本.txt", Some(3)),
            ("日本語.txt", Some(2)),
            ("新日本.txt", Some(2)),
            ("日本.txt.bak", Some(1)),
            ("日本語.txt.bak", Some(0)),
            ("日本.pdf", None),
            ("日本語.txt.bak/日本.txt", Some(3)),
            ("日本/語.txt", None),
        ] {
            assert_eq!(matcher.match_quality(text, false), expected, "{text}");
        }
        let compound = MigemoMatcher::new("kaigi2026.org", &[]).unwrap();
        assert!(compound.has_filename_priority());
        assert_eq!(compound.match_quality("会議2026.org", false), Some(3));
        let ordinary = MigemoMatcher::new("nihon", &[]).unwrap();
        assert!(!ordinary.has_filename_priority());
        assert_eq!(ordinary.match_quality("日本", false), Some(0));
        assert_eq!(ordinary.match_quality("英語", false), None);
        let case = MigemoMatcher::new("nihon.TXT", &[]).unwrap();
        assert_eq!(case.match_quality("日本.txt", false), None);
        assert_eq!(case.match_quality("日本.txt", true), Some(3));
        let folded_extension = MigemoMatcher::new("nihon.k", &[]).unwrap();
        assert_eq!(folded_extension.match_quality("日本.K", true), Some(3));
    }

    #[test]
    fn accepts_migemo_long_vowels_and_syllabic_n() {
        for (term, text) in [
            ("pa-to", "パートナー"),
            ("ko-hi-", "コーヒー"),
            ("kan'i", "簡易"),
        ] {
            assert!(MigemoMatcher::new(term, &[])
                .unwrap()
                .find(text, 0, false)
                .is_some());
        }
        for term in ["", "-pa", "a.*", "abc''", "abc'd", "検索"] {
            assert!(MigemoMatcher::new(term, &[]).is_none(), "{term}");
        }
    }

    #[test]
    fn bundled_readings_cover_mixed_spellings_and_the_abbreviation() {
        for (term, text) in [
            ("keiji", "軽自"),
            ("keiji", "軽自動車"),
            ("toiawase", "問い合わせ"),
            ("moushiko", "申し込み"),
        ] {
            assert!(MigemoMatcher::new(term, &[])
                .unwrap()
                .find(text, 0, false)
                .is_some());
        }
    }

    #[test]
    fn filename_suffixes_attach_to_expansions_and_remain_literal() {
        for (term, text, expected) in [
            ("kaigi2026", "会議2026.org", Some((0, 6))),
            ("nihon.txt", "日本.txt", Some((0, 6))),
            ("kaigi2026.org", "会議2026.org", Some((0, 10))),
            ("kaigi2026.org", "会議2026-old.org", None),
            ("kaigi2026", "会議-2026.org", None),
            ("nihon.tar.gz", "日本.tar.gz", Some((0, 9))),
            ("nihon.", "日本.txt", Some((0, 3))),
            ("nihon.txt", "日本Xtxt", None),
        ] {
            assert_eq!(
                MigemoMatcher::new(term, &[]).unwrap().find(text, 0, false),
                expected,
                "{term}"
            );
        }
        let matcher = MigemoMatcher::new("nihon.TXT", &[]).unwrap();
        assert_eq!(matcher.find("日本.txt", 0, false), None);
        assert_eq!(matcher.find("日本.txt", 0, true), Some((0, 6)));
        assert_eq!(matcher.find("NIHON.TXT", 0, false), None);
        assert_eq!(matcher.find("NIHON.TXT", 0, true), Some((0, 9)));
        assert!(migemo_regexp("nihon.txt").unwrap().contains("\\.txt"));
        for invalid in ["a.*", "nihon.txt*", "nihon/abc"] {
            assert!(MigemoMatcher::new(invalid, &[]).is_none(), "{invalid}");
            assert!(migemo_regexp(invalid).is_none(), "{invalid}");
        }
        assert!(MigemoMatcher::new(&format!("a.{}", "x".repeat(31)), &[]).is_none());
    }

    #[test]
    fn suffix_follows_a_stem_prefix_without_crossing_path_components() {
        let matcher = MigemoMatcher::new("pa-to.txt", &[]).unwrap();
        for (text, expected) in [
            ("パート.txt", Some((0, 7))),
            ("パートナー.txt", Some((0, 9))),
            ("😀パートナー.txt", Some((1, 10))),
            ("パートナー.pdf", None),
            ("パート/ナー.txt", None),
            ("パート\\ナー.txt", None),
            ("パート\nナー.txt", None),
        ] {
            assert_eq!(matcher.find(text, 0, false), expected, "{text}");
        }
        assert_eq!(
            MigemoMatcher::new("pa-to.tx", &[])
                .unwrap()
                .find("パートナー.txt", 0, false),
            Some((0, 8))
        );
        let regexp = migemo_regexp("pa-to.txt").unwrap();
        assert!(regexp.contains("[^/\\\\\n\r]*?\\.txt"));
    }

    #[test]
    fn suffix_after_a_short_prefix_preserves_longer_candidates() {
        let bytes = compact_dictionary_builder::build(HashMap::from([(
            "かいぎ".to_owned(),
            vec!["会議".to_owned(), "会議室".to_owned()],
        )]));
        let local = MigemoDictionary(CompactDictionary::new(&bytes));
        let matcher = MigemoMatcher::new("kaigi2026", &[&local]).unwrap();
        assert_eq!(matcher.find("会議室2026", 0, false), Some((0, 7)));
        assert_eq!(matcher.find("会議2026", 0, false), Some((0, 6)));
    }

    #[test]
    fn merges_local_words_without_hiding_bundled_prefixes() {
        let bytes = rustmigemo::migemo::compact_dictionary_builder::build(HashMap::from([
            ("ぱーとふ".to_owned(), vec!["パート譜".to_owned()]),
            ("けんさくげきじょう".to_owned(), vec!["検索劇場".to_owned()]),
            ("くらうど".to_owned(), vec!["新製品（β）".to_owned()]),
        ]));
        let local = MigemoDictionary(CompactDictionary::new(&bytes));
        let part = MigemoMatcher::new("pa-to", &[&local]).unwrap();
        assert_eq!(part.find("パートナー", 0, false), Some((0, 3)));
        assert_eq!(part.find("A PA-TO", 0, true), Some((2, 7)));
        assert_eq!(part.find("A PA-TO", 0, false), None);
        assert_eq!(part.find("ＰＡ－ＴＯ", 0, true), Some((0, 5)));
        assert_eq!(part.find("ＰＡ－ＴＯ", 0, false), None);
        let mixed = MigemoMatcher::new("kuraudo", &[&local]).unwrap();
        assert_eq!(mixed.find("😀新製品（β）", 0, false), Some((1, 7)));
        assert!(migemo_regexp("kensaku").unwrap().contains("検索"));
    }
}
