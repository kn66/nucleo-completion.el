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

use crate::filename::{self, FilenameExtension};
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
        let (leading, rest) = filename::split_leading_number(term);
        let suffix_start = rest
            .find(|c: char| c.is_ascii_digit() || c == '.' || c == '_')
            .unwrap_or(rest.len());
        let (reading, suffix) = rest.split_at(suffix_start);
        if !valid_reading(reading) {
            return None;
        }
        // Attached characters stay adjacent; a bare extension may follow a longer stem.
        let (attached, extension) = filename::split_suffix(suffix)?;
        Some(Self {
            literal: &term[..term.len() - extension.len()],
            leading,
            reading,
            attached,
            extension,
        })
    }

    fn readings(&self) -> impl Iterator<Item = (&'a str, &'a str)> + '_ {
        // Keep the whole romaji reading for long vowels. Also try each
        // plausible hyphen as the start of a literal filename suffix.
        let separated = self.reading.match_indices('-').filter_map(|(index, _)| {
            let attached = &self.literal[self.leading.len() + index..];
            attached
                .trim_start_matches('-')
                .as_bytes()
                .first()
                .is_some_and(u8::is_ascii_alphanumeric)
                .then_some((&self.reading[..index], attached))
        });
        std::iter::once((self.reading, self.attached)).chain(separated)
    }
}

struct MigemoWords<'a, F> {
    reading: &'a str,
    collect: F,
}

impl<F: FnMut(&[char])> RegexGeneratorTrait for MigemoWords<'_, F> {
    fn add(&mut self, word: &[char]) {
        if !word.iter().all(char::is_ascii) || word.iter().copied().eq(self.reading.chars()) {
            (self.collect)(word);
        }
    }

    fn generate(&self, _: &RegexOperator) -> String {
        String::new()
    }
}

fn expand_reading(reading: &str, extra: &[&MigemoDictionary], collect: impl FnMut(&[char])) {
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
    let mut collector = MigemoWords { reading, collect };
    let dictionaries = [bundled, unidic, abbreviations]
        .into_iter()
        .chain(extra.iter().map(|dictionary| &dictionary.0));
    for dictionary in dictionaries {
        query_a_word_with_generator(
            reading,
            dictionary,
            &RegexOperator::EmacsNonNewline,
            &mut collector,
        );
    }
}

fn expand(term: &MigemoTerm<'_>, extra: &[&MigemoDictionary]) -> Vec<(String, bool)> {
    let mut spellings = BTreeSet::new();
    for (reading, attached) in term.readings() {
        expand_reading(reading, extra, |word| {
            let mut spelling = term.leading.to_owned();
            spelling.extend(word);
            spelling.push_str(attached);
            spellings.insert((spelling, attached.is_empty()));
        });
    }
    // Keep short alternatives such as パート. With an attached literal and
    // a dotted suffix, each full spelling may have its own adjacent match.
    let mut prefixes: Vec<(String, bool)> = Vec::new();
    let mut last = [None, None];
    for (word, gap) in spellings {
        let prefix = &mut last[usize::from(gap)];
        if (!gap && !term.extension.is_empty())
            || prefix.is_none_or(|index: usize| !word.starts_with(&prefixes[index].0))
        {
            *prefix = Some(prefixes.len());
            prefixes.push((word, gap));
        }
    }
    prefixes
}

pub fn migemo_regexp(term: &str) -> Option<String> {
    let term = MigemoTerm::parse(term)?;
    let mut groups = Vec::new();
    for (reading, attached) in term.readings() {
        let mut generator = TernaryRegexGenerator::new();
        let mut alternatives = Vec::new();
        let mut prefix = String::new();
        let mut words = BTreeSet::<String>::new();
        expand_reading(reading, &[], |word| {
            words.insert(word.iter().collect());
        });
        for word in words {
            if attached.is_empty() && !prefix.is_empty() && word.starts_with(&prefix) {
                continue;
            }
            prefix.clone_from(&word);
            if word.contains([
                '(', ')', '{', '}', '|', '^', '$', '.', '*', '+', '?', '[', ']', '\\', '-',
            ]) {
                // Emacs escapes a different set of characters from the
                // upstream generator, which also puts hyphens in classes.
                let mut quoted = String::new();
                for character in word.chars() {
                    if "$^.*+?[\\]".contains(character) {
                        quoted.push('\\');
                    }
                    quoted.push(character);
                }
                alternatives.push(quoted);
            } else {
                generator.add(&word.chars().collect::<Vec<_>>());
            }
        }
        let generated = generator.generate(&RegexOperator::EmacsNonNewline);
        if !generated.is_empty() {
            alternatives.push(generated);
        }
        if alternatives.is_empty() {
            continue;
        }
        let expression = match alternatives.as_slice() {
            [only] => only.clone(),
            _ => format!("\\(?:{}\\)", alternatives.join("\\|")),
        };
        // Append the shared suffix once instead of repeating it in the
        // expansion of every spelling.
        let mut group = format!("{}{expression}{attached}", term.leading);
        filename::append_extension_regexp(&mut group, term.extension, attached.is_empty());
        groups.push(group);
    }
    let result = match groups.as_slice() {
        [only] => only.clone(),
        _ => format!("\\(?:{}\\)", groups.join("\\|")),
    };
    (result.len() <= MAX_REGEXP_BYTES).then_some(result)
}

pub struct MigemoMatcher {
    literal: String,
    extension: String,
    allow_extension_gap: bool,
    words: HashMap<char, Vec<(String, bool)>>,
}

impl MigemoMatcher {
    pub fn new(term: &str, extra: &[&MigemoDictionary]) -> Option<Self> {
        let parsed = MigemoTerm::parse(term)?;
        let mut by_initial = HashMap::<_, Vec<_>>::new();
        for (word, gap) in expand(&parsed, extra) {
            if let Some(initial) = word.chars().next().filter(|_| word != parsed.literal) {
                by_initial
                    .entry(initial.to_lowercase().next().unwrap())
                    .or_default()
                    .push((word, gap));
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
            let at_component_start = offset == 0
                || text[..offset]
                    .chars()
                    .next_back()
                    .is_some_and(filename::is_component_boundary);
            let candidates = self
                .words
                .get(&initial)
                .into_iter()
                .flatten()
                .map(|(word, gap)| (word.as_str(), *gap))
                .chain(
                    (initial == literal_initial)
                        .then_some((self.literal.as_str(), self.allow_extension_gap)),
                );
            for (stem, gap) in candidates {
                if let Some(quality) =
                    self.filename_quality(tail, stem, ignore_case, at_component_start, gap)
                {
                    best = Some(best.map_or(quality, |value: u8| value.max(quality)));
                    if best == Some(filename::MAX_PRIORITY) {
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
                if let Some(length) = self.match_filename(
                    suffix,
                    &self.literal,
                    ignore_case,
                    self.allow_extension_gap,
                ) {
                    return Some((begin, begin + length));
                }
            }
            if let Some(length) = self.words.get(&initial).and_then(|words| {
                words
                    .iter()
                    .find_map(|(word, gap)| self.match_filename(suffix, word, ignore_case, *gap))
            }) {
                return Some((begin, begin + length));
            }
        }
        None
    }
    // A dotted suffix alone can follow extra characters in the same filename.
    // Return character offsets to match the module's highlighting API.
    fn match_filename(
        &self,
        text: &str,
        stem: &str,
        ignore_case: bool,
        gap: bool,
    ) -> Option<usize> {
        if !filename::literal_prefix(text, stem, ignore_case) {
            return None;
        }
        let stem_len = stem.chars().count();
        if self.extension.is_empty() {
            return Some(stem_len);
        }
        let offset = text
            .char_indices()
            .nth(stem_len)
            .map_or(text.len(), |(offset, _)| offset);
        FilenameExtension::new(&self.extension, gap)
            .match_end(&text[offset..], ignore_case)
            .map(|length| stem_len + length)
    }

    // Consider later suffixes in case the first occurrence is part of .txt.bak.
    fn filename_quality(
        &self,
        text: &str,
        stem: &str,
        ignore_case: bool,
        at_component_start: bool,
        gap: bool,
    ) -> Option<u8> {
        if !filename::literal_prefix(text, stem, ignore_case) {
            return None;
        }
        let stem_len = stem.chars().count();
        let offset = text
            .char_indices()
            .nth(stem_len)
            .map_or(text.len(), |(offset, _)| offset);
        FilenameExtension::new(&self.extension, gap).match_quality(
            &text[offset..],
            ignore_case,
            at_component_start,
        )
    }
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
    fn alphabetic_hyphen_suffixes_stay_literal() {
        for (query, text) in [
            ("nihon-report", "日本-report.txt"),
            ("nihon-report.txt", "日本-report.txt"),
            ("nihon-report-final.txt", "日本-report-final.txt"),
            ("ko-hi--2026", "コーヒー-2026"),
            ("ko-hi-2026", "コーヒー2026"),
            ("pa-to", "パートナー"),
        ] {
            assert!(
                MigemoMatcher::new(query, &[])
                    .unwrap()
                    .find(text, 0, false)
                    .is_some(),
                "{query} {text}"
            );
        }
        assert!(migemo_regexp("nihon-report").is_some());
        let matcher = MigemoMatcher::new("nihon-report.txt", &[]).unwrap();
        assert_eq!(matcher.match_quality("日本-report.txt", false), Some(3));
        assert_eq!(matcher.find("日本-report-old.txt", 0, false), None);
        assert_eq!(matcher.find("nihon-report.txt", 0, false), Some((0, 16)));
        assert!(migemo_regexp("nihon-report.txt")
            .unwrap()
            .contains("-report\\.txt"));
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
