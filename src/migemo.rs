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

fn valid_term(term: &str) -> bool {
    let mut bytes = term.bytes();
    if term.len() > MAX_TERM_CHARACTERS
        || !bytes.next().is_some_and(|byte| byte.is_ascii_alphabetic())
    {
        return false;
    }
    term.bytes().zip(bytes).all(|(previous, byte)| {
        byte.is_ascii_alphabetic()
            || byte == b'-'
            || (byte == b'\'' && previous.eq_ignore_ascii_case(&b'n'))
    })
}

struct MigemoWords<'a> {
    literal: &'a str,
    words: BTreeSet<String>,
}

impl RegexGeneratorTrait for MigemoWords<'_> {
    fn add(&mut self, word: &[char]) {
        if !word.is_empty() && !word.iter().all(char::is_ascii) {
            self.words.insert(word.iter().collect());
        } else if word.iter().copied().eq(self.literal.chars()) {
            self.words.insert(self.literal.to_owned());
        }
    }

    fn generate(&self, _: &RegexOperator) -> String {
        String::new()
    }
}

fn expand(term: &str, extra: &[&MigemoDictionary]) -> Option<Vec<String>> {
    if !valid_term(term) {
        return None;
    }
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
        literal: term,
        words: BTreeSet::new(),
    };
    let dictionaries = [bundled, unidic, abbreviations]
        .into_iter()
        .chain(extra.iter().map(|dictionary| &dictionary.0));
    for dictionary in dictionaries {
        query_a_word_with_generator(
            term,
            dictionary,
            &RegexOperator::EmacsNonNewline,
            &mut collector,
        );
    }
    // Shorter prefixes already match every longer spelling at the same
    // position. Keep them before building either representation so a longer
    // dictionary entry cannot hide a kana prefix such as パート.
    let mut prefixes: Vec<String> = Vec::new();
    for word in collector.words {
        if prefixes
            .last()
            .is_none_or(|prefix| !word.starts_with(prefix))
        {
            prefixes.push(word);
        }
    }
    Some(prefixes)
}

pub fn migemo_regexp(term: &str) -> Option<String> {
    let mut generator = TernaryRegexGenerator::new();
    let mut literal_alternatives = Vec::new();
    for word in expand(term, &[])? {
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
    let result = match literal_alternatives.as_slice() {
        [only] => only.clone(),
        _ => format!("\\(?:{}\\)", literal_alternatives.join("\\|")),
    };
    (result.len() <= MAX_REGEXP_BYTES).then_some(result)
}

pub struct MigemoMatcher {
    literal: String,
    words: HashMap<char, Vec<String>>,
}

impl MigemoMatcher {
    pub fn new(term: &str, extra: &[&MigemoDictionary]) -> Option<Self> {
        let mut by_initial = HashMap::<_, Vec<_>>::new();
        for word in expand(term, extra)? {
            if let Some(initial) = word.chars().next().filter(|_| word != term) {
                by_initial
                    .entry(initial.to_lowercase().next().unwrap())
                    .or_default()
                    .push(word);
            }
        }
        Some(Self {
            literal: term.to_owned(),
            words: by_initial,
        })
    }

    pub fn find(&self, text: &str, start: usize, ignore_case: bool) -> Option<(usize, usize)> {
        let literal_initial = self.literal.as_bytes()[0].to_ascii_lowercase() as char;
        for (begin, (offset, first)) in text.char_indices().enumerate().skip(start) {
            let suffix = &text[offset..];
            let initial = first.to_lowercase().next().unwrap();
            if initial == literal_initial && matches_prefix(suffix, &self.literal, ignore_case) {
                return Some((begin, begin + self.literal.len()));
            }
            if let Some(word) = self.words.get(&initial).and_then(|words| {
                words
                    .iter()
                    .find(|word| matches_prefix(suffix, word, ignore_case))
            }) {
                return Some((begin, begin + word.chars().count()));
            }
        }
        None
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
