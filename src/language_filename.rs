//! Apply filename affixes to direct pinyin and Hangul matches.

use crate::filename::{self, FilenameExtension};
use crate::hangul::HangulMatcher;
use crate::languages::{self, PinyinMatcher, MAX_REGEXP_BYTES, MAX_TERM_CHARACTERS};

#[derive(Clone, Copy)]
pub(crate) enum FilenameLanguage {
    Pinyin,
    Hangul,
}

struct FilenameTerm<'a> {
    leading: &'a str,
    reading: &'a str,
    attached: &'a str,
    extension: &'a str,
    reading_with_digits: &'a str,
    punctuation_suffix: &'a str,
}

impl<'a> FilenameTerm<'a> {
    fn parse(term: &'a str) -> Option<Self> {
        if term.chars().count() > MAX_TERM_CHARACTERS {
            return None;
        }
        let (leading, rest) = filename::split_leading_number(term);
        let boundary = rest.find(['-', '_', '.']).unwrap_or(rest.len());
        let (reading_with_digits, suffix) = rest.split_at(boundary);
        let (punctuation_suffix, extension) = filename::split_suffix(suffix)?;
        let reading = reading_with_digits.trim_end_matches(|c: char| c.is_ascii_digit());
        let attached = &rest[reading.len()..rest.len() - extension.len()];
        if reading.is_empty() || (leading.is_empty() && attached.is_empty() && extension.is_empty())
        {
            return None;
        }
        Some(Self {
            leading,
            reading,
            attached,
            extension,
            reading_with_digits,
            punctuation_suffix,
        })
    }

    fn readings(&self, language: FilenameLanguage) -> impl Iterator<Item = (&'a str, &'a str)> {
        // A trailing digit can be part of pinyin or literal filename text.
        // Try both interpretations and let the reading matcher validate tones.
        let tonal = (matches!(language, FilenameLanguage::Pinyin)
            && self.reading != self.reading_with_digits)
            .then_some((self.reading_with_digits, self.punctuation_suffix));
        std::iter::once((self.reading, self.attached)).chain(tonal)
    }
}

enum FilenameReading {
    Pinyin(PinyinMatcher),
    Hangul(HangulMatcher),
}

struct FilenameAlternative {
    reading: FilenameReading,
    attached: String,
}

pub(crate) struct LanguageFilenameMatcher {
    leading: String,
    extension: String,
    alternatives: Vec<FilenameAlternative>,
}

impl LanguageFilenameMatcher {
    pub fn new(term: &str, language: FilenameLanguage) -> Option<Self> {
        let term = FilenameTerm::parse(term)?;
        let alternatives: Vec<_> = term
            .readings(language)
            .filter_map(|(reading, attached)| {
                let reading = match language {
                    FilenameLanguage::Pinyin => {
                        FilenameReading::Pinyin(PinyinMatcher::new(reading)?)
                    }
                    FilenameLanguage::Hangul => {
                        FilenameReading::Hangul(HangulMatcher::new(reading)?)
                    }
                };
                Some(FilenameAlternative {
                    reading,
                    attached: attached.to_owned(),
                })
            })
            .collect();
        (!alternatives.is_empty()).then_some(Self {
            leading: term.leading.to_owned(),
            extension: term.extension.to_owned(),
            alternatives,
        })
    }

    pub fn has_filename_priority(&self) -> bool {
        !self.extension.is_empty()
    }

    // Pinyin can finish at several positions because syllable boundaries are
    // ambiguous. Check the affixes for each possible end of the reading.
    fn visit_stems(
        &self,
        text: &str,
        ignore_case: bool,
        mut visit: impl FnMut(usize, &str, FilenameExtension<'_>),
    ) {
        let Some(text) = text.strip_prefix(self.leading.as_str()) else {
            return;
        };
        for alternative in &self.alternatives {
            let mut visit_end = |length| {
                let mut chars = text.chars();
                chars.nth(length - 1);
                if filename::literal_prefix(chars.as_str(), &alternative.attached, ignore_case) {
                    let attached_length = alternative.attached.len();
                    if attached_length > 0 {
                        chars.nth(attached_length - 1);
                    }
                    visit(
                        self.leading.len() + length + attached_length,
                        chars.as_str(),
                        FilenameExtension::new(&self.extension, attached_length == 0),
                    );
                }
            };
            match &alternative.reading {
                FilenameReading::Pinyin(matcher) => {
                    matcher.prefix_ends(text).for_each(visit_end);
                }
                FilenameReading::Hangul(matcher) => {
                    if let Some(length) = matcher.match_prefix(text.chars(), ignore_case) {
                        visit_end(length);
                    }
                }
            }
        }
    }

    pub fn find(&self, text: &str, start: usize, ignore_case: bool) -> Option<(usize, usize)> {
        for (begin, (offset, _)) in text.char_indices().enumerate().skip(start) {
            let mut length = None;
            self.visit_stems(&text[offset..], ignore_case, |stem, tail, extension| {
                if let Some(suffix) = extension.match_end(tail, ignore_case) {
                    length = length.max(Some(stem + suffix));
                }
            });
            if let Some(length) = length {
                return Some((begin, begin + length));
            }
        }
        None
    }

    pub fn match_quality(&self, text: &str, ignore_case: bool) -> Option<u8> {
        let mut at_component_start = true;
        let mut best = None;
        for (offset, character) in text.char_indices() {
            self.visit_stems(&text[offset..], ignore_case, |_, tail, extension| {
                best = best.max(extension.match_quality(tail, ignore_case, at_component_start));
            });
            if best == Some(filename::MAX_PRIORITY) {
                break;
            }
            at_component_start = filename::is_component_boundary(character);
        }
        best
    }
}

pub(crate) fn regexp(term: &str, language: FilenameLanguage) -> Option<String> {
    let reading_regexp = match language {
        FilenameLanguage::Pinyin => languages::pinyin_regexp,
        FilenameLanguage::Hangul => languages::hangul_regexp,
    };
    let Some(term) = FilenameTerm::parse(term) else {
        return reading_regexp(term);
    };
    let mut expressions = Vec::new();
    for (reading, attached) in term.readings(language) {
        if let Some(reading) = reading_regexp(reading) {
            let mut expression = format!("{}{reading}{attached}", term.leading);
            filename::append_extension_regexp(&mut expression, term.extension, attached.is_empty());
            expressions.push(expression);
        }
    }
    let expression = match expressions.as_slice() {
        [] => return None,
        [only] => only.clone(),
        _ => format!("\\(?:{}\\)", expressions.join("\\|")),
    };
    (expression.len() <= MAX_REGEXP_BYTES).then_some(expression)
}
