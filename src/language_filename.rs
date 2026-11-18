//! Apply filename affixes to direct pinyin and Hangul matches.

use std::collections::HashMap;
use std::sync::Arc;

use crate::filename::{self, FilenameExtension};
use crate::hangul::HangulMatcher;
use crate::languages::{self, PinyinInitialsMatcher, PinyinMatcher, MAX_REGEXP_BYTES};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum FilenameLanguage {
    Pinyin,
    PinyinInitials,
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
    fn parse(leading: &'a str, rest: &'a str) -> Option<Self> {
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
    PinyinInitials(PinyinInitialsMatcher),
    Hangul(HangulMatcher),
}

type ReadingCache<'a> = HashMap<(&'a str, FilenameLanguage), Option<Arc<FilenameReading>>>;

impl FilenameReading {
    fn new(term: &str, language: FilenameLanguage) -> Option<Self> {
        match language {
            FilenameLanguage::Pinyin => PinyinMatcher::new(term).map(Self::Pinyin),
            FilenameLanguage::PinyinInitials => {
                PinyinInitialsMatcher::new(term).map(Self::PinyinInitials)
            }
            FilenameLanguage::Hangul => HangulMatcher::new(term).map(Self::Hangul),
        }
    }

    fn cached<'a>(
        term: &'a str,
        language: FilenameLanguage,
        cache: &mut ReadingCache<'a>,
    ) -> Option<Arc<Self>> {
        cache
            .entry((term, language))
            .or_insert_with(|| Self::new(term, language).map(Arc::new))
            .clone()
    }

    fn visit_ends(&self, text: &str, ignore_case: bool, mut visit: impl FnMut(usize)) {
        match self {
            Self::Pinyin(matcher) => matcher.prefix_ends(text).for_each(visit),
            Self::PinyinInitials(matcher) => matcher.prefix_ends(text).for_each(visit),
            Self::Hangul(matcher) => {
                if let Some(length) = matcher.match_prefix(text.chars(), ignore_case) {
                    visit(length);
                }
            }
        }
    }
}

struct FilenameAlternative {
    reading: Arc<FilenameReading>,
    leading: String,
    attached: String,
    literal_number: bool,
}

struct CompoundSegment {
    literal: String,
    readings: Vec<(Arc<FilenameReading>, String)>,
}

struct CompoundAlternative {
    leading: String,
    first: Vec<(Arc<FilenameReading>, String)>,
    segments: Vec<CompoundSegment>,
    literal_number_tiebreak: bool,
}

fn reading_parts(term: &str, language: FilenameLanguage) -> impl Iterator<Item = (&str, &str)> {
    let split = term.find('-').unwrap_or(term.len());
    let (word, affix) = term.split_at(split);
    let base = word.trim_end_matches(|c: char| c.is_ascii_digit());
    let toned = (matches!(language, FilenameLanguage::Pinyin) && base.len() != word.len())
        .then_some((word, affix));
    std::iter::once((base, &term[base.len()..])).chain(toned)
}

fn compile_readings<'a>(
    term: &'a str,
    language: FilenameLanguage,
    cache: &mut ReadingCache<'a>,
) -> Vec<(Arc<FilenameReading>, String)> {
    // An initials-enabled compound may mix full readings and initials.
    let full_pinyin =
        matches!(language, FilenameLanguage::PinyinInitials).then_some(FilenameLanguage::Pinyin);
    full_pinyin
        .into_iter()
        .chain(std::iter::once(language))
        .flat_map(|language| reading_parts(term, language).map(move |part| (language, part)))
        .filter_map(|(language, (reading, suffix))| {
            Some((
                FilenameReading::cached(reading, language, cache)?,
                suffix.to_owned(),
            ))
        })
        .collect()
}

fn advance(text: &str, count: usize) -> &str {
    text.char_indices()
        .nth(count)
        .map_or("", |(offset, _)| &text[offset..])
}

impl CompoundAlternative {
    // A reading and its affix consume no underscores. Check only the ends
    // the reading can reach; each matching segment advances through one
    // separator, even when more than one reading has the same endpoint.
    fn after_reading_separator<'a>(
        readings: &[(Arc<FilenameReading>, String)],
        text: &'a str,
        ignore_case: bool,
    ) -> Option<(usize, &'a str)> {
        readings.iter().find_map(|(reading, suffix)| {
            let mut next = None;
            reading.visit_ends(text, ignore_case, |end| {
                let tail = advance(text, end);
                if filename::literal_prefix(tail, suffix, ignore_case) {
                    let length = end + suffix.chars().count() + 1;
                    if let Some(rest) =
                        advance(tail, suffix.chars().count()).strip_prefix(['_', '＿'])
                    {
                        next = Some((length, rest));
                    }
                }
            });
            next
        })
    }

    fn visit(
        &self,
        text: &str,
        ignore_case: bool,
        extension: &str,
        visit: &mut impl FnMut(usize, &str, FilenameExtension<'_>, bool),
    ) {
        if !filename::literal_prefix(text, &self.leading, ignore_case) {
            return;
        }
        let leading = self.leading.len();
        let Some((first_len, mut tail)) =
            Self::after_reading_separator(&self.first, advance(text, leading), ignore_case)
        else {
            return;
        };
        let mut length = leading + first_len;
        let (last, intermediate) = self.segments.split_last().unwrap();
        for segment in intermediate {
            let literal_len = segment.literal.chars().count();
            let literal = filename::literal_prefix(tail, &segment.literal, ignore_case)
                .then(|| advance(tail, literal_len).strip_prefix(['_', '＿']))
                .flatten()
                .map(|rest| (literal_len + 1, rest));
            let Some((segment_len, rest)) = literal
                .or_else(|| Self::after_reading_separator(&segment.readings, tail, ignore_case))
            else {
                return;
            };
            length += segment_len;
            tail = rest;
        }
        let prefer_literal_number = self.literal_number_tiebreak;
        let literal_len = last.literal.chars().count();
        if filename::literal_prefix(tail, &last.literal, ignore_case) {
            visit(
                length + literal_len,
                advance(tail, literal_len),
                FilenameExtension::new(extension, false),
                prefer_literal_number,
            );
        }
        for (index, (reading, suffix)) in last.readings.iter().enumerate() {
            reading.visit_ends(tail, ignore_case, |end| {
                let after = advance(tail, end);
                if filename::literal_prefix(after, suffix, ignore_case) {
                    let suffix_len = suffix.chars().count();
                    visit(
                        length + end + suffix_len,
                        advance(after, suffix_len),
                        FilenameExtension::new(extension, suffix_len == 0),
                        prefer_literal_number && index == 0,
                    );
                }
            });
        }
    }
}

fn compile_compound<'a>(
    leading: &str,
    rest: &'a str,
    language: FilenameLanguage,
    cache: &mut ReadingCache<'a>,
) -> Option<CompoundAlternative> {
    let (stem, extension) = rest.split_at(rest.find('.').unwrap_or(rest.len()));
    filename::split_suffix(extension)?;
    if stem.chars().any(filename::is_component_boundary) {
        return None;
    }
    if !stem.contains('_') {
        return None;
    }
    let mut pieces = stem.split('_');
    let first = compile_readings(pieces.next()?, language, cache);
    if first.is_empty() {
        return None;
    }
    let segments: Vec<_> = pieces
        .map(|segment| CompoundSegment {
            literal: segment.to_owned(),
            readings: compile_readings(segment, language, cache),
        })
        .collect();
    if segments.is_empty() || segments.iter().any(|segment| segment.literal.is_empty()) {
        return None;
    }
    let literal_number_tiebreak = language == FilenameLanguage::Pinyin
        && !extension.is_empty()
        && segments
            .last()
            .is_some_and(|segment| segment.readings.len() == 2);
    Some(CompoundAlternative {
        leading: leading.to_owned(),
        first,
        segments,
        literal_number_tiebreak,
    })
}

pub(crate) struct LanguageFilenameMatcher {
    extension: String,
    alternatives: Vec<FilenameAlternative>,
    highest_priority: u8,
    compounds: Vec<CompoundAlternative>,
}

impl LanguageFilenameMatcher {
    pub fn new(term: &str, language: FilenameLanguage) -> Option<Self> {
        if term.chars().count() > filename::MAX_FILENAME_QUERY_CHARACTERS {
            return None;
        }
        let mut alternatives = Vec::new();
        let mut compounds = Vec::new();
        let mut reading_cache = HashMap::new();
        for (leading, rest) in filename::leading_splits(term) {
            if let Some(compound) = compile_compound(leading, rest, language, &mut reading_cache) {
                compounds.push(compound);
            }
            let Some(term) = FilenameTerm::parse(leading, rest) else {
                continue;
            };
            let mut interpretations: Vec<_> = term
                .readings(language)
                .enumerate()
                .filter_map(|(index, (reading, attached))| {
                    let reading = FilenameReading::cached(reading, language, &mut reading_cache)?;
                    Some(FilenameAlternative {
                        reading,
                        leading: term.leading.to_owned(),
                        attached: attached.to_owned(),
                        literal_number: index == 0,
                    })
                })
                .collect();
            // Only two valid interpretations with an extension get the tie break.
            if interpretations.len() != 2 || term.extension.is_empty() {
                for alternative in &mut interpretations {
                    alternative.literal_number = false;
                }
            }
            alternatives.extend(interpretations);
        }
        if alternatives.is_empty() && compounds.is_empty() {
            return None;
        }
        Some(Self {
            extension: term[term.find('.').unwrap_or(term.len())..].to_owned(),
            highest_priority: filename::encode_priority(
                filename::MAX_PRIORITY,
                alternatives
                    .iter()
                    .any(|alternative| alternative.literal_number)
                    || compounds
                        .iter()
                        .any(|compound| compound.literal_number_tiebreak),
            ),
            alternatives,
            compounds,
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
        mut visit: impl FnMut(usize, &str, FilenameExtension<'_>, bool),
    ) {
        for compound in &self.compounds {
            compound.visit(text, ignore_case, &self.extension, &mut visit);
        }
        for alternative in &self.alternatives {
            if !filename::literal_prefix(text, &alternative.leading, ignore_case) {
                continue;
            }
            // Query affixes are ASCII; candidate affixes can be fullwidth.
            let leading_length = alternative.leading.len();
            let mut chars = text.chars();
            if leading_length > 0 {
                chars.nth(leading_length - 1);
            }
            let text = chars.as_str();
            alternative.reading.visit_ends(text, ignore_case, |length| {
                let mut chars = text.chars();
                chars.nth(length - 1);
                if filename::literal_prefix(chars.as_str(), &alternative.attached, ignore_case) {
                    let attached_length = alternative.attached.len();
                    if attached_length > 0 {
                        chars.nth(attached_length - 1);
                    }
                    visit(
                        leading_length + length + attached_length,
                        chars.as_str(),
                        FilenameExtension::new(&self.extension, attached_length == 0),
                        alternative.literal_number,
                    );
                }
            });
        }
    }

    pub fn find(&self, text: &str, start: usize, ignore_case: bool) -> Option<(usize, usize)> {
        for (begin, (offset, _)) in text.char_indices().enumerate().skip(start) {
            let mut length = None;
            self.visit_stems(&text[offset..], ignore_case, |stem, tail, extension, _| {
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
            self.visit_stems(
                &text[offset..],
                ignore_case,
                |_, tail, extension, literal_number| {
                    let quality = extension
                        .match_quality(tail, ignore_case, at_component_start)
                        .map(|quality| filename::encode_priority(quality, literal_number));
                    best = best.max(quality);
                },
            );
            if best == Some(self.highest_priority) {
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
        FilenameLanguage::PinyinInitials => return None,
        FilenameLanguage::Hangul => languages::hangul_regexp,
    };
    if term.chars().count() > filename::MAX_FILENAME_QUERY_CHARACTERS {
        return None;
    }
    let mut expressions = Vec::new();
    let mut parsed = false;
    let mut total_bytes = 0;
    for (leading, rest) in filename::leading_splits(term) {
        let Some(term) = FilenameTerm::parse(leading, rest) else {
            continue;
        };
        parsed = true;
        for (reading, attached) in term.readings(language) {
            if let Some(reading) = reading_regexp(reading) {
                let mut expression = String::new();
                filename::append_literal_regexp(&mut expression, term.leading);
                expression.push_str(&reading);
                filename::append_literal_regexp(&mut expression, attached);
                filename::append_extension_regexp(
                    &mut expression,
                    term.extension,
                    attached.is_empty(),
                );
                total_bytes += expression.len();
                if total_bytes > MAX_REGEXP_BYTES {
                    return None;
                }
                expressions.push(expression);
            }
        }
    }
    // Compose segment alternatives independently, keeping the expression
    // linear while preserving literal numbers and pinyin tone interpretations.
    for (leading, rest) in filename::leading_splits(term) {
        let (stem, extension) = rest.split_at(rest.find('.').unwrap_or(rest.len()));
        if !stem.contains('_')
            || filename::split_suffix(extension).is_none()
            || stem.chars().any(filename::is_component_boundary)
        {
            continue;
        }
        let mut pieces = stem.split('_');
        let segment_regexp = |segment: &str, last: bool| -> Option<String> {
            let expressions: Vec<_> = reading_parts(segment, language)
                .filter_map(|(reading, suffix)| {
                    let mut expression = reading_regexp(reading)?;
                    filename::append_literal_regexp(&mut expression, suffix);
                    if last {
                        filename::append_extension_regexp(
                            &mut expression,
                            extension,
                            suffix.is_empty(),
                        );
                    }
                    Some(expression)
                })
                .collect();
            match expressions.as_slice() {
                [] => None,
                [only] => Some(only.clone()),
                _ => Some(format!("\\(?:{}\\)", expressions.join("\\|"))),
            }
        };
        let Some(first) = pieces
            .next()
            .and_then(|segment| segment_regexp(segment, false))
        else {
            continue;
        };
        let segments: Vec<_> = pieces.collect();
        if segments.iter().any(|segment| segment.is_empty()) {
            continue;
        }
        let mut expression = String::new();
        filename::append_literal_regexp(&mut expression, leading);
        expression.push_str(&first);
        for (index, segment) in segments.iter().enumerate() {
            filename::append_literal_regexp(&mut expression, "_");
            let mut literal = String::new();
            filename::append_literal_regexp(&mut literal, segment);
            let last = index + 1 == segments.len();
            if last {
                filename::append_extension_regexp(&mut literal, extension, false);
            }
            if let Some(reading) = segment_regexp(segment, last) {
                expression.push_str("\\(?:");
                expression.push_str(&literal);
                expression.push_str("\\|");
                expression.push_str(&reading);
                expression.push_str("\\)");
            } else {
                expression.push_str(&literal);
            }
        }
        total_bytes += expression.len();
        if total_bytes > MAX_REGEXP_BYTES {
            return None;
        }
        expressions.push(expression);
        parsed = true;
    }
    if !parsed {
        return reading_regexp(term);
    }
    let expression = match expressions.as_slice() {
        [] => return None,
        [only] => only.clone(),
        _ => format!("\\(?:{}\\)", expressions.join("\\|")),
    };
    (expression.len() <= MAX_REGEXP_BYTES).then_some(expression)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn compounds_match_readings_and_literal_segments() {
        let chinese =
            LanguageFilenameMatcher::new("zhongguo_huiyi.txt", FilenameLanguage::Pinyin).unwrap();
        assert_eq!(
            chinese.find("😀中国＿会议．ｔｘｔ", 0, false),
            Some((1, 10))
        );
        assert!(chinese.match_quality("中国_huiyi.txt", false).is_some());
        assert!(chinese.match_quality("中国/会议.txt", false).is_none());
        assert!(LanguageFilenameMatcher::new(
            "report-zhongguo_huiyi.txt",
            FilenameLanguage::Pinyin
        )
        .unwrap()
        .match_quality("report-中国_会议.txt", false)
        .is_some());
        let korean =
            LanguageFilenameMatcher::new("ㄱㅅ_ㅎㅇ.txt", FilenameLanguage::Hangul).unwrap();
        assert!(korean.match_quality("감사_회의.txt", false).is_some());
        assert!(regexp("zhongguo_huiyi.txt", FilenameLanguage::Pinyin).is_some());
        assert!(regexp("ㄱㅅ_ㅎㅇ.txt", FilenameLanguage::Hangul).is_some());
    }

    #[test]
    fn literal_number_breaks_only_equal_filename_quality() {
        let matcher =
            LanguageFilenameMatcher::new("zhongguo2.txt", FilenameLanguage::Pinyin).unwrap();
        let literal = matcher.match_quality("中国2.txt", false).unwrap();
        let tonal = matcher.match_quality("中国.txt", false).unwrap();
        assert!(literal > tonal);
        assert!(matcher.match_quality("dir/中国2.txt", false).unwrap() > tonal);
        assert!(matcher.match_quality("prefix中国2.txt", false).unwrap() < tonal);
        assert_eq!(matcher.find("中国2.txt", 0, false), Some((0, 7)));
        assert_eq!(matcher.find("中国.txt", 0, false), Some((0, 6)));
        let compound =
            LanguageFilenameMatcher::new("zhongguo_ma1.txt", FilenameLanguage::Pinyin).unwrap();
        assert!(
            compound.match_quality("中国_马1.txt", false).unwrap()
                > compound.match_quality("中国_妈.txt", false).unwrap()
        );
        let without_extension =
            LanguageFilenameMatcher::new("zhongguo2", FilenameLanguage::Pinyin).unwrap();
        assert_eq!(
            without_extension.match_quality("中国2", false),
            without_extension.match_quality("中国", false)
        );
        let dated =
            LanguageFilenameMatcher::new("zhong1guo2-2026.txt", FilenameLanguage::Pinyin).unwrap();
        assert!(dated.match_quality("中国-2026.txt", false).is_some());
    }

    #[test]
    fn fullwidth_literal_affixes_keep_character_offsets() {
        let matcher =
            LanguageFilenameMatcher::new("2026-zhongguo2.txt", FilenameLanguage::Pinyin).unwrap();
        assert_eq!(
            matcher.find("😀２０２６－中国２．ｔｘｔ", 0, false),
            Some((1, 13))
        );
        assert!(matcher
            .match_quality("２０２６－中国２．ｔｘｔ", false)
            .is_some());
        let expression = regexp("2026-zhongguo2.txt", FilenameLanguage::Pinyin).unwrap();
        assert!(expression.contains('２'));
        assert!(expression.contains('－'));
    }
    #[test]
    fn ascii_prefixes_match_original_character_offsets() {
        let chinese =
            LanguageFilenameMatcher::new("report-zhongguo.txt", FilenameLanguage::Pinyin).unwrap();
        assert_eq!(chinese.find("😀report-中国.txt", 0, false), Some((1, 14)));
        assert!(chinese.match_quality("report-中国.txt", false).is_some());
        let korean =
            LanguageFilenameMatcher::new("report-감사.txt", FilenameLanguage::Hangul).unwrap();
        assert_eq!(korean.find("😀report-감사.txt", 0, false), Some((1, 17)));
        assert!(korean.match_quality("report-감사.txt", false).is_some());
        assert!(regexp("report-zhongguo.txt", FilenameLanguage::Pinyin)
            .unwrap()
            .contains("[rｒ]"));
        assert!(regexp("report-감사.txt", FilenameLanguage::Hangul)
            .unwrap()
            .contains("[rｒ]"));
    }

    #[test]
    fn long_literal_affixes_do_not_limit_reading() {
        let prefix = "report".repeat(8);
        let query = format!("{prefix}-zhongguo-{}.txt", "a".repeat(40));
        let candidate = format!("{prefix}-中国-{}.txt", "a".repeat(40));
        let matcher = LanguageFilenameMatcher::new(&query, FilenameLanguage::Pinyin).unwrap();
        assert_eq!(
            matcher.find(&candidate, 0, false),
            Some((0, candidate.chars().count()))
        );
        assert!(regexp(&query, FilenameLanguage::Pinyin).is_some());
        let query = format!("{prefix}-감사.txt");
        let candidate = format!("{prefix}-감사.txt");
        assert!(
            LanguageFilenameMatcher::new(&query, FilenameLanguage::Hangul)
                .unwrap()
                .find(&candidate, 0, false)
                .is_some()
        );
        assert!(regexp(&query, FilenameLanguage::Hangul).is_some());
    }
}
