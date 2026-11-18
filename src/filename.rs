//! Literal filename affixes shared by the language matchers.

pub(crate) const MAX_PRIORITY: u8 = 3;
// Bound work on long compound queries independently of the reading limit.
pub(crate) const MAX_FILENAME_QUERY_CHARACTERS: usize = 255;

/// Keep the extension tier primary across all language matchers.
pub(crate) fn encode_priority(extension_quality: u8, literal_number: bool) -> u8 {
    extension_quality * 2 + u8::from(literal_number)
}

/// Fold only filename characters supported by the ASCII query syntax.
#[inline]
pub(crate) fn fold_width(character: char) -> char {
    match character {
        'Ａ'..='Ｚ' | 'ａ'..='ｚ' | '０'..='９' | '－' | '＿' | '．' | '＇' | '：' => {
            char::from_u32(character as u32 - 0xfee0).unwrap()
        }
        _ => character,
    }
}

pub(crate) fn split_leading_number(term: &str) -> (&str, &str) {
    let digits = term.bytes().take_while(u8::is_ascii_digit).count();
    let end = if digits > 0 && matches!(term.as_bytes().get(digits), Some(b'-' | b'_')) {
        digits + 1
    } else {
        digits
    };
    term.split_at(end)
}

/// Try the original numeric prefix and literal ASCII prefixes ending at a
/// filename separator. Stop at the extension or the first non-ASCII segment.
pub(crate) fn leading_splits(term: &str) -> impl Iterator<Item = (&str, &str)> {
    let original = split_leading_number(term);
    let first_is_alphanumeric = term
        .as_bytes()
        .first()
        .is_some_and(u8::is_ascii_alphanumeric);
    let explicit = term
        .bytes()
        .enumerate()
        .take_while(move |&(_, byte)| {
            first_is_alphanumeric && (byte.is_ascii_alphanumeric() || matches!(byte, b'-' | b'_'))
        })
        .filter_map(move |(index, byte)| {
            let end = index + 1;
            if matches!(byte, b'-' | b'_')
                && end > original.0.len()
                && term
                    .as_bytes()
                    .get(end)
                    .is_some_and(|next| !matches!(next, b'-' | b'_' | b'.'))
            {
                Some(term.split_at(end))
            } else {
                None
            }
        });
    std::iter::once(original).chain(explicit)
}

pub(crate) fn split_suffix(suffix: &str) -> Option<(&str, &str)> {
    if !suffix
        .bytes()
        .all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'.' | b'_' | b'-'))
    {
        return None;
    }
    Some(suffix.split_at(suffix.find('.').unwrap_or(suffix.len())))
}

pub(crate) fn is_component_boundary(character: char) -> bool {
    matches!(character, '/' | '\\' | '\n' | '\r')
}

#[inline]
pub(crate) fn literal_prefix(text: &str, prefix: &str, ignore_case: bool) -> bool {
    if text.starts_with(prefix) {
        return true;
    }
    let mut text = text.chars();
    prefix.chars().all(|expected| {
        text.next().is_some_and(|actual| {
            let actual = fold_width(actual);
            let expected = fold_width(expected);
            actual == expected || (ignore_case && actual.to_lowercase().eq(expected.to_lowercase()))
        })
    })
}

pub(crate) fn append_literal_regexp(result: &mut String, literal: &str) {
    for character in literal.chars() {
        if character.is_ascii_alphanumeric() || matches!(character, '-' | '_' | '.' | '\'' | ':') {
            let wide = char::from_u32(character as u32 + 0xfee0).unwrap();
            // A hyphen is literal when it appears first in a bracket expression.
            result.push('[');
            result.push(character);
            result.push(wide);
            result.push(']');
        } else {
            if "$^.*+?[\\]".contains(character) {
                result.push('\\');
            }
            result.push(character);
        }
    }
}

pub(crate) fn append_extension_regexp(result: &mut String, extension: &str, allow_gap: bool) {
    if !extension.is_empty() {
        if allow_gap {
            // Stop at the first extension in this component for highlighting.
            result.push_str("[^/\\\\\n\r]*?");
        }
        append_literal_regexp(result, extension);
    }
}

pub(crate) struct FilenameExtension<'a> {
    literal: &'a str,
    allow_gap: bool,
}

impl<'a> FilenameExtension<'a> {
    pub fn new(extension: &'a str, allow_gap: bool) -> Self {
        Self {
            literal: extension,
            allow_gap,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.literal.is_empty()
    }

    // Each occurrence gives the offset of the dot and the rest of the text.
    // Offsets count characters so highlighting uses Emacs string positions.
    fn occurrences<'text>(
        &self,
        text: &'text str,
        ignore_case: bool,
    ) -> impl Iterator<Item = (usize, &'text str)> + use<'a, 'text> {
        let Self { literal, allow_gap } = *self;
        text.char_indices()
            .enumerate()
            .take_while(move |&(position, (_, character))| {
                (allow_gap || position == 0) && !is_component_boundary(character)
            })
            .filter_map(move |(position, (byte_offset, character))| {
                if !matches!(character, '.' | '．') {
                    return None;
                }
                let tail = &text[byte_offset..];
                literal_prefix(tail, literal, ignore_case).then_some((position, tail))
            })
    }

    pub fn match_end(&self, text_after_stem: &str, ignore_case: bool) -> Option<usize> {
        if self.is_empty() {
            return Some(0);
        }
        let (gap, _) = self.occurrences(text_after_stem, ignore_case).next()?;
        Some(gap + self.literal.len())
    }

    pub fn match_quality(
        &self,
        text_after_stem: &str,
        ignore_case: bool,
        at_component_start: bool,
    ) -> Option<u8> {
        if self.is_empty() {
            return Some(0);
        }
        self.occurrences(text_after_stem, ignore_case)
            .map(|(gap, tail)| {
                let terminal = tail
                    .chars()
                    .nth(self.literal.len())
                    .is_none_or(is_component_boundary);
                match (gap == 0, terminal) {
                    (true, true) if at_component_start => MAX_PRIORITY,
                    (_, true) => 2,
                    (true, false) => 1,
                    (false, false) => 0,
                }
            })
            .max()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn filename_prefixes_preserve_separators_and_stop_at_the_extension() {
        assert_eq!(
            leading_splits("report-2026_kaigi.final-v03").collect::<Vec<_>>(),
            [
                ("", "report-2026_kaigi.final-v03"),
                ("report-", "2026_kaigi.final-v03"),
                ("report-2026_", "kaigi.final-v03")
            ]
        );
        assert_eq!(
            leading_splits("2026-kaigi").collect::<Vec<_>>(),
            [("2026-", "kaigi")]
        );
        assert_eq!(
            leading_splits("report-감사.txt").collect::<Vec<_>>(),
            [("", "report-감사.txt"), ("report-", "감사.txt")]
        );
        assert_eq!(
            leading_splits("report-").collect::<Vec<_>>(),
            [("", "report-")]
        );
    }
}
