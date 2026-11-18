//! Literal filename affixes shared by the language matchers.

pub(crate) const MAX_PRIORITY: u8 = 3;

pub(crate) fn split_leading_number(term: &str) -> (&str, &str) {
    let digits = term.bytes().take_while(u8::is_ascii_digit).count();
    let end = if digits > 0 && matches!(term.as_bytes().get(digits), Some(b'-' | b'_')) {
        digits + 1
    } else {
        digits
    };
    term.split_at(end)
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
    if !ignore_case {
        return false;
    }
    let mut text = text.chars();
    prefix.chars().all(|expected| {
        text.next()
            .is_some_and(|actual| actual.to_lowercase().eq(expected.to_lowercase()))
    })
}

pub(crate) fn append_extension_regexp(result: &mut String, extension: &str, allow_gap: bool) {
    if !extension.is_empty() {
        if allow_gap {
            // Stop at the first extension in this component for highlighting.
            result.push_str("[^/\\\\\n\r]*?");
        }
        for character in extension.chars() {
            if character == '.' {
                result.push('\\');
            }
            result.push(character);
        }
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
            .filter_map(move |(position, (byte_offset, _))| {
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
