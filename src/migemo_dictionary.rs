//! Import a local C/Migemo text dictionary into rustmigemo's in-memory format.

use std::collections::{BTreeSet, HashMap};
use std::fs::File;
use std::io::{self, Read};
use std::path::Path;

use rustmigemo::migemo::{compact_dictionary::CompactDictionary, compact_dictionary_builder};

// The builder keeps several copies of the dictionary and its tries in memory.
const MAX_DICTIONARY_BYTES: u64 = 64 * 1024 * 1024;
const MAX_LINE_BYTES: usize = 64 * 1024;
const MAX_CANDIDATE_FIELDS: usize = 1_000_000;

/// Load a UTF-8 C/Migemo tab-separated text dictionary from PATH.
///
/// Invalid records and readings the upstream compact encoding cannot represent
/// are ignored. An empty or unusable dictionary is reported as an error.
pub fn load(path: &Path) -> io::Result<CompactDictionary> {
    let metadata = std::fs::metadata(path)?;
    if !metadata.is_file() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "Migemo dictionary must be a regular file",
        ));
    }
    if metadata.len() > MAX_DICTIONARY_BYTES {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "Migemo dictionary is too large",
        ));
    }
    let mut bytes = Vec::new();
    File::open(path)?
        .take(MAX_DICTIONARY_BYTES + 1)
        .read_to_end(&mut bytes)?;
    if bytes.len() as u64 > MAX_DICTIONARY_BYTES {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "Migemo dictionary is too large",
        ));
    }
    let text = std::str::from_utf8(&bytes).map_err(|_| {
        io::Error::new(io::ErrorKind::InvalidData, "Migemo dictionary is not UTF-8")
    })?;
    let mut entries: HashMap<String, BTreeSet<String>> = HashMap::new();
    let mut candidate_fields = 0;
    for line in text.trim_start_matches('\u{feff}').lines() {
        if line.len() > MAX_LINE_BYTES || line.starts_with(';') || line.starts_with('#') {
            continue;
        }
        let mut fields = line.trim_end_matches('\r').split('\t');
        let Some(reading) = fields.next() else {
            continue;
        };
        // The upstream builder silently removes unsupported keys and prints them.
        // Its compact key encoding accepts printable ASCII, hiragana and ー.
        if reading.is_empty()
            || !reading.chars().all(|c| {
                (' '..='~').contains(&c) || ('\u{3041}'..='\u{3096}').contains(&c) || c == 'ー'
            })
        {
            continue;
        }
        let reading = reading.to_ascii_lowercase();
        for candidate in fields {
            candidate_fields += 1;
            if candidate_fields > MAX_CANDIDATE_FIELDS {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "Migemo dictionary has too many candidates",
                ));
            }
            if candidate.is_empty() || candidate.chars().any(char::is_control) {
                continue;
            }
            entries
                .entry(reading.clone())
                .or_default()
                .insert(candidate.to_owned());
        }
    }
    if entries.is_empty() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "Migemo dictionary has no usable entries",
        ));
    }
    // Sort candidates before building so repeated readings and duplicate entries
    // produce a stable result regardless of their order in the source file.
    let entries = entries
        .into_iter()
        .map(|(key, values)| (key, values.into_iter().collect()))
        .collect();
    let compact = compact_dictionary_builder::build(entries);
    Ok(CompactDictionary::new(&compact))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};

    static NEXT_FILE: AtomicUsize = AtomicUsize::new(0);

    fn with_dictionary<T>(contents: &[u8], f: impl FnOnce(&Path) -> T) -> T {
        let path = std::env::temp_dir().join(format!(
            "nucleo-migemo-{}-{}",
            std::process::id(),
            NEXT_FILE.fetch_add(1, Ordering::Relaxed)
        ));
        std::fs::write(&path, contents).unwrap();
        let result = f(&path);
        std::fs::remove_file(path).unwrap();
        result
    }

    fn search(dict: &CompactDictionary, key: &str) -> Vec<String> {
        dict.search(&key.encode_utf16().collect())
            .map(|v| String::from_utf16(&v).unwrap())
            .collect()
    }

    #[test]
    fn imports_and_merges_readings() {
        with_dictionary("\u{feff}; comment\nけんさく\t検索\t研削\nけんさく\t検索\t探索\nABC-1\t英字\nカタカナ\tignored\nmalformed\n".as_bytes(), |path| {
            let dict = load(path).unwrap();
            assert_eq!(search(&dict, "けんさく"), vec!["探索", "検索", "研削"]);
            assert_eq!(search(&dict, "abc-1"), vec!["英字"]);
            assert!(search(&dict, "カタカナ").is_empty());
        });
    }

    #[test]
    fn rejects_invalid_or_unusable_input() {
        for bytes in [&b"\xff"[..], &b"; comment\nkey\n\tvalue\n"[..]] {
            with_dictionary(bytes, |path| {
                assert_eq!(load(path).unwrap_err().kind(), io::ErrorKind::InvalidData)
            });
        }
    }

    #[test]
    fn skips_oversized_records() {
        let text = format!("{}\tbad\nよみ\t良い\n", "a".repeat(MAX_LINE_BYTES));
        with_dictionary(text.as_bytes(), |path| {
            assert_eq!(search(&load(path).unwrap(), "よみ"), vec!["良い"])
        });
    }
}
