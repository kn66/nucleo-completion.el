//! Build a compact dictionary from the sorted TSV produced by
//! build-unidic-extra.py, using the same rustmigemo version as the package.
use rustmigemo::migemo::{
    compact_dictionary::CompactDictionary, compact_dictionary_builder::build,
};
use std::{collections::HashMap, env, fs};

fn main() {
    let args: Vec<String> = env::args().collect();
    assert_eq!(args.len(), 3, "usage: build-unidic-extra INPUT.tsv OUTPUT");
    let input = fs::read_to_string(&args[1]).expect("read TSV");
    let mut entries: HashMap<String, Vec<String>> = HashMap::new();
    for line in input.lines() {
        let (reading, surface) = line.split_once('\t').expect("reading and surface");
        entries
            .entry(reading.into())
            .or_default()
            .push(surface.into());
    }
    let bytes = build(entries.clone());
    let dictionary = CompactDictionary::new(&bytes);
    for (reading, surfaces) in &entries {
        let found: Vec<String> = dictionary
            .search(&reading.encode_utf16().collect())
            .map(|s| String::from_utf16(&s).unwrap())
            .collect();
        assert_eq!(&found, surfaces, "round trip for {reading}");
    }
    eprintln!(
        "{} readings, {} pairs, {} bytes verified",
        entries.len(),
        input.lines().count(),
        bytes.len()
    );
    fs::write(&args[2], bytes).expect("write dictionary");
}
