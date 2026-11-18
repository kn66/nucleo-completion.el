use rustmigemo::migemo::{
    bit_vector::BitVector, compact_dictionary::CompactDictionary,
    compact_dictionary_builder::build, louds_trie::LoudsTrie, query::query_a_word,
    regex_generator::RegexOperator,
};
use std::collections::HashMap;

#[test]
fn rank_accepts_end_of_word_and_block_boundaries() {
    for size in [0, 64, 512, 1024] {
        let vector = BitVector::new(vec![0x5555_5555_5555_5555; size / 64], size);
        assert_eq!(vector.rank(size, true), size / 2);
        assert_eq!(vector.rank(size, false), size / 2);
    }
}

#[test]
fn last_leaf_has_no_child() {
    let keys = vec!["けいじ".encode_utf16().collect()];
    let (trie, _) = LoudsTrie::build(&keys);
    let leaf = trie.get(&keys[0]).unwrap();
    assert_eq!(trie.first_child(leaf), None);
    assert_eq!(
        trie.get(&"けいじず".encode_utf16().collect::<Vec<_>>()),
        None
    );
}

#[test]
fn tiny_dictionary_supports_incomplete_romaji_and_extensions() {
    let entries = HashMap::from([("けいじ".into(), vec!["軽自".into()])]);
    let dictionary = CompactDictionary::new(&build(entries));
    for word in ["keij", "keiz", "keiji", "keijiz"] {
        let pattern = query_a_word(word, &dictionary, &RegexOperator::EmacsNonNewline);
        assert!(!pattern.is_empty());
    }
    let found: Vec<_> = dictionary
        .predictive_search(&"けいじ".encode_utf16().collect::<Vec<_>>())
        .map(|s| String::from_utf16(&s).unwrap())
        .collect();
    assert!(found.iter().any(|s| s == "軽自"));
}
