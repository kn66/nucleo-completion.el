#!/usr/bin/env python3
"""Extract mixed kanji/kana spellings and their readings from UniDic CWJ CSV.

Usage: python3 data/build-unidic-extra.py /path/to/lex.csv > /tmp/unidic-extra.tsv
The TSV is input to build-unidic-extra.rs; see data/README.org.
"""
import csv
import sys


def is_kanji(char):
    return '\u3400' <= char <= '\u4dbf' or '\u4e00' <= char <= '\u9fff' or char == '々'


def hiragana(text):
    return ''.join(chr(ord(char) - 0x60) if 'ァ' <= char <= 'ヶ' else char for char in text)


pairs = set()
with open(sys.argv[1], encoding='utf-8', newline='') as source:
    for row in csv.reader(source):
        if len(row) < 25:
            continue
        surface, reading = row[0], hiragana(row[24])
        # rustmigemo stores values in a UTF-16 trie and keys in a restricted
        # byte encoding. Avoid supplementary characters and unsupported keys.
        if (any(is_kanji(c) for c in surface)
                and any(('ぁ' <= c <= 'ゖ' or 'ァ' <= c <= 'ヶ')
                        and any(is_kanji(later) for later in surface[i + 1:])
                        for i, c in enumerate(surface))
                and all(ord(c) <= 0xffff and (is_kanji(c) or 'ぁ' <= c <= 'ゖ'
                        or 'ァ' <= c <= 'ヶ' or c == 'ー') for c in surface)
                and reading and all('ぁ' <= c <= 'ゖ' or c == 'ー' for c in reading)):
            pairs.add((reading, surface))
for reading, surface in sorted(pairs):
    print(reading, surface, sep='\t')
