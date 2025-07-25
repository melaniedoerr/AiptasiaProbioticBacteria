#!/bin/bash

# Usage: ./add_taxonomy_to_fasta.sh asv.fasta asv_table.tsv output.fasta
# asv.fasta: FASTA file with ASV sequences (headers are ASV IDs)
# asv_table.tsv: Tab-separated file with ASV IDs and taxonomy (ASV_ID <tab> taxonomy)
# output.fasta: Output FASTA file with taxonomy added to headers

if [ "$#" -ne 3 ]; then
    echo "Usage: $0 asv.fasta asv_table.tsv output.fasta"
    exit 1
fi

FASTA="$1"
TABLE="$2"
OUT="$3"

# Create a temporary mapping file
awk -F'\t' '{print $1"\t"$2}' "$TABLE" > taxonomy_map.tsv

awk -v map=taxonomy_map.tsv '
BEGIN {
    while ((getline < map) > 0) {
        tax[$1] = $2
    }
}
{
    if ($0 ~ /^>/) {
        asv = substr($0, 2)
        if (tax[asv] != "") {
            print ">" asv " " tax[asv]
        } else {
            print $0
        }
    } else {
        print $0
    }
}
' "$FASTA" > "$OUT"

rm taxonomy_map.tsv