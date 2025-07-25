#!/bin/bash

# Usage: ./blastn_script.sh <database_fasta> <query_fasta> <output_file>

if [ "$#" -ne 3 ]; then
    echo "Usage: $0 <database_fasta> <query_fasta> <output_file>"
    exit 1
fi

DB_FASTA="$1"
QUERY_FASTA="$2"
OUTPUT_FILE="$3"
DB_NAME="blast_db"

# Make BLAST database
makeblastdb -in "$DB_FASTA" -dbtype nucl -out "$DB_NAME"

# Run BLASTN
blastn -query "$QUERY_FASTA" -db "$DB_NAME" -out "$OUTPUT_FILE" -outfmt 6 -num_threads 100

echo "BLASTN search completed. Results in $OUTPUT_FILE"