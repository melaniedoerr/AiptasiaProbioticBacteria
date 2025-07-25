#!/bin/bash

# Simple script to trim paired-end FASTQ files using Trimmomatic
# This script processes all paired-end files in the specified input directory. And cuts the first 10 bases from the reads.

for R1 in *_1.fastq.gz; do
    R2="${R1/_1.fastq.gz/_2.fastq.gz}"
    if [[ -f "$R2" ]]; then
        BASE="${R1%_1.fastq.gz}"
        OUT_R1="${BASE}_1.trimmed.fastq.gz"
        OUT_R2="${BASE}_2.trimmed.fastq.gz"
        UNP_R1="${BASE}_1.unpaired.fastq.gz"
        UNP_R2="${BASE}_2.unpaired.fastq.gz"

        # trimmomatic PE \in    
        trimmomatic PE  "$R1" "$R2" \
            "$OUT_R1" "$UNP_R1" \
            "$OUT_R2" "$UNP_R2" \
            HEADCROP:10
    else
        echo "Warning: Paired file for $R1 not found (expected $R2)"
    fi &
done