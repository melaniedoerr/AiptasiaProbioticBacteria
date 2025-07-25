# Input FASTQ Files

This folder should include all the FASTQ files from the specified range of SRA accessions (to be added).

Please add the relevant SRA FASTQ files here for further analysis.

## Downloading SRA FASTQ Files

You can download FASTQ files from the SRA using the `fasterq-dump` tool from the [SRA Toolkit](https://github.com/ncbi/sra-tools). Example:

```bash
# Replace SRRXXXXXXX with your SRA accession
fasterq-dump SRRXXXXXXX
```

Ensure the downloaded sra files are placed in this directory and match the names used in [input.csv](input.csv) for automated processing.
