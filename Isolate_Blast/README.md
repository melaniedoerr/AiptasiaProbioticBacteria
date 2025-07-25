# Isolate BLAST Analysis Scripts

This folder contains two bash scripts for performing BLAST analysis and adding taxonomic information to FASTA files.

## Scripts Overview

### 1. `blast-fa2fa.sh` - BLAST Database Creation and Search

This script creates a BLAST database from a reference FASTA file and performs a BLASTN search against query sequences.

**Usage:**

```bash
./blast-fa2fa.sh <database_fasta> <query_fasta> <output_file>
```

**Parameters:**

- `database_fasta`: Reference FASTA file to create the BLAST database from
- `query_fasta`: FASTA file containing query sequences to search
- `output_file`: Output file for BLAST results

**What it does:**

1. Creates a nucleotide BLAST database using `makeblastdb`
2. Runs BLASTN search with tabular output format (outfmt 6)
3. Uses 100 threads for parallel processing
4. Saves results in tab-separated format

**Example:**

```bash
./blast-fa2fa.sh reference_sequences.fasta query_asvs.fasta blast_results.tsv
```

### 2. `add_tax2fa.sh` - Add Taxonomy to FASTA Headers

This script adds taxonomic information to FASTA sequence headers using a mapping table.

**Usage:**

```bash
./add_tax2fa.sh asv.fasta asv_table.tsv output.fasta
```

**Parameters:**

- `asv.fasta`: Input FASTA file with ASV sequences (headers contain ASV IDs)
- `asv_table.tsv`: Tab-separated file with ASV IDs and taxonomy information (ASV_ID [tab] taxonomy)
- `output.fasta`: Output FASTA file with taxonomy added to headers

**What it does:**

1. Creates a temporary mapping from the taxonomy table
2. Parses the input FASTA file
3. Matches ASV IDs with taxonomy information
4. Outputs FASTA with enriched headers containing taxonomy

**Input format for taxonomy table:**

```text
ASV_001    Bacteria;Proteobacteria;Gammaproteobacteria;Vibrionales;Vibrionaceae;Vibrio
ASV_002    Bacteria;Firmicutes;Bacilli;Lactobacillales;Streptococcaceae;Streptococcus
```

**Example:**

```bash
./add_tax2fa.sh sequences.fasta taxonomy_mapping.tsv sequences_with_taxonomy.fasta
```

## Prerequisites

- BLAST+ toolkit installed (`makeblastdb`, `blastn`)
- Standard Unix tools (`awk`, `bash`)

## Output Formats

- **BLAST output**: Tab-separated format with standard BLAST fields
- **Taxonomy-enriched FASTA**: Standard FASTA format with taxonomy appended to sequence headers
