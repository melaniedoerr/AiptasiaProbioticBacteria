# AiptasiaProbioticBacteria

This repository contains code and data for the analysis and figure generation supporting the manuscript:
**"Engineering Coral Resilience: Functional Testing of Probiotic Bacteria to Increase Thermal Stress Tolerance of the Coral Model Aiptasia"**  
by [Author Names], published in [Journal Name, Year].  
DOI: [doi placeholder]

## Contents

- Data files for nf-core/ampliseq analysis
- Figure generation code
- Supplementary materials

## Citation

If you use this code or data, please cite the manuscript above.

## Running nf-core/ampliseq

To perform amplicon sequencing analysis using nf-core/ampliseq, run the following command in the project directory:

```bash
nextflow run nf-core/ampliseq -r 2.13.0 -name DADA2_AIP_MD -profile conda -params-file nf-params.json
```

This will execute the pipeline with the specified parameters defined in `nf-params.json`.
