# EngineeringCoralResilience

This repository contains code and data for the analysis and figure generation supporting the Ph.D. thesis:
**"Engineering Coral Resilience: Functional Testing of Probiotic Bacterial Isolates in Aiptasia Under Thermal Stress"**  
by Dörr, Melanie, submitted in 2025.  
NCBI BioProject: [PRJNA1297037](https://www.ncbi.nlm.nih.gov/bioproject/PRJNA1297037)

## Contents

- Data files for nf-core/ampliseq analysis
- Figure generation code
- Supplementary materials

## Citation

If you use this code or data, please cite the thesis above.

## Running nf-core/ampliseq

To perform amplicon sequencing analysis using nf-core/ampliseq, run the following command in the project directory:

```bash
nextflow run nf-core/ampliseq -r 2.13.0 -name DADA2_AIP_MD -profile conda -params-file nf-params.json
```

This will execute the pipeline with the specified parameters defined in `nf-params.json`.
