# Load packages
library(dplyr)
library(tidyr)
library(tidyverse)
library(vegan)
library(VennDiagram)
library(ggpubr)
library(RColorBrewer)
library(ggplot2)
library(ggtext)
library(grid)
library(cowplot)
library(httpgd)
library(phyloseq)
library(MiscMetabar)
library(ggvenn)
library(ggVennDiagram)
library(microbiome)
library(writexl)

install.packages("MiscMetabar")

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("phyloseq")

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("dada2")

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("microbiome")

# Filter ASV table 
# read ASV table
#setwd("~/Desktop/PhD_VoolstraLab/Paper_BacteriaScreen/Figures/Figure2/DADA2_results_NO-RS_MD/dada2")
#ASV_table <- read.table("ASV_table.tsv", header = TRUE, sep = "\t", row.names = 1)
# mark all values in the ASV table > 1 (< 1 = FALSE, > 1 = TRUE)
#singleton_filter <- ASV_table > 1
# filter out everything that appears in only one sample but keeping every ASV that appears at least 2 times within one sample in at least 2 samples (across all samples)
#ASV_filtered <- ASV_table[rowSums(singleton_filter) > 1, ]
# additional filtering of any ASV that still has a total row count of less than 10 across all samples
#ASV_filtered <- ASV_filtered[rowSums(ASV_filtered) > 10, ]

##### Work only from phyloseq object #####
# read phyloseq object
setwd("path/to/directory")
physeq <- readRDS("dada2_phyloseq.rds")

hgd()

# get ASV table as a dataframe
otu_df <- as.data.frame(as(otu_table(physeq), "matrix"))
# mark all values in the ASV table > 1 (< 1 = FALSE, > 1 = TRUE)
singleton_filter <- otu_df > 1
# filter out everything that appears in only one sample but keeping every ASV that appears at least 2 times within one sample in at least 2 samples (across all samples)
otu_df_filtered <- otu_df[rowSums(singleton_filter) > 1, ]
# additional filtering of any ASV that still has a total row count of less than 10 across all samples
otu_df_filtered <- otu_df_filtered[rowSums(otu_df_filtered) > 10, ]

# Replace the OTU table in the phyloseq object with the manually filtered ASV table
# Convert to phyloseq OTU table
otu_new <- otu_table(as.matrix(otu_df_filtered), taxa_are_rows = TRUE)

# sanity checking
setdiff(rownames(otu_df_filtered), rownames(tax_table(physeq)))
any(rownames(tax_table(physeq)) == "" | is.na(rownames(tax_table(physeq))))
any(duplicated(rownames(otu_new)))
any(duplicated(rownames(tax_table(physeq))))

# Subset taxonomy table to filtered ASVs -> only keep ASVs that are present in both: tax and otu/ASV table
tax_new <- tax_table(physeq)[rownames(otu_df_filtered), , drop=FALSE]

# Re-build phyloseq object
physeq_filtered <- phyloseq(
  otu_new,
  tax_new,
  sample_data(physeq))

##### Subset phyloseq #####
physeq_filtered_noSW <- subset_samples(physeq_filtered, type != "seawater")

##### BLAST results #####
# read BLAST results file 
setwd("~/Desktop/PhD_VoolstraLab/Paper_BacteriaScreen/Manuscript/Main_Tables/Table3+4_IsolateBLAST")
BLASThits <- readxl::read_excel("filtered_BLAST_ISOLATE2ASV_Table3_Input.xlsx")

# normalize ASV abundance counts to relative abundance (per sample - each sample adds up to 100 %)#
physeq_rel <- transform_sample_counts(physeq_filtered_noSW, function(x) (x / sum(x)) * 100)

# extract the ASV IDs from your BLAST hits 
blast_asvs <- BLASThits$ASV_ID
head(blast_asvs)

# subset ASV table to only ASVs with BLAST Hits (the relative abundance values remain relative to the total sample, not just the subset) and make it a dataframe 
physeq_blast <- prune_taxa(taxa_names(physeq_rel) %in% blast_asvs, physeq_rel)
physeq_blast <- psmelt(physeq_blast)

# change column name in BLASThits dataframe from OTU to ASV_ID to join it with subsetted ASV table 
colnames(BLASThits)[colnames(BLASThits) == "ASV_ID"] <- "OTU"

# join dataframes by ASV ID
df_merged <- left_join(physeq_blast, BLASThits, by = "OTU")

# explore output
write_xlsx(df_merged, "BLASTHits_ASVs_merged.xlsx")


##### Repeat but split the ASV table by Aiptasia strain before merging with BLASThits #####
# split physeq_blast by strain 
physeq_F003 <- subset_samples(physeq_filtered_noSW, strain == "F003")
physeq_H2   <- subset_samples(physeq_filtered_noSW, strain == "H2")

# normalize each subset to relative abundance 
physeq_F003_rel <- transform_sample_counts(physeq_F003, function(x) (x / sum(x)) * 100)
physeq_H2_rel   <- transform_sample_counts(physeq_H2,   function(x) (x / sum(x)) * 100)

# split Blasthits by isolate_Strain
BLASThits_F003 <- filter(BLASThits, isolate_Strain == "F003")
BLASThits_H2 <- filter(BLASThits, isolate_Strain == "H2")

# extract the ASV IDs from split by strain BLAST hits (column ASV_ID might be renamed to OTU already depending on how you run the code)
blast_asvs_F003 <- BLASThits_F003$OTU
blast_asvs_H2 <- BLASThits_H2$OTU

# subset to BLAST-Hit ASVs
physeq_F003_blast <- prune_taxa(taxa_names(physeq_F003_rel) %in% blast_asvs_F003, physeq_F003_rel)
physeq_H2_blast   <- prune_taxa(taxa_names(physeq_H2_rel)   %in% blast_asvs_H2, physeq_H2_rel)

# melt each subset into a dataframe format
df_F003 <- psmelt(physeq_F003_blast)
df_H2   <- psmelt(physeq_H2_blast)

# merge each subset with BLASThits
df_F003_merged <- left_join(df_F003, BLASThits_F003, by = "OTU")
df_H2_merged   <- left_join(df_H2,   BLASThits_H2, by = "OTU")

# explore output
write_xlsx(df_F003_merged, "BLASTHits_ASVs_F003_merged.xlsx")
write_xlsx(df_H2_merged, "BLASTHits_ASVs_H2_merged.xlsx")


##### Average relative abundances across samples #####
# F003
df_avg_F003 <- df_F003_merged %>%
  group_by(OTU, isolate_ID, timepoint, pident, length) %>%  # include all relevant BLAST columns
  summarise(mean_abundance = mean(Abundance, na.rm = TRUE), .groups = "drop")

# Add the range of abundances to the dataframe 
df_avg_F003_range <- df_F003_merged %>%
  group_by(OTU, isolate_ID, timepoint, pident, length) %>%
  summarise(
    mean_abundance = mean(Abundance, na.rm = TRUE),
    min_abundance  = if (any(Abundance > 0, na.rm = TRUE)) min(Abundance[Abundance > 0], na.rm = TRUE) else 0,
    max_abundance  = max(Abundance, na.rm = TRUE),
    .groups = "drop")

# H2
df_avg_H2 <- df_H2_merged %>%
  group_by(OTU, isolate_ID, timepoint, pident, length) %>%  # include all relevant BLAST columns
  summarise(mean_abundance = mean(Abundance, na.rm = TRUE), .groups = "drop")

# Add the range of abundances to the dataframe 
df_avg_H2_range <- df_H2_merged %>%
  group_by(OTU, isolate_ID, timepoint, pident, length) %>%
  summarise(
    mean_abundance = mean(Abundance, na.rm = TRUE),
    min_abundance  = if (any(Abundance > 0, na.rm = TRUE)) min(Abundance[Abundance > 0], na.rm = TRUE) else 0,
    max_abundance  = max(Abundance, na.rm = TRUE),
    .groups = "drop")

# explore output
write_xlsx(df_avg_F003_range, "Abundances_BLAST_ASVs_F003.xlsx")
write_xlsx(df_avg_H2_range, "Abundances_BLAST_ASVs_H2.xlsx")


#### Add ASV number from final ASV table to the BLAST hit results - join by Original_ASV_ID ####
# Prepare a key table 
asv_key <- final_table %>%
  select(ASV_number, Original_ASV_ID)

# Merge with BLAST Summary Tables 
# For F003
df_avg_F003_range_numbered <- df_avg_F003_range %>%
  left_join(asv_key, by = c("OTU" = "Original_ASV_ID")) %>%
  relocate(ASV_number, .before = OTU)  # Move ASV_number to the front

# For H2
df_avg_H2_range_numbered <- df_avg_H2_range %>%
  left_join(asv_key, by = c("OTU" = "Original_ASV_ID")) %>%
  relocate(ASV_number, .before = OTU)

# explore output
write_xlsx(df_avg_F003_range_numbered, "Abundances_BLAST_ASVs_F003_numb.xlsx")
write_xlsx(df_avg_H2_range_numbered, "Abundances_BLAST_ASVs_H2_numb.xlsx")
