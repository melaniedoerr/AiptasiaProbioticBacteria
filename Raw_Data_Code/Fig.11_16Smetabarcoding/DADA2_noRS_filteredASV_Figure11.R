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
library(pairwiseAdonis)

install.packages("MiscMetabar")

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("phyloseq")

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("dada2")

if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
devtools::install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")


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

#### Total number of families in all samples ####
taxa_table <- as.data.frame(tax_table(physeq_filtered))

families <- taxa_table %>%
  filter(
    !is.na(Family),          # Remove NA values
    Family != "",            # Remove empty strings
    !grepl("uncultured|unknown", Family, ignore.case = TRUE)  # Custom filters
  ) %>%
  pull(Family)   

num_families <- length(unique(families))
print(paste("Number of bacterial families:", num_families))

##### Pies across all samples per strain and timepoint #####
# Check for NAs in Family
head(sample_data(physeq_filtered))
# Agglomerate to Family level and calculate mean relative abundance
physeq_family <- physeq_filtered %>%
  tax_glom("Family") %>%
  transform_sample_counts(function(x) x / sum(x)) %>%
  merge_samples(group = c("strain_type_timepoint"), fun = mean) %>% 
  psmelt()
# After merging, the sample data for the new, merged samples only contains the grouping variable (e.g., strain_type_timepoint) as the sample name.
# All other metadata fields are not automatically preserved or recalculated for the merged sample, so they appear as NA in the output of psmelt().

# Identify top 15 families
top_families <- physeq_family %>%
  group_by(Family) %>%
  summarise(Abundance = sum(Abundance)) %>%
  slice_max(order_by = Abundance, n = 15) %>%
  pull(Family)

# Assign "Other" to non-top families
physeq_family <- physeq_family %>%
  mutate(Family = if_else(Family %in% top_families, Family, "Other"))

# Split strain_type_timepoint into strain_type and timepoint for faceting
physeq_family <- physeq_family %>%
  separate(Sample, into = c("strain", "type", "timepoint"), sep = "_") %>%
  mutate(strain_type = paste(strain, type, sep = "_")) #This gives you a strain_type column (e.g., "F003_anemone") and a timepoint column (e.g., "T1").

df_pie <- physeq_family %>%
  group_by(strain_type, timepoint) %>%
  mutate(Abundance = Abundance / sum(Abundance)) %>%
  ungroup()

#Set Factor Levels for Desired Stacking Order
main_families <- setdiff(unique(df_pie$Family), "Other")
ordered_families <- c(sort(main_families), "Other") # A-Z, then Other
df_pie$Family <- factor(df_pie$Family, levels = ordered_families)

# Your own palette for the main families (length = number of main families)
my_palette <- c("#F2A8C5", "#CFA8EB", "#AB8ADC", "#B588C4", "#9199D8",
                "#6DB9D3", "#6C9FCB", "#9BD4C9", "#B8E192", "#A2E2EE",
                "#EEC3DE", "#E5B380", "#FFEC81", "#F08383", "#BD6868")
my_palette <- rev(my_palette)

# Combine with "Other" color at the start
fam_colors <- c(my_palette, "#CFCFCF") 

#### Plot pie charts ####
pies <- ggplot(df_pie, aes(x = "", y = Abundance, fill = Family)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y") +
        facet_grid(strain_type ~ timepoint) + 
        scale_fill_manual(values = fam_colors) + 
        theme_void()
pies

#### Stacked Bar Plots: Each Replicate Individually ####
# Agglomerate to Family level and transform to relative abundance (per replicate)
physeq_family_bar <- physeq_filtered %>%
  tax_glom("Family") %>%
  transform_sample_counts(function(x) x / sum(x))

# Melt to long format
df_bar <- psmelt(physeq_family_bar)

# Assign top 15 families, rest to "Other"
df_bar <- df_bar %>%
  mutate(Family = if_else(Family %in% top_families, Family, "Other"))

#### Stacked bar plot ####
# Plot stacked bar plots, one bar per replicate
df_bar$strain_type <- factor(df_bar$strain_type, levels = c("F003_anemone", "H2_anemone", "F003_seawater", "H2_seawater")) # adjust as needed
df_bar$timepoint <- factor(df_bar$timepoint, levels = c("t1", "t2"))

#Set Factor Levels for Desired Stacking Order
main_families <- setdiff(unique(df_bar$Family), "Other")
ordered_families <- c(sort(main_families), "Other") # A-Z, then Other
df_bar$Family <- factor(df_bar$Family, levels = ordered_families)

# Your own palette for the main families (length = number of main families)
my_palette <- c("#F2A8C5", "#CFA8EB", "#AB8ADC", "#B588C4", "#9199D8",
                "#6DB9D3", "#6C9FCB", "#9BD4C9", "#B8E192", "#A2E2EE",
                "#EEC3DE", "#E5B380", "#FFEC81", "#F08383", "#BD6868")
my_palette <- rev(my_palette)

# Combine with "Other" color at the start
fam_colors <- c(my_palette, "#CFCFCF")  

barplot <- ggplot(df_bar, aes(x = replicate, y = Abundance, fill = Family)) +
  geom_bar(stat = "identity") +
  facet_wrap(. ~ strain_type + timepoint, scales = "free_x", nrow = 1) +
  scale_fill_manual(values = fam_colors) +
  scale_x_continuous(limits = c(0.5, 10.5), breaks = seq(1, 10, by = 1)) +
  labs(title = "Relative Abundance per Replicate",
       x = "Replicate",
       y = "Relative Abundance") +
  theme_bw()
barplot

barplot_mod <- barplot + theme (plot.title = element_text(hjust = 0.5), 
                                legend.position= "right", 
                                line = element_line(linewidth = 0.8),
                                axis.line = element_line(colour = "black"),
                                axis.ticks = element_line(colour = "black"),
                                axis.ticks.length = unit(0.2 , "cm"),
                                axis.text.x = element_text(color = "black", size = 13),
                                axis.text.y = element_text(hjust = 0, color = "black",size = 13),
                                text = element_text(size = 13, colour = "black"),
                                panel.grid.major = element_blank(),
                                panel.grid.minor = element_blank(),
                                panel.background = element_blank(),
                                strip.background = element_rect(fill = "white", color = "black"), 
                                strip.placement = "top", # Place strip labels outside
                                panel.spacing = unit(1, "lines"), # Adjust spacing between facets
                                strip.text.x = element_text(color = "black", size = 12, hjust = 0.5, vjust = 0.5, face = "bold"))
barplot_mod

#### Make relative abundance table of top15 most abundant families for Supplement ####
# Convert relative abundance to percent
df_bar$Abundance_percent <- df_bar$Abundance * 100
df_bar <- df_bar %>%
  mutate(replicate = as.character(replicate))

df_bar <- df_bar %>%
  group_by(Sample, strain, replicate, strain_type, timepoint, Family) %>%
  summarise(Abundance_percent = sum(Abundance_percent, na.rm = TRUE), .groups = "drop")

# Wide format: one row per replicate, columns for families
individual_table <- df_bar %>%
  select(Sample, strain, replicate, strain_type, timepoint, Family, Abundance_percent) %>%
  pivot_wider(
    names_from = Family,
    values_from = Abundance_percent,
    values_fill = list(Abundance_percent = 0)
  )

# Summarize to get the mean percent abundance for each group (e.g., strain_type and timepoint)
average_table <- df_bar %>%
  group_by(strain, strain_type, timepoint, Family) %>%
  summarise(mean_abundance_percent = mean(Abundance_percent, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = Family,
    values_from = mean_abundance_percent,
    values_fill = list(mean_abundance_percent = 0)
  )

# write as Excel tables 
write_xlsx(individual_table, "Family_RelAbundance_Replicates_F003_H2.xlsx")
write_xlsx(average_table, "Family_RelAbundance_Average_F003_H2.xlsx")

#### Calculate diversity metrics ####

#### Alpha diversity without seawater ####
# Calculate alpha diversity and add sample IDs as a column
alpha_df <- estimate_richness(physeq_filtered, measures = c("Observed", "Shannon")) %>%
  rownames_to_column("SampleID")

# Convert sample_data to a data frame and add sample IDs as a column
metadata_df <- data.frame(sample_data(physeq_filtered)) %>%
  rownames_to_column("SampleID")

# Calculate Simpson evenness for each sample
simpson_evenness <- evenness(physeq_filtered, index = "simpson")
str(simpson_evenness)
simpson_evenness_df <- data.frame(
  SampleID = rownames(simpson_evenness),
  Simpson_evenness = simpson_evenness[, 1])

# Join alpha diversity with metadata
alpha_df <- left_join(alpha_df, metadata_df, by = "SampleID")
# Joing simpson evenness with alpha_df
alpha_df <- left_join(alpha_df, simpson_evenness_df, by = "SampleID")

# Reshape for plotting
alpha_long <- alpha_df %>%
  pivot_longer(cols = c("Observed", "Shannon", "Simpson_evenness"), names_to = "Metric", values_to = "Value")

# Remove seawater samples for plotting
alpha_df_no_seawater <- alpha_df %>%
  filter(type != "seawater")

# Boxplots faceted by timepoint and metric
strain_colors <- c("F003" = "#B772B3","H2" = "#FFB94E")

alpha_div <- ggplot(alpha_long, aes(x = strain, y = Value, fill = strain)) +
             geom_boxplot(width = 0.4, staplewidth = 0.3, position = position_dodge(width = 0.8), key_glyph = "rect") +  
             #geom_jitter(size = 1, width = 0.2, alpha = 0.5, key_glyph = "rect") +
             facet_wrap(. ~ Metric, scales = "free_y", nrow = 1) +
             scale_fill_manual(values = strain_colors) + # Custom fill colors for boxplots
             theme_bw()
alpha_div

alpha_div_mod <- alpha_div + theme (plot.title = element_text(hjust = 0.5), 
                                    legend.position= "bottom", 
                                    line = element_line(linewidth = 0.8),
                                    axis.line = element_line(colour = "black"),
                                    axis.ticks = element_line(colour = "black"),
                                    axis.ticks.length = unit(0.2 , "cm"),
                                    axis.text.x = element_text(color = "black", size = 13),
                                    axis.text.y = element_text(hjust = 0, color = "black",size = 13),
                                    text = element_text(size = 13, colour = "black"),
                                    panel.grid.major = element_blank(),
                                    panel.grid.minor = element_blank(),
                                    panel.background = element_blank(),
                                    strip.background = element_rect(fill = "white", color = "black"), 
                                    strip.placement = "top", # Place strip labels outside
                                    panel.spacing = unit(1, "lines"), # Adjust spacing between facets
                                    strip.text.x = element_text(color = "black", size = 12, hjust = 0.5, vjust = 0.5, face = "bold"))
alpha_div_mod

#### Some quick statistical tests ####
wilcox.test(Observed ~ strain, data = alpha_df_no_seawater)
wilcox.test(Shannon ~ strain, data = alpha_df_no_seawater)
wilcox.test(Simpson_evenness ~ strain, data = alpha_df_no_seawater)

#### Create Supplement table with filtered and unfiltered ASV counts ####
# Calculate observed ASVs (richness) on unfiltered data
alpha_unfiltered <- estimate_richness(physeq, measures = c("Observed")) %>%
  rownames_to_column("SampleID") %>%
  rename(Observed_ASVs_unfiltered = Observed)

alpha_unfiltered <- left_join(alpha_unfiltered, metadata_df, by = "SampleID")

alpha_unfiltered_no_seawater <- alpha_unfiltered %>%
  filter(type != "seawater")

supplement_table <- alpha_df_no_seawater %>%
  left_join(alpha_unfiltered_no_seawater, by = "SampleID") %>%
  select(
    SampleID,
    Observed_ASVs_unfiltered,
    Observed,
    Shannon,
    Simpson_evenness,
    everything())  # Add metadata columns after main metrics

# write as Excel table 
write_xlsx(supplement_table, "Alpha_diversity_F003_H2.xlsx")


#### Alpha diversity with seawater ####
# Create a new grouping variable in your metadata - group = strains split by timepoint, seawater split by timepoint (not by strain)
metadata_df$group <- ifelse(
  metadata_df$type == "seawater",
  paste("seawater", metadata_df$timepoint, sep = "_"),
  paste(metadata_df$strain, metadata_df$timepoint, sep = "_")
)
# Create a grouping variable - group2 = strains and seawater not split by timepoint (F003, H2, seawater)
metadata_df <- metadata_df %>%
  mutate(
    group2 = case_when(
      type == "seawater" ~ "seawater",
      TRUE ~ as.character(strain)  # For anemones, use strain
    )
  )

# Join alpha diversity with updated metadata
alpha_df <- left_join(alpha_df, metadata_df, by = "SampleID")

# Reshape for plotting
alpha_long <- alpha_df %>%
  pivot_longer(cols = c("Observed", "Shannon", "Simpson_evenness"), names_to = "Metric", values_to = "Value")

# Boxplots faceted by timepoint and metric
group2_colors <- c("F003" = "#B772B3","H2" = "#FFB94E", "seawater" = "#6AD4F8")

alpha_div_sw <- ggplot(alpha_long, aes(x = group2, y = Value, fill = group2)) +
             geom_boxplot(width = 0.4, staplewidth = 0.3, position = position_dodge(width = 0.8), key_glyph = "rect") +  
             #geom_jitter(size = 1, width = 0.2, alpha = 0.5, key_glyph = "rect") +
             facet_wrap(. ~ Metric, scales = "free_y", nrow = 1) +
             scale_fill_manual(values = group2_colors) + # Custom fill colors for boxplots
             theme_bw()
alpha_div_sw

alpha_div_sw_mod <- alpha_div_sw + theme (plot.title = element_text(hjust = 0.5), 
                                    legend.position= "bottom", 
                                    line = element_line(linewidth = 0.8),
                                    axis.line = element_line(colour = "black"),
                                    axis.ticks = element_line(colour = "black"),
                                    axis.ticks.length = unit(0.2 , "cm"),
                                    axis.text.x = element_text(color = "black", size = 13),
                                    axis.text.y = element_text(hjust = 0, color = "black",size = 13),
                                    text = element_text(size = 13, colour = "black"),
                                    panel.grid.major = element_blank(),
                                    panel.grid.minor = element_blank(),
                                    panel.background = element_blank(),
                                    strip.background = element_rect(fill = "white", color = "black"), 
                                    strip.placement = "top", # Place strip labels outside
                                    panel.spacing = unit(1, "lines"), # Adjust spacing between facets
                                    strip.text.x = element_text(color = "black", size = 12, hjust = 0.5, vjust = 0.5, face = "bold"))
alpha_div_sw_mod

#### Beta diversity without seawater ####

# PCoA with Bray-Curtis without seawater
# Remove seawater samples for plotting
physeq_filtered_noSW <- subset_samples(physeq_filtered, type != "seawater")

#PCoA
ord_bc <- ordinate(physeq_filtered_noSW, method = "PCoA", distance = "bray")

# Combine strain and timepoint into a new column
sample_data(physeq_filtered_noSW)$strain_timepoint <- paste(
  sample_data(physeq_filtered_noSW)$strain, 
  sample_data(physeq_filtered_noSW)$timepoint, 
  sep = "_"
)

# Plot without seawater
strain_colors_time <- c("F003_t1" = "#B772B3", "F003_t2" = "#D8B3D6" , "H2_t1" = "#FFB94E", "H2_t2" = "#FFD18B")

beta_div <- plot_ordination(physeq_filtered_noSW, ord_bc, color = "strain_timepoint") +
            geom_point(size = 3) +  
            stat_ellipse(aes(group = strain_timepoint), linetype = 2) + # Optional
            scale_color_manual(values = strain_colors_time) +
            theme_bw()
beta_div

beta_div_mod <- beta_div + theme (plot.title = element_text(hjust = 0.5), 
                                    legend.position= "bottom", 
                                    line = element_line(linewidth = 0.8),
                                    axis.line = element_line(colour = "black"),
                                    axis.ticks = element_line(colour = "black"),
                                    axis.ticks.length = unit(0.2 , "cm"),
                                    axis.text.x = element_text(color = "black", size = 13),
                                    axis.text.y = element_text(hjust = 0, color = "black",size = 13),
                                    text = element_text(size = 13, colour = "black"),
                                    panel.grid.major = element_blank(),
                                    panel.grid.minor = element_blank(),
                                    panel.background = element_blank(),
                                    strip.background = element_rect(fill = "white", color = "black"), 
                                    strip.placement = "top", # Place strip labels outside
                                    panel.spacing = unit(1, "lines"), # Adjust spacing between facets
                                    strip.text.x = element_text(color = "black", size = 12, hjust = 0.5, vjust = 0.5, face = "bold"))
beta_div_mod

# Plot without seawater and without timepoints
beta_div_strain <- plot_ordination(physeq_filtered_noSW, ord_bc, color = "strain") +
                   geom_point(size = 3) +  
                   stat_ellipse(aes(group = strain), linetype = 2) + # Optional
                   scale_color_manual(values = strain_colors) +
                   theme_bw()
beta_div_strain

beta_div_strain_mod <- beta_div_strain + theme (plot.title = element_text(hjust = 0.5), 
                                    legend.position= "bottom", 
                                    line = element_line(linewidth = 0.8),
                                    axis.line = element_line(colour = "black"),
                                    axis.ticks = element_line(colour = "black"),
                                    axis.ticks.length = unit(0.2 , "cm"),
                                    axis.text.x = element_text(color = "black", size = 13),
                                    axis.text.y = element_text(hjust = 0, color = "black",size = 13),
                                    text = element_text(size = 13, colour = "black"),
                                    panel.grid.major = element_blank(),
                                    panel.grid.minor = element_blank(),
                                    panel.background = element_blank(),
                                    strip.background = element_rect(fill = "white", color = "black"), 
                                    strip.placement = "top", # Place strip labels outside
                                    panel.spacing = unit(1, "lines"), # Adjust spacing between facets
                                    strip.text.x = element_text(color = "black", size = 12, hjust = 0.5, vjust = 0.5, face = "bold"))
beta_div_strain_mod


#### Beta Diversity with seawater ####
# PCoA with Bray-Curtis with seawater (strains and seawater split by timepoints but seawater NOT split by strains)
# Access sample data
sd <- sample_data(physeq_filtered)
# Create a new variable 'group'
sd$group <- ifelse(
  sd$type == "seawater",
  paste("seawater", sd$timepoint, sep = "_"),
  paste(sd$strain, sd$timepoint, sep = "_")
)
# For ellipses: just strain for anemone, "seawater" for seawater
sd$group_ellipse <- ifelse(
  sd$type == "seawater",
  "seawater",
  as.character(sd$strain)
)

# Assign back to the phyloseq object
sample_data(physeq_filtered)$group <- sd$group
sample_data(physeq_filtered)$group_ellipse <- sd$group_ellipse

#PCoA
ord_bc_sw <- ordinate(physeq_filtered, method = "PCoA", distance = "bray")

# Plot with seawater
group_colors <- c("F003_t1" = "#B772B3", "F003_t2" = "#D8B3D6" , "H2_t1" = "#FFB94E", "H2_t2" = "#FFD18B", "seawater_t1" = "#6AD4F8", "seawater_t2" = "#C4EEFC" )

beta_div_sw <- plot_ordination(physeq_filtered, ord_bc_sw, color = "group") +
            geom_point(size = 2) +  
            stat_ellipse(aes(group = group_ellipse, color = group_ellipse), linetype = 2) + # Optional
            scale_color_manual(values = group_colors) +
            theme_bw()
beta_div_sw

beta_div_sw_mod <- beta_div_sw + theme (plot.title = element_text(hjust = 0.5), 
                                    legend.position= "bottom", 
                                    line = element_line(linewidth = 0.8),
                                    axis.line = element_line(colour = "black"),
                                    axis.ticks = element_line(colour = "black"),
                                    axis.ticks.length = unit(0.2 , "cm"),
                                    axis.text.x = element_text(color = "black", size = 13),
                                    axis.text.y = element_text(hjust = 0, color = "black",size = 13),
                                    text = element_text(size = 13, colour = "black"),
                                    panel.grid.major = element_blank(),
                                    panel.grid.minor = element_blank(),
                                    panel.background = element_blank(),
                                    strip.background = element_rect(fill = "white", color = "black"), 
                                    strip.placement = "top", # Place strip labels outside
                                    panel.spacing = unit(1, "lines"), # Adjust spacing between facets
                                    strip.text.x = element_text(color = "black", size = 12, hjust = 0.5, vjust = 0.5, face = "bold"))
beta_div_sw_mod


#### Test for significant differences in community composition using PERMANOVA ####
# Calculate Bray-Curtis distance matrix from filtered phyloseq object
bray_dist <- phyloseq::distance(physeq_filtered_noSW, method = "bray")
# Extract sample data as a data frame
meta <- data.frame(sample_data(physeq_filtered_noSW))
# Use a combined group variable for strain - timepoint differences 
meta$group <- paste(meta$strain, meta$timepoint, sep = "_")
# Use the Bray-Curtis distance and grouping variable to run PERMANOVA
# For differences by strain
adonis_result <- adonis2(bray_dist ~ strain, data = meta, permutations = 999)
# For differences by strain and timepoint (if desired)
adonis_result2 <- adonis2(bray_dist ~ strain * timepoint, data = meta, permutations = 999)

# View results
print(adonis_result)
print(adonis_result2)

#### Make pairwise PERMANOVA comparisons between F003 and H2 over time ####
# Pairwise PERMANOVA for 'strain'
pairwise_strain <- pairwiseAdonis::pairwise.adonis2(
  bray_dist ~ strain,
  data = meta,
  p.adjust.m = "BH" # Benjamini-Hochberg adjustment
)
print(pairwise_strain)

# Pairwise PERMANOVA for 'strain_timepoint'
pairwise_strain_time <- pairwiseAdonis::pairwise.adonis2(
  bray_dist ~ strain_timepoint,
  data = meta,
  p.adjust.m = "BH" # Benjamini-Hochberg adjustment
)
print(pairwise_strain_time)

##### Venn diagrams ####
##### Venn diagram without seawater (with and without timepoint splitting) #####
physeq_filtered_noSW <- subset_samples(physeq_filtered, type != "seawater")

# Ensure F003 is first, H2 is second
sample_data(physeq_filtered_noSW)$strain <- factor(sample_data(physeq_filtered_noSW)$strain, levels = c("F003", "H2"))

venn_strain <- ggvenn_pq(physeq = physeq_filtered_noSW, fact = "strain") + 
               coord_flip()
venn_strain

venn_timepoint <- ggvenn_pq(physeq = physeq_filtered_noSW, fact = "strain", split_by = "timepoint")
venn_timepoint

##### Venn diagram with seawater (with and without timepoint splitting) #####
# create new variable in sample data 
tmp_meta <- data.frame(sample_data(physeq_filtered))
tmp_meta$venn_group <- ifelse(tmp_meta$type == "seawater", "seawater", as.character(tmp_meta$strain))
sample_data(physeq_filtered)$venn_group <- tmp_meta$venn_group

venn_sw <- ggvenn_pq(physeq = physeq_filtered, fact = "venn_group")
venn_sw

venn_sw_timepoint <- ggvenn_pq(physeq = physeq_filtered, fact = "venn_group", split_by = "timepoint")
venn_sw_timepoint

##### Comparative analysis on family and genus level #####
# Aggregate to Family level
physeq_family <- tax_glom(physeq_filtered_noSW, taxrank = "Family", NArm = FALSE)
# Remove taxa where Family is NA or "Unclassified"
physeq_family_classified <- subset_taxa(physeq_family, !is.na(Family) & Family != "Unclassified" & Family != "")

# Aggregate to Genus level
physeq_genus <- tax_glom(physeq_filtered_noSW, taxrank = "Genus", NArm = FALSE)
# Remove taxa where Genus is NA or "Unclassified"
physeq_genus_classified <- subset_taxa(physeq_genus, !is.na(Genus) & Genus != "Unclassified" & Genus != "")

# Family-Level Comparison
# Strain comparison
venn_family <- ggvenn_pq(physeq = physeq_family_classified, fact = "strain", taxonomic_rank = "Family") + 
  coord_flip()
venn_family
# Split by timepoint
venn_family_time <- ggvenn_pq(physeq = physeq_family_classified, fact = "strain", taxonomic_rank = "Family", split_by = "timepoint")
venn_family_time
# Genus-Level Comparison
# Strain comparison
venn_genus <- ggvenn_pq(physeq = physeq_genus_classified, fact = "strain", taxonomic_rank = "Genus") + 
  coord_flip()
venn_genus

# Split by timepoint
venn_genus_time <- ggvenn_pq(physeq = physeq_genus_classified, taxonomic_rank = "Genus", fact = "strain", split_by = "timepoint")
venn_genus_time

#### Create Supplement filtered ASV Table with a sequential ASV number (by total count), Original_ASV_ID, sample columns, total_count across samples and taxonomy information ####
# 1. Extract the ASV table and compute total counts
asv_table <- as.data.frame(otu_table(physeq_filtered))
asv_table$Total_Count <- rowSums(asv_table)

# 2. Order ASVs by total abundance (descending)
asv_table <- asv_table[order(-asv_table$Total_Count), ]

# 3. Add ASV_number column with leading zeros
n_asvs <- nrow(asv_table)
asv_table$ASV_number <- sprintf("ASV%0*d", nchar(n_asvs), seq_len(n_asvs))

# 4. Add Original_ASV_ID as a column
asv_table$Original_ASV_ID <- rownames(asv_table)

# 5. Reorder columns: ASV_number, Original_ASV_ID, sample columns, Total_Count
sample_cols <- sample_names(physeq_filtered)
asv_table <- asv_table[, c("ASV_number", "Original_ASV_ID", sample_cols, "Total_Count")]

# 6. Add taxonomy information
# asv_table already has ASV_number and Original_ASV_ID columns
taxonomy_df <- as.data.frame(tax_table(physeq_filtered))
taxonomy_df$Original_ASV_ID <- rownames(taxonomy_df)

# Merge by Original_ASV_ID
final_table <- left_join(asv_table, taxonomy_df, by = "Original_ASV_ID")

# 7. Write to file
write_xlsx(final_table, "ASVTable_F003_H2.xlsx")

#### Create Supplement unfiltered ASV Table with a sequential ASV number (by total count), Original_ASV_ID, sample columns, total_count across samples and taxonomy information ####
# 1. Extract the ASV table and compute total counts
asv_table_uf <- as.data.frame(otu_table(physeq))
asv_table_uf$Total_Count <- rowSums(asv_table_uf)

# 2. Order ASVs by total abundance (descending)
asv_table_uf <- asv_table_uf[order(-asv_table_uf$Total_Count), ]

# 3. Add ASV_number column with leading zeros
n_asvs <- nrow(asv_table_uf)
asv_table_uf$ASV_number <- sprintf("ASV%0*d", nchar(n_asvs), seq_len(n_asvs))

# 4. Add Original_ASV_ID as a column
asv_table_uf$Original_ASV_ID <- rownames(asv_table_uf)

# 5. Reorder columns: ASV_number, Original_ASV_ID, sample columns, Total_Count
sample_cols <- sample_names(physeq)
asv_table_uf <- asv_table_uf[, c("ASV_number", "Original_ASV_ID", sample_cols, "Total_Count")]

# 6. Add taxonomy information
# asv_table already has ASV_number and Original_ASV_ID columns
taxonomy_df_uf <- as.data.frame(tax_table(physeq))
taxonomy_df_uf$Original_ASV_ID <- rownames(taxonomy_df_uf)

# Merge by Original_ASV_ID
final_table_uf <- left_join(asv_table_uf, taxonomy_df_uf, by = "Original_ASV_ID")

# 7. Write to file
write_xlsx(final_table_uf, "ASVTable_F003_H2_unfiltered.xlsx")
