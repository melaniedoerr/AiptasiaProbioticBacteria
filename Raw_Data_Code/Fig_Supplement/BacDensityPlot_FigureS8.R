# Load packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(rstudioapi)
library(RColorBrewer)
library(CBASSED50)
library(grid)
library(httpgd)
library(ggtext)
library(ggpattern)
library(cowplot)
library(rstatix)
library(ggpubr)
library(ggh4x)
#library(drc)

# read input file
setwd("path/to/directory")
Input<- readxl::read_excel("Raw_Data_Code/Fig_Supplement/AllPreScreening_PAM+Score_F003H2_FigureS8_Input.xlsx")

hgd()

#### Prepare dataframe ####
Input$Timepoint <- gsub("5", "5 h", Input$Timepoint)
Input$Timepoint <- gsub("24", "24 h", Input$Timepoint)
Input$Timepoint <- gsub("48", "48 h", Input$Timepoint)

Input <- Input %>%
  mutate(
    PAM = as.numeric(as.character(PAM)),
    Score = as.numeric(as.character(Score)),
    Timepoint = factor(Timepoint, levels = c("5 h", "24 h", "48 h")),
    Concentration = factor(Concentration, levels = c("10^5", "10^6", "10^7"))
  )

Input_F003 <- filter(Input, Strain == "F003")
Input_H2 <- filter(Input, Strain == "H2")

#Define the desired labels with expressions
concentration_labels <- c(
  "10^5" = expression("10" * phantom(.)^5),
  "10^6" = expression("10" * phantom(.)^6),
  "10^7" = expression("10" * phantom(.)^7))

#### Raw Data distribution: Boxplots Fv/Fm against bac. concentration and Score against bac. concentration per timepoint! ####
## Arrange data so that I can plot PAM-T1, Score-T1, PAM-T2 and Score-T2, PAM-T3, Score-T3
long_F003 <- Input_F003 %>%
  pivot_longer(cols = c(PAM, Score), names_to = "Variable", values_to = "Value") %>%
  mutate(Panel = paste(Variable, Timepoint, sep = "-"))

# For H2
long_H2 <- Input_H2 %>%
  pivot_longer(cols = c(PAM, Score), names_to = "Variable", values_to = "Value") %>%
  mutate(Panel = paste(Variable, Timepoint, sep = "-"))

# For F003 and H2 combined
long_df <- Input %>%
  pivot_longer(cols = c(PAM, Score), names_to = "Variable", values_to = "Value") %>%
  mutate(Panel = paste(Variable, Timepoint, sep = "-"))

# Set these according to your data range!
pam_limits <- c(0, 0.73)
pam_breaks <- seq(0, 0.7, by = 0.1)

score_limits <- c(0, 1.24)
score_breaks <- seq(0, 1.2, by = 0.2)

conc_colors <- c("10^5" = "#E6DAC6","10^6" = "#D6AB7C","10^7" = "#95632F")

#### Separate plots ####
#### F003 ####
#F003 PAM-24h
pam_24h_F003 <- ggplot(filter(long_F003, Panel == "PAM-24 h"),
                      aes(x = Concentration, y = Value, fill = Concentration)) +
  geom_boxplot(outlier.size = 1, width = 0.4, staplewidth = 0.3, position = position_dodge(width = 0.8), key_glyph = "rect") +  #outlier.shape = NA,
  #geom_jitter(aes(color = Temperature), width = 0.2, size = 1.2, alpha = 0.7, show.legend = FALSE) +
  scale_y_continuous(limits = pam_limits, breaks = pam_breaks) +
  scale_x_discrete(labels = concentration_labels) +
  scale_fill_manual(values = conc_colors, guide = guide_legend(nrow = 1), labels = concentration_labels) +     # Custom fill colors for boxplots
  labs(x = "Bacterial concentration [cells/mL]", y = expression("Photosynthetic efficiency (F"[v]*"/F"[m]*")"), fill = "Bacterial concentration [cells/mL]",) +
  theme (plot.title = element_text(hjust = 0.5), 
           legend.position= "bottom", 
           line = element_line(linewidth = 0.8),
           axis.line = element_line(colour = "black"),
           axis.ticks = element_line(colour = "black"),
           axis.ticks.length = unit(0.2 , "cm"),
           axis.text.x = element_text(color = "black", size = 13),
           axis.text.y = element_text(hjust = 0, size = 13),
           text = element_text(size = 13, colour = "black"),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.background = element_blank(),
           strip.background = element_rect(fill = "white", color = "black"), 
           strip.placement = "top", # Place strip labels outside
           panel.spacing = unit(1, "lines"), # Adjust spacing between facets
           strip.text.x = element_text(color = "black", size = 12, hjust = 0.5, vjust = 0.5, face = "bold"))
pam_24h_F003

# Repeat for Score-T1, PAM-T2, Score-T2, and for H2
score_24h_F003 <- ggplot(filter(long_F003, Panel == "Score-24 h"),
                         aes(x = Concentration, y = Value, fill = Concentration)) +
  geom_boxplot(outlier.size = 1, width = 0.4, staplewidth = 0.3, position = position_dodge(width = 0.8), key_glyph = "rect") +  #outlier.shape = NA,
  #geom_jitter(aes(color = Temperature), width = 0.2, size = 1.2, alpha = 0.7, show.legend = FALSE) +
  scale_y_continuous(limits = score_limits, breaks = score_breaks) +
  scale_x_discrete(labels = concentration_labels) +
  scale_fill_manual(values = conc_colors, guide = guide_legend(nrow = 1)) +     # Custom fill colors for boxplots
  labs(x = "Bacterial concentration [cells/mL]", y = "Behavioral indicator (BPS)", fill = "Bacterial concentration [cells/mL]") +
  theme (plot.title = element_text(hjust = 0.5), 
           legend.position= "bottom", 
           line = element_line(linewidth = 0.8),
           axis.line = element_line(colour = "black"),
           axis.ticks = element_line(colour = "black"),
           axis.ticks.length = unit(0.2 , "cm"),
           axis.text.x = element_text(color = "black", size = 13),
           axis.text.y = element_text(hjust = 0, size = 13),
           text = element_text(size = 13, colour = "black"),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.background = element_blank(),
           strip.background = element_rect(fill = "white", color = "black"), 
           strip.placement = "top", # Place strip labels outside
           panel.spacing = unit(1, "lines"), # Adjust spacing between facets
           strip.text.x = element_text(color = "black", size = 12, hjust = 0.5, vjust = 0.5, face = "bold"))
score_24h_F003

#H2 PAM-24h
pam_24h_H2 <- ggplot(filter(long_H2, Panel == "PAM-24 h"),
                      aes(x = Concentration, y = Value, fill = Concentration)) +
  geom_boxplot(outlier.size = 1, width = 0.4, staplewidth = 0.3, position = position_dodge(width = 0.8), key_glyph = "rect") +  #outlier.shape = NA,
  #geom_jitter(aes(color = Temperature), width = 0.2, size = 1.2, alpha = 0.7, show.legend = FALSE) +
  scale_y_continuous(limits = pam_limits, breaks = pam_breaks) +
  scale_x_discrete(labels = concentration_labels) +
  scale_fill_manual(values = conc_colors, guide = guide_legend(nrow = 1), labels = concentration_labels) +     # Custom fill colors for boxplots
  labs(x = "Bacterial concentration [cells/mL]", y = expression("Photosynthetic efficiency (F"[v]*"/F"[m]*")"), fill = "Bacterial concentration [cells/mL]",) +
  theme (plot.title = element_text(hjust = 0.5), 
           legend.position= "bottom", 
           line = element_line(linewidth = 0.8),
           axis.line = element_line(colour = "black"),
           axis.ticks = element_line(colour = "black"),
           axis.ticks.length = unit(0.2 , "cm"),
           axis.text.x = element_text(color = "black", size = 13),
           axis.text.y = element_text(hjust = 0, size = 13),
           text = element_text(size = 13, colour = "black"),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.background = element_blank(),
           strip.background = element_rect(fill = "white", color = "black"), 
           strip.placement = "top", # Place strip labels outside
           panel.spacing = unit(1, "lines"), # Adjust spacing between facets
           strip.text.x = element_text(color = "black", size = 12, hjust = 0.5, vjust = 0.5, face = "bold"))
pam_24h_H2

#H2 Score-24h
score_24h_H2 <- ggplot(filter(long_H2, Panel == "Score-24 h"),
                         aes(x = Concentration, y = Value, fill = Concentration)) +
  geom_boxplot(outlier.size = 1, width = 0.4, staplewidth = 0.3, position = position_dodge(width = 0.8), key_glyph = "rect") +  #outlier.shape = NA,
  #geom_jitter(aes(color = Temperature), width = 0.2, size = 1.2, alpha = 0.7, show.legend = FALSE) +
  scale_y_continuous(limits = score_limits, breaks = score_breaks) +
  scale_x_discrete(labels = concentration_labels) +
  scale_fill_manual(values = conc_colors, guide = guide_legend(nrow = 1)) +     # Custom fill colors for boxplots
  labs(x = "Bacterial concentration [cells/mL]", y = "Behavioral indicator (BPS)", fill = "Bacterial concentration [cells/mL]") +
  theme (plot.title = element_text(hjust = 0.5), 
           legend.position= "bottom", 
           line = element_line(linewidth = 0.8),
           axis.line = element_line(colour = "black"),
           axis.ticks = element_line(colour = "black"),
           axis.ticks.length = unit(0.2 , "cm"),
           axis.text.x = element_text(color = "black", size = 13),
           axis.text.y = element_text(hjust = 0, size = 13),
           text = element_text(size = 13, colour = "black"),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.background = element_blank(),
           strip.background = element_rect(fill = "white", color = "black"), 
           strip.placement = "top", # Place strip labels outside
           panel.spacing = unit(1, "lines"), # Adjust spacing between facets
           strip.text.x = element_text(color = "black", size = 12, hjust = 0.5, vjust = 0.5, face = "bold"))
score_24h_H2


F003_H2_24h_plot <- ggarrange(pam_24h_F003, score_24h_F003, pam_24h_H2, score_24h_H2, ncol = 4, nrow = 1, common.legend = TRUE, legend = "bottom")
F003_H2_24h_plot




#### Plot F003 and H2 together ####
long_df$pattern <- ifelse(long_df$Strain == "H2", "stripe", "none")
long_df <- drop_na(long_df)

#### 24h ####
#F003 + H2 PAM-24h
pam_24h <- ggplot(filter(long_df, Panel == "PAM-24 h"),
                  aes(x = Strain, y = Value, fill = Concentration, pattern = pattern)) +
           geom_boxplot_pattern(
              outlier.size = 1, width = 0.4, staplewidth = 0.3, position = position_dodge(width = 0.8), key_glyph = "rect",
              color = "black",        # Outline color
              pattern_fill = "white", # Background for stripes
              pattern_colour = NA,    # Stripes use conc. colors
              pattern_density = 0.2,
              pattern_spacing = 0.03) +
  #geom_jitter(aes(color = Temperature), width = 0.2, size = 1.2, alpha = 0.7, show.legend = FALSE) +
  scale_y_continuous(limits = pam_limits, breaks = pam_breaks) +
  scale_x_discrete(labels = concentration_labels) +
  scale_fill_manual(values = conc_colors, guide = guide_legend(nrow = 1), labels = concentration_labels) +
  scale_pattern_manual(values = c("none" = "none", "stripe" = "stripe")) +
  labs(x = "", y = expression("Photosynthetic efficiency (F"[v]*"/F"[m]*")"), fill = "Bacterial concentration [cells/mL]") +
  guides(pattern = "none") +
  theme (plot.title = element_text(hjust = 0.5), 
           legend.position= "bottom", 
           line = element_line(linewidth = 0.8),
           axis.line = element_line(colour = "black"),
           axis.ticks = element_line(colour = "black"),
           axis.ticks.length = unit(0.2 , "cm"),
           axis.text.x = element_text(color = "black", size = 13),
           axis.text.y = element_text(color = "black", hjust = 0, size = 13),
           text = element_text(size = 13, colour = "black"),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.background = element_blank(),
           strip.background = element_rect(fill = "white", color = "black"), 
           strip.placement = "top", # Place strip labels outside
           panel.spacing = unit(1, "lines"), # Adjust spacing between facets
           strip.text.x = element_text(color = "black", size = 12, hjust = 0.5, vjust = 0.5, face = "bold"))
pam_24h

#F003 + H2 Score-24h
score_24h <- ggplot(filter(long_df, Panel == "Score-24 h"),
                  aes(x = Strain, y = Value, fill = Concentration, pattern = pattern)) +
             geom_boxplot_pattern(
              outlier.size = 1, width = 0.4, staplewidth = 0.3, position = position_dodge(width = 0.8), key_glyph = "rect",
              color = "black",        # Outline color
              pattern_fill = "white", # Background for stripes
              pattern_colour = NA,    # Stripes use conc. colors
              pattern_density = 0.2,
              pattern_spacing = 0.03) +
  #geom_jitter(aes(color = Temperature), width = 0.2, size = 1.2, alpha = 0.7, show.legend = FALSE) +
  scale_y_continuous(limits = score_limits, breaks = score_breaks) +
  scale_x_discrete(labels = concentration_labels) +
  scale_fill_manual(values = conc_colors, guide = guide_legend(nrow = 1), labels = concentration_labels) +
  scale_pattern_manual(values = c("none" = "none", "stripe" = "stripe")) +
  labs(x = "", y = "Behavioral indicator (BPS)", fill = "Bacterial concentration [cells/mL]") +
  theme (plot.title = element_text(hjust = 0.5), 
           legend.position= "bottom", 
           line = element_line(linewidth = 0.8),
           axis.line = element_line(colour = "black"),
           axis.ticks = element_line(colour = "black"),
           axis.ticks.length = unit(0.2 , "cm"),
           axis.text.x = element_text(color = "black", size = 13),
           axis.text.y = element_text(color = "black", hjust = 0, size = 13),
           text = element_text(size = 13, colour = "black"),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.background = element_blank(),
           strip.background = element_rect(fill = "white", color = "black"), 
           strip.placement = "top", # Place strip labels outside
           panel.spacing = unit(1, "lines"), # Adjust spacing between facets
           strip.text.x = element_text(color = "black", size = 12, hjust = 0.5, vjust = 0.5, face = "bold"))
score_24h


T2_24h_plot <- ggarrange(pam_24h, score_24h, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")
T2_24h_plot

#### 5h ####
#F003 + H2 PAM-5h
pam_5h <- ggplot(filter(long_df, Panel == "PAM-5 h"),
                  aes(x = Strain, y = Value, fill = Concentration, pattern = pattern)) +
           geom_boxplot_pattern(
              outlier.size = 1, width = 0.4, staplewidth = 0.3, position = position_dodge(width = 0.8), key_glyph = "rect",
              color = "black",        # Outline color
              pattern_fill = "white", # Background for stripes
              pattern_colour = NA,    # Stripes use conc. colors
              pattern_density = 0.2,
              pattern_spacing = 0.03) +
  #geom_jitter(aes(color = Temperature), width = 0.2, size = 1.2, alpha = 0.7, show.legend = FALSE) +
  scale_y_continuous(limits = pam_limits, breaks = pam_breaks) +
  scale_x_discrete(labels = concentration_labels) +
  scale_fill_manual(values = conc_colors, guide = guide_legend(nrow = 1), labels = concentration_labels) +
  scale_pattern_manual(values = c("none" = "none", "stripe" = "stripe")) +
  labs(x = "", y = expression("Photosynthetic efficiency (F"[v]*"/F"[m]*")"), fill = "Bacterial concentration [cells/mL]") +
  guides(pattern = "none") +
  theme (plot.title = element_text(hjust = 0.5), 
           legend.position= "bottom", 
           line = element_line(linewidth = 0.8),
           axis.line = element_line(colour = "black"),
           axis.ticks = element_line(colour = "black"),
           axis.ticks.length = unit(0.2 , "cm"),
           axis.text.x = element_text(color = "black", size = 13),
           axis.text.y = element_text(color = "black", hjust = 0, size = 13),
           text = element_text(size = 13, colour = "black"),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.background = element_blank(),
           strip.background = element_rect(fill = "white", color = "black"), 
           strip.placement = "top", # Place strip labels outside
           panel.spacing = unit(1, "lines"), # Adjust spacing between facets
           strip.text.x = element_text(color = "black", size = 12, hjust = 0.5, vjust = 0.5, face = "bold"))
pam_5h

#F003 + H2 Score-5h
score_5h <- ggplot(filter(long_df, Panel == "Score-5 h"),
                  aes(x = Strain, y = Value, fill = Concentration, pattern = pattern)) +
             geom_boxplot_pattern(
              outlier.size = 1, width = 0.4, staplewidth = 0.3, position = position_dodge(width = 0.8), key_glyph = "rect",
              color = "black",        # Outline color
              pattern_fill = "white", # Background for stripes
              pattern_colour = NA,    # Stripes use conc. colors
              pattern_density = 0.2,
              pattern_spacing = 0.03) +
  #geom_jitter(aes(color = Temperature), width = 0.2, size = 1.2, alpha = 0.7, show.legend = FALSE) +
  scale_y_continuous(limits = score_limits, breaks = score_breaks) +
  scale_x_discrete(labels = concentration_labels) +
  scale_fill_manual(values = conc_colors, guide = guide_legend(nrow = 1), labels = concentration_labels) +
  scale_pattern_manual(values = c("none" = "none", "stripe" = "stripe")) +
  labs(x = "", y = "Behavioral indicator (BPS)", fill = "Bacterial concentration [cells/mL]") +
  theme (plot.title = element_text(hjust = 0.5), 
           legend.position= "bottom", 
           line = element_line(linewidth = 0.8),
           axis.line = element_line(colour = "black"),
           axis.ticks = element_line(colour = "black"),
           axis.ticks.length = unit(0.2 , "cm"),
           axis.text.x = element_text(color = "black", size = 13),
           axis.text.y = element_text(color = "black", hjust = 0, size = 13),
           text = element_text(size = 13, colour = "black"),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.background = element_blank(),
           strip.background = element_rect(fill = "white", color = "black"), 
           strip.placement = "top", # Place strip labels outside
           panel.spacing = unit(1, "lines"), # Adjust spacing between facets
           strip.text.x = element_text(color = "black", size = 12, hjust = 0.5, vjust = 0.5, face = "bold"))
score_5h

T1_5h_plot <- ggarrange(pam_5h, score_5h, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")
T1_5h_plot

#### 48h ####
#F003 + H2 PAM-48h
pam_48h <- ggplot(filter(long_df, Panel == "PAM-48 h"),
                  aes(x = Strain, y = Value, fill = Concentration, pattern = pattern)) +
           geom_boxplot_pattern(
              outlier.size = 1, width = 0.4, staplewidth = 0.3, position = position_dodge(width = 0.8), key_glyph = "rect",
              color = "black",        # Outline color
              pattern_fill = "white", # Background for stripes
              pattern_colour = NA,    # Stripes use conc. colors
              pattern_density = 0.2,
              pattern_spacing = 0.03) +
  #geom_jitter(aes(color = Temperature), width = 0.2, size = 1.2, alpha = 0.7, show.legend = FALSE) +
  scale_y_continuous(limits = pam_limits, breaks = pam_breaks) +
  scale_x_discrete(labels = concentration_labels) +
  scale_fill_manual(values = conc_colors, guide = guide_legend(nrow = 1), labels = concentration_labels) +
  scale_pattern_manual(values = c("none" = "none", "stripe" = "stripe")) +
  labs(x = "", y = expression("Photosynthetic efficiency (F"[v]*"/F"[m]*")"), fill = "Bacterial concentration [cells/mL]") +
  guides(pattern = "none") +
  theme (plot.title = element_text(hjust = 0.5), 
           legend.position= "bottom", 
           line = element_line(linewidth = 0.8),
           axis.line = element_line(colour = "black"),
           axis.ticks = element_line(colour = "black"),
           axis.ticks.length = unit(0.2 , "cm"),
           axis.text.x = element_text(color = "black", size = 13),
           axis.text.y = element_text(color = "black", hjust = 0, size = 13),
           text = element_text(size = 13, colour = "black"),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.background = element_blank(),
           strip.background = element_rect(fill = "white", color = "black"), 
           strip.placement = "top", # Place strip labels outside
           panel.spacing = unit(1, "lines"), # Adjust spacing between facets
           strip.text.x = element_text(color = "black", size = 12, hjust = 0.5, vjust = 0.5, face = "bold"))
pam_48h

#F003 + H2 Score-24h
score_48h <- ggplot(filter(long_df, Panel == "Score-48 h"),
                  aes(x = Strain, y = Value, fill = Concentration, pattern = pattern)) +
             geom_boxplot_pattern(
              outlier.size = 1, width = 0.4, staplewidth = 0.3, position = position_dodge(width = 0.8), key_glyph = "rect",
              color = "black",        # Outline color
              pattern_fill = "white", # Background for stripes
              pattern_colour = NA,    # Stripes use conc. colors
              pattern_density = 0.2,
              pattern_spacing = 0.03) +
  #geom_jitter(aes(color = Temperature), width = 0.2, size = 1.2, alpha = 0.7, show.legend = FALSE) +
  scale_y_continuous(limits = score_limits, breaks = score_breaks) +
  scale_x_discrete(labels = concentration_labels) +
  scale_fill_manual(values = conc_colors, guide = guide_legend(nrow = 1), labels = concentration_labels) +
  scale_pattern_manual(values = c("none" = "none", "stripe" = "stripe")) +
  labs(x = "", y = "Behavioral indicator (BPS)", fill = "Bacterial concentration [cells/mL]") +
  theme (plot.title = element_text(hjust = 0.5), 
           legend.position= "bottom", 
           line = element_line(linewidth = 0.8),
           axis.line = element_line(colour = "black"),
           axis.ticks = element_line(colour = "black"),
           axis.ticks.length = unit(0.2 , "cm"),
           axis.text.x = element_text(color = "black", size = 13),
           axis.text.y = element_text(color = "black", hjust = 0, size = 13),
           text = element_text(size = 13, colour = "black"),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           panel.background = element_blank(),
           strip.background = element_rect(fill = "white", color = "black"), 
           strip.placement = "top", # Place strip labels outside
           panel.spacing = unit(1, "lines"), # Adjust spacing between facets
           strip.text.x = element_text(color = "black", size = 12, hjust = 0.5, vjust = 0.5, face = "bold"))
score_48h


T3_48h_plot <- ggarrange(pam_48h, score_48h, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")
T3_48h_plot

##### Combine all plots by timepoint ####
All_combined <- ggarrange(T1_5h_plot, T2_24h_plot, T3_48h_plot, ncol = 3, nrow = 1, common.legend = TRUE, legend = "bottom")
All_combined

##### Combine all plots by metric ####
All_combined_Metric <- ggarrange(pam_5h, pam_24h, pam_48h, score_5h, score_24h, score_48h, ncol = 6, nrow = 1, common.legend = TRUE, legend = "bottom")
All_combined_Metric

##### All timepoints faceted #####
#F003 + H2 
long_df_PAM <- filter(long_df, Variable == "PAM")
long_df_Score <- filter(long_df, Variable == "Score")

#PAM
bac_dens_PAM <- ggplot(long_df_PAM, aes(x = Strain, y = Value, fill = Concentration, pattern = pattern)) +
                geom_boxplot_pattern(
                  outlier.size = 1, width = 0.4, staplewidth = 0.4, position = position_dodge(width = 0.8), key_glyph = "rect",
                  color = "black",        # Outline color
                  pattern_fill = "white", # Background for stripes
                  pattern_colour = NA,    # Stripes use conc. colors
                  pattern_density = 0.2,
                  pattern_spacing = 0.03) +
                #geom_jitter(aes(color = Temperature), width = 0.2, size = 1.2, alpha = 0.7, show.legend = FALSE) +
                scale_y_continuous(limits = pam_limits, breaks = pam_breaks) +
                scale_x_discrete(labels = concentration_labels) +
                scale_fill_manual(values = conc_colors, guide = guide_legend(nrow = 1), labels = concentration_labels) +
                scale_pattern_manual(values = c("none" = "none", "stripe" = "stripe")) +
                labs(x = "", y = expression("Photosynthetic efficiency (F"[v]*"/F"[m]*")"), fill = "Bacterial concentration [cells/mL]") +
                guides(pattern = "none") +
                facet_wrap(~ Timepoint) + 
                theme (plot.title = element_text(hjust = 0.5), 
                         legend.position= "bottom", 
                         line = element_line(linewidth = 0.8),
                         axis.line = element_line(colour = "black"),
                         axis.ticks = element_line(colour = "black"),
                         axis.ticks.length = unit(0.2 , "cm"),
                         axis.text.x = element_text(color = "black", size = 13),
                         axis.text.y = element_text(color = "black", hjust = 0, size = 13),
                         text = element_text(size = 13, colour = "black"),
                         panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         panel.background = element_blank(),
                         strip.background = element_blank(),
                         strip.placement = "top", # Place strip labels outside
                         panel.spacing = unit(1, "lines"), # Adjust spacing between facets
                         strip.text.x = element_text(color = "black", size = 12, hjust = 0.5, vjust = 0.5, face = "bold"))
bac_dens_PAM

#PAM
bac_dens_Score <- ggplot(long_df_Score, aes(x = Strain, y = Value, fill = Concentration, pattern = pattern)) +
                  geom_boxplot_pattern(
                    outlier.size = 1, width = 0.4, staplewidth = 0.4, position = position_dodge(width = 0.8), key_glyph = "rect",
                    color = "black",        # Outline color
                    pattern_fill = "white", # Background for stripes
                    pattern_colour = NA,    # Stripes use conc. colors
                    pattern_density = 0.2,
                    pattern_spacing = 0.03) +
                  #geom_jitter(aes(color = Temperature), width = 0.2, size = 1.2, alpha = 0.7, show.legend = FALSE) +
                  scale_y_continuous(limits = score_limits, breaks = score_breaks) +
                  scale_x_discrete(labels = concentration_labels) +
                  scale_fill_manual(values = conc_colors, guide = guide_legend(nrow = 1), labels = concentration_labels) +
                  scale_pattern_manual(values = c("none" = "none", "stripe" = "stripe")) +
                  labs(x = "", y = "Behavioral indicator (BPS)", fill = "Bacterial concentration [cells/mL]") +
                  guides(pattern = "none") +
                  facet_wrap(~ Timepoint) + 
                  theme (plot.title = element_text(hjust = 0.5), 
                           legend.position= "bottom", 
                           line = element_line(linewidth = 0.8),
                           axis.line = element_line(colour = "black"),
                           axis.ticks = element_line(colour = "black"),
                           axis.ticks.length = unit(0.2 , "cm"),
                           axis.text.x = element_text(color = "black", size = 13),
                           axis.text.y = element_text(color = "black", hjust = 0, size = 13),
                           text = element_text(size = 13, colour = "black"),
                           panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(),
                           panel.background = element_blank(),
                           strip.background = element_blank(), 
                           strip.placement = "top", # Place strip labels outside
                           panel.spacing = unit(1, "lines"), # Adjust spacing between facets
                           strip.text.x = element_text(color = "black", size = 12, hjust = 0.5, vjust = 0.5, face = "bold"))
bac_dens_Score


PAM_Score <- ggarrange(bac_dens_PAM, bac_dens_Score, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")
PAM_Score
