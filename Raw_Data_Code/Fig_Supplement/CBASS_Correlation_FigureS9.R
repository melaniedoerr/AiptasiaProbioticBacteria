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
Input<- readxl::read_excel("Raw_Data_Code/Fig_Supplement/AllCBASS_ScorePAM_FigureS9_Input.xlsx")

hgd()

#### Prepare dataframe ####
Input$Timepoint <- gsub("1", "T1", Input$Timepoint)
Input$Timepoint <- gsub("2", "T2", Input$Timepoint)

Input <- Input %>%
  mutate(
    PAM = as.numeric(as.character(PAM)),
    Score = as.numeric(as.character(Score)),
    Timepoint = factor(Timepoint, levels = c("T1", "T2")),
    Temperature = factor(Temperature, levels = c("30", "34", "36", "37", "39", "40"))
  )

Input_F003 <- filter(Input, Species == "F003")
Input_H2 <- filter(Input, Species == "H2")

Input_F003 <- na.omit(Input_F003)
Input_H2 <- na.omit(Input_H2)

## not needed for raw data plotting I think ##
Input_F003 <- Input_F003 %>%
  drop_na(PAM, Score)
Input_H2 <- Input_H2 %>%
  drop_na(PAM, Score)

#### Check for normally distributed data ####
## F003
# For PAM
pam_results_F003 <- Input_F003 %>%
  group_by(Number) %>%
  shapiro_test(PAM)
# For Score
score_results_F003 <- Input_F003 %>%
  group_by(Number) %>%
  filter(n() >= 5) %>%
  shapiro_test(Score)

## H2
# For PAM
pam_results_H2 <- Input_H2 %>%
  group_by(Number) %>%
  shapiro_test(PAM)
# For Score
score_results_H2 <- Input_H2 %>%
  group_by(Number) %>%
  filter(n() >= 5) %>%
  shapiro_test(Score)

## Histogram observations ##
# For PAM F003
histogram_F003 <- gghistogram(Input_F003, "PAM", facet.by = "Number", bins = 5)
histogram_F003
# For PAM H2
histogram_H2 <-gghistogram(Input_H2, "PAM", facet.by = "Number", bins = 5)
histogram_H2

## Data is not normally distributed! ##

#### Raw Data distribution: Boxplots Fv/Fm against temperature and Score against temperature per timepoint! Plot nested ####
## Arrange data so that I can plot PAM-T1, Score-T1, PAM-T2 and Score-T2 
long_F003 <- Input_F003 %>%
  pivot_longer(cols = c(PAM, Score), names_to = "Variable", values_to = "Value") %>%
  mutate(Panel = paste(Variable, Timepoint, sep = "-"))

# For H2
long_H2 <- Input_H2 %>%
  pivot_longer(cols = c(PAM, Score), names_to = "Variable", values_to = "Value") %>%
  mutate(Panel = paste(Variable, Timepoint, sep = "-"))

# Set these according to your data range!
pam_limits <- c(0, 0.7)
pam_breaks <- seq(0, 0.7, by = 0.1)

score_limits <- c(0, 1.2)
score_breaks <- seq(0, 1.2, by = 0.2)

temp_colors <- c("30" = "#ACE6FB","34" = "#FFB94F","36" = "#ED7275","37" = "#EE151C","39" = "#B90309","40" = "#690206")

#### F003 ####
#F003 PAM-T1
pam_t1_F003 <- ggplot(filter(long_F003, Panel == "PAM-T1"),
                      aes(x = Temperature, y = Value, fill = Temperature)) +
  geom_boxplot(outlier.size = 1, width = 0.4, staplewidth = 0.3, position = position_dodge(width = 0.8), key_glyph = "rect") +  #outlier.shape = NA,
  #geom_jitter(aes(color = Temperature), width = 0.2, size = 1.2, alpha = 0.7, show.legend = FALSE) +
  scale_y_continuous(limits = pam_limits, breaks = pam_breaks) +
  scale_fill_manual(values = temp_colors, guide = guide_legend(nrow = 1)) +     # Custom fill colors for boxplots
  labs(x = "Temperature [°C]", y = expression("Photosynthetic efficiency (F"[v]*"/F"[m]*")"), fill = "Temperature [°C]",) +
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
           panel.spacing = unit(0.1, "lines"), # Adjust spacing between facets
           strip.text.x = element_text(color = "black", size = 12, hjust = 0.5, vjust = 0.5, face = "bold"))
pam_t1_F003

# Repeat for Score-T1, PAM-T2, Score-T2, and for H2
score_t1_F003 <- ggplot(filter(long_F003, Panel == "Score-T1"),
                        aes(x = Temperature, y = Value, fill = Temperature)) +
  geom_boxplot(outlier.size = 1, width = 0.4, staplewidth = 0.3, position = position_dodge(width = 0.8), key_glyph = "rect") +  #outlier.shape = NA,
  #geom_jitter(aes(color = Temperature), width = 0.2, size = 1.2, alpha = 0.7, show.legend = FALSE) +
  scale_y_continuous(limits = score_limits, breaks = score_breaks) +
  scale_fill_manual(values = temp_colors, guide = guide_legend(nrow = 1)) +     # Custom fill colors for boxplots
  labs(x = "Temperature [°C]", y = "BPS", fill = "Temperature [°C]") +
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
           panel.spacing = unit(0.1, "lines"), # Adjust spacing between facets
           strip.text.x = element_text(color = "black", size = 12, hjust = 0.5, vjust = 0.5, face = "bold"))
score_t1_F003

pam_t2_F003 <- ggplot(filter(long_F003, Panel == "PAM-T2"),
                      aes(x = Temperature, y = Value, fill = Temperature)) +
  geom_boxplot(outlier.size = 1, width = 0.4, staplewidth = 0.3, position = position_dodge(width = 0.8), key_glyph = "rect") +  #outlier.shape = NA,
  #geom_jitter(aes(color = Temperature), width = 0.2, size = 1.2, alpha = 0.7, show.legend = FALSE) +
  scale_y_continuous(limits = pam_limits, breaks = pam_breaks) +
  scale_fill_manual(values = temp_colors, guide = guide_legend(nrow = 1)) +     # Custom fill colors for boxplots
  labs(x = "Temperature [°C]", y = expression("Photosynthetic efficiency (F"[v]*"/F"[m]*")"), fill = "Temperature [°C]",) +
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
           panel.spacing = unit(0.1, "lines"), # Adjust spacing between facets
           strip.text.x = element_text(color = "black", size = 12, hjust = 0.5, vjust = 0.5, face = "bold"))
pam_t2_F003

score_t2_F003 <- ggplot(filter(long_F003, Panel == "Score-T2"),
                        aes(x = Temperature, y = Value, fill = Temperature)) +
  geom_boxplot(outlier.size = 1, width = 0.4, staplewidth = 0.3, position = position_dodge(width = 0.8), key_glyph = "rect") +  #outlier.shape = NA,
  #geom_jitter(aes(color = Temperature), width = 0.2, size = 1.2, alpha = 0.7, show.legend = FALSE) +
  scale_y_continuous(limits = score_limits, breaks = score_breaks) +
  scale_fill_manual(values = temp_colors, guide = guide_legend(nrow = 1)) +     # Custom fill colors for boxplots
  labs(x = "Temperature [°C]", y = "BPS", fill = "Temperature [°C]") +
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
           panel.spacing = unit(0.1, "lines"), # Adjust spacing between facets
           strip.text.x = element_text(color = "black", size = 12, hjust = 0.5, vjust = 0.5, face = "bold"))
score_t2_F003

F003_plot <- ggarrange(pam_t1_F003, score_t1_F003, pam_t2_F003, score_t2_F003, ncol = 4, nrow = 1, common.legend = TRUE, legend = "bottom")
F003_plot

#### H2 ####
#H2 PAM-T1
pam_t1_H2 <- ggplot(filter(long_H2, Panel == "PAM-T1"),
                      aes(x = Temperature, y = Value, fill = Temperature)) +
  geom_boxplot(outlier.size = 1, width = 0.4, staplewidth = 0.3, position = position_dodge(width = 0.8), key_glyph = "rect") +  #outlier.shape = NA,
  #geom_jitter(aes(color = Temperature), width = 0.2, size = 1.2, alpha = 0.7, show.legend = FALSE) +
  scale_y_continuous(limits = pam_limits, breaks = pam_breaks) +
  scale_fill_manual(values = temp_colors, guide = guide_legend(nrow = 1)) +     # Custom fill colors for boxplots
  labs(x = "Temperature [°C]", y = expression("Photosynthetic efficiency (F"[v]*"/F"[m]*")"), fill = "Temperature [°C]",) +
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
           panel.spacing = unit(0.1, "lines"), # Adjust spacing between facets
           strip.text.x = element_text(color = "black", size = 12, hjust = 0.5, vjust = 0.5, face = "bold"))
pam_t1_H2

# Repeat for Score-T1, PAM-T2, Score-T2
score_t1_H2 <- ggplot(filter(long_H2, Panel == "Score-T1"),
                        aes(x = Temperature, y = Value, fill = Temperature)) +
  geom_boxplot(outlier.size = 1, width = 0.4, staplewidth = 0.3, position = position_dodge(width = 0.8), key_glyph = "rect") +  #outlier.shape = NA,
  #geom_jitter(aes(color = Temperature), width = 0.2, size = 1.2, alpha = 0.7, show.legend = FALSE) +
  scale_y_continuous(limits = score_limits, breaks = score_breaks) +
  scale_fill_manual(values = temp_colors, guide = guide_legend(nrow = 1)) +     # Custom fill colors for boxplots
  labs(x = "Temperature [°C]", y = "BPS", fill = "Temperature [°C]") +
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
           panel.spacing = unit(0.1, "lines"), # Adjust spacing between facets
           strip.text.x = element_text(color = "black", size = 12, hjust = 0.5, vjust = 0.5, face = "bold"))
score_t1_H2

pam_t2_H2 <- ggplot(filter(long_H2, Panel == "PAM-T2"),
                      aes(x = Temperature, y = Value, fill = Temperature)) +
  geom_boxplot(outlier.size = 1, width = 0.4, staplewidth = 0.3, position = position_dodge(width = 0.8), key_glyph = "rect") +  #outlier.shape = NA,
  #geom_jitter(aes(color = Temperature), width = 0.2, size = 1.2, alpha = 0.7, show.legend = FALSE) +
  scale_y_continuous(limits = pam_limits, breaks = pam_breaks) +
  scale_fill_manual(values = temp_colors, guide = guide_legend(nrow = 1)) +     # Custom fill colors for boxplots
  labs(x = "Temperature [°C]", y = expression("Photosynthetic efficiency (F"[v]*"/F"[m]*")"), fill = "Temperature [°C]",) +
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
           panel.spacing = unit(0.1, "lines"), # Adjust spacing between facets
           strip.text.x = element_text(color = "black", size = 12, hjust = 0.5, vjust = 0.5, face = "bold"))
pam_t2_H2

score_t2_H2 <- ggplot(filter(long_H2, Panel == "Score-T2"),
                        aes(x = Temperature, y = Value, fill = Temperature)) +
  geom_boxplot(outlier.size = 1, width = 0.4, staplewidth = 0.3, position = position_dodge(width = 0.8), key_glyph = "rect") +  #outlier.shape = NA,
  #geom_jitter(aes(color = Temperature), width = 0.2, size = 1.2, alpha = 0.7, show.legend = FALSE) +
  scale_y_continuous(limits = score_limits, breaks = score_breaks) +
  scale_fill_manual(values = temp_colors, guide = guide_legend(nrow = 1)) +     # Custom fill colors for boxplots
  labs(x = "Temperature [°C]", y = "BPS", fill = "Temperature [°C]") +
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
           panel.spacing = unit(0.1, "lines"), # Adjust spacing between facets
           strip.text.x = element_text(color = "black", size = 12, hjust = 0.5, vjust = 0.5, face = "bold"))
score_t2_H2

H2_plot <- ggarrange(pam_t1_H2, score_t1_H2, pam_t2_H2, score_t2_H2, ncol = 4, nrow = 1, common.legend = TRUE, legend = "bottom")
H2_plot


#### Compute Kendalls tau correlation coefficient on PAM and Score means ####
## F003
# Compute means and SDs for PAM and Score by Bacterial Species, Temperature and Timepoint
summary_F003 <- Input_F003 %>%
  group_by(Timepoint, Temperature, Number) %>%
  summarise(
    PAM_mean = mean(PAM, na.rm = TRUE),
    PAM_sd = sd(PAM, na.rm = TRUE),
    Score_mean = mean(Score, na.rm = TRUE),
    Score_sd = sd(Score, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup()

# Compute Kendall's tau using the means and their respective groupings
# Overall correlation
cor_test_F003 <- cor.test(summary_F003$PAM_mean, summary_F003$Score_mean, method = "kendall")
# Extract correlation coefficient and p-value
tau_value_F003 <- cor_test_F003$estimate
p_value_F003 <- cor_test_F003$p.value
# Print results
cat(sprintf("Kendall's tau: %.3f, p-value: %.3f\n", tau_value_F003, p_value_F003))

#Correlation by Timepoint
cor_by_timepoint_F003 <- summary_F003 %>%
  group_by(Timepoint) %>%
  summarise(
    kendall_tau = cor(PAM_mean, Score_mean, method = "kendall"),
    p_value = cor.test(PAM_mean, Score_mean, method = "kendall")$p.value
  )
print(cor_by_timepoint_F003)

#Correlation by Temperature
cor_by_temp_F003 <- summary_F003 %>%
  group_by(Temperature) %>%
  summarise(
    kendall_tau = cor(PAM_mean, Score_mean, method = "kendall"),
    p_value = cor.test(PAM_mean, Score_mean, method = "kendall")$p.value
  )
print(cor_by_temp_F003)

#Correlation by Species
cor_by_species_F003 <- summary_F003 %>%
  group_by(Number) %>%
  summarise(
    kendall_tau = cor(PAM_mean, Score_mean, method = "kendall"),
    p_value = cor.test(PAM_mean, Score_mean, method = "kendall")$p.value
  )
print(cor_by_species_F003)

#Correlation by Timepoint and Temperature
cor_by_time_temp_F003 <- summary_F003 %>%
  group_by(Timepoint, Temperature) %>%
  summarise(
    kendall_tau = cor(PAM_mean, Score_mean, method = "kendall"),
    p_value = cor.test(PAM_mean, Score_mean, method = "kendall")$p.value
  )
print(cor_by_time_temp_F003)

#Correlation by Timepoint and Species
cor_by_time_species_F003 <- summary_F003 %>%
  group_by(Timepoint, Number) %>%
  summarise(
    kendall_tau = cor(PAM_mean, Score_mean, method = "kendall"),
    p_value = cor.test(PAM_mean, Score_mean, method = "kendall")$p.value
  )
print(cor_by_time_species_F003)

#Correlation by Temperature and Species
cor_by_temp_species_F003 <- summary_F003 %>%
  group_by(Temperature, Number) %>%
  summarise(
    kendall_tau = cor(PAM_mean, Score_mean, method = "kendall"),
    p_value = cor.test(PAM_mean, Score_mean, method = "kendall")$p.value
  )
print(cor_by_temp_species_F003)

# Create scatter plot with error bars
cor_F003 <- ggplot(summary_F003, aes(x = Score_mean, y = PAM_mean, color = Temperature)) +
  geom_point(size = 2) +
  #geom_errorbar(aes(ymin = PAM_mean - PAM_sd, ymax = PAM_mean + PAM_sd), width = 0.05) +
  #geom_errorbarh(aes(xmin = Score_mean - Score_sd, xmax = Score_mean + Score_sd), height = 0.05) +
  facet_wrap(~ Timepoint) +
  geom_smooth(aes(group = 1), method = "loess", span = 0.75, se = TRUE, color = "black", linewidth = 0.8) +
  labs(
    x = "BPS (mean BPS ± SD)",
    y = expression("Photosynthetic efficiency (mean F"[v]*"/F"[m]*" ± SD)"), 
    color = "Temperature",
    title = "PAM vs Score by Timepoint and Temperature"
  ) +
  scale_color_manual(values = temp_colors, guide = guide_legend(nrow = 1)) + # custom colors for points/legend
  scale_x_continuous(limits = c(0, 1.2), breaks = seq(0, 1.2, by = 0.2)) +
  scale_y_continuous(limits = c(-0.2, 0.7), breaks = seq(-0.2, 0.7, by = 0.1)) +
  theme_bw() +
  stat_cor(
    method = "kendall",
    cor.coef.name = "tau",
    color = "black",  # <-- fixes the grouping
    aes(label = paste("tau[b] == ", ..r.label.., "*','~", ..p.label..)),
    parse = TRUE, 
    label.x.npc = "left",
    label.y.npc = 1,
    size = 4,  r.accuracy = 0.01,    # 2 decimals for tau
    p.accuracy = 0.001    # 3 decimals for p
  )

print(cor_F003)

cor_F003_mod <- cor_F003 + theme (plot.title = element_text(hjust = 0.5), 
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
                                   panel.spacing = unit(0.1, "lines"), # Adjust spacing between facets
                                   strip.text.x = element_text(color = "black", size = 12, hjust = 0.5, vjust = 0.5, face = "bold"))

print(cor_F003_mod)

## H2
# Compute means and SDs for PAM and Score by Bacterial Species, Temperature and Timepoint
summary_H2 <- Input_H2 %>%
  group_by(Timepoint, Temperature, Number) %>%
  summarise(
    PAM_mean = mean(PAM, na.rm = TRUE),
    PAM_sd = sd(PAM, na.rm = TRUE),
    Score_mean = mean(Score, na.rm = TRUE),
    Score_sd = sd(Score, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup()

# Compute Kendall's tau using the means and their respective groupings
# Overall correlation
cor_test_H2 <- cor.test(summary_H2$PAM_mean, summary_H2$Score_mean, method = "kendall")
# Extract correlation coefficient and p-value
tau_value_H2 <- cor_test_H2$estimate
p_value_H2 <- cor_test_H2$p.value
# Print results
cat(sprintf("Kendall's tau: %.3f, p-value: %.3f\n", tau_value_H2, p_value_H2))

#Correlation by Timepoint
cor_by_timepoint_H2 <- summary_H2 %>%
  group_by(Timepoint) %>%
  summarise(
    kendall_tau = cor(PAM_mean, Score_mean, method = "kendall"),
    p_value = cor.test(PAM_mean, Score_mean, method = "kendall")$p.value
  )
print(cor_by_timepoint_H2)

#Correlation by Temperature
cor_by_temp_H2 <- summary_H2 %>%
  group_by(Temperature) %>%
  summarise(
    kendall_tau = cor(PAM_mean, Score_mean, method = "kendall"),
    p_value = cor.test(PAM_mean, Score_mean, method = "kendall")$p.value
  )
print(cor_by_temp_H2)

#Correlation by Species
cor_by_species_H2 <- summary_H2 %>%
  group_by(Number) %>%
  summarise(
    kendall_tau = cor(PAM_mean, Score_mean, method = "kendall"),
    p_value = cor.test(PAM_mean, Score_mean, method = "kendall")$p.value
  )
print(cor_by_species_H2)

#Correlation by Timepoint and Temperature
cor_by_time_temp_H2 <- summary_H2 %>%
  group_by(Timepoint, Temperature) %>%
  summarise(
    kendall_tau = cor(PAM_mean, Score_mean, method = "kendall"),
    p_value = cor.test(PAM_mean, Score_mean, method = "kendall")$p.value
  )
print(cor_by_time_temp_H2)

#Correlation by Timepoint and Species
cor_by_time_species_H2 <- summary_H2 %>%
  group_by(Timepoint, Number) %>%
  summarise(
    kendall_tau = cor(PAM_mean, Score_mean, method = "kendall"),
    p_value = cor.test(PAM_mean, Score_mean, method = "kendall")$p.value
  )
print(cor_by_time_species_H2)

#Correlation by Temperature and Species
cor_by_temp_species_H2 <- summary_H2 %>%
  group_by(Temperature, Number) %>%
  summarise(
    kendall_tau = cor(PAM_mean, Score_mean, method = "kendall"),
    p_value = cor.test(PAM_mean, Score_mean, method = "kendall")$p.value
  )
print(cor_by_temp_species_H2)

# Create scatter plot with error bars
cor_H2 <- ggplot(summary_H2, aes(x = Score_mean, y = PAM_mean, color = Temperature)) +
  geom_point(size = 2) +
  #geom_errorbar(aes(ymin = PAM_mean - PAM_sd, ymax = PAM_mean + PAM_sd), width = 0.05) +
  #geom_errorbarh(aes(xmin = Score_mean - Score_sd, xmax = Score_mean + Score_sd), height = 0.05) +
  facet_wrap(~ Timepoint) +
  geom_smooth(aes(group = 1), method = "loess", span = 0.75, se = TRUE, color = "black", linewidth = 0.8) +
  labs(
    x = "BPS (mean BPS ± SD)",
    y = expression("Photosynthetic efficiency (mean F"[v]*"/F"[m]*" ± SD)"), 
    color = "Temperature",
    title = "PAM vs Score by Timepoint and Temperature"
  ) +
  scale_color_manual(values = temp_colors, guide = guide_legend(nrow = 1)) + # custom colors for points/legend
  scale_x_continuous(limits = c(0, 1.2), breaks = seq(0, 1.2, by = 0.2)) +
  scale_y_continuous(limits = c(-0.2, 0.7), breaks = seq(-0.2, 0.7, by = 0.1)) +
  theme_bw() +
  stat_cor(
    method = "kendall",
    cor.coef.name = "tau",
    color = "black",  # <-- fixes the grouping
    aes(label = paste("tau[b] == ", ..r.label.., "*','~", ..p.label..)),
    parse = TRUE, 
    label.x.npc = "left",
    label.y.npc = 1,
    size = 4,  r.accuracy = 0.01,    # 2 decimals for tau
    p.accuracy = 0.001    # 3 decimals for p
  )

print(cor_H2)

cor_H2_mod <- cor_H2 + theme (plot.title = element_text(hjust = 0.5), 
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
                                   panel.spacing = unit(0.1, "lines"), # Adjust spacing between facets
                                   strip.text.x = element_text(color = "black", size = 12, hjust = 0.5, vjust = 0.5, face = "bold"))

print(cor_H2_mod)
