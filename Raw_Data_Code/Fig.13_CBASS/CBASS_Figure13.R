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
#library(drc)

# read input file
setwd("path/to/directory")
ed50_diff<- readxl::read_excel("Raw_Data_Code/Fig.13_CBASS/CBASS_Diffplot_Input_BothStrains_Figure13_Input.xlsx")

hgd()

ed50_diff$LowerErrorDelta=as.numeric(ed50_diff$LowerErrorDelta)
ed50_diff$UpperErrorDelta=as.numeric(ed50_diff$UpperErrorDelta)

ed50_diff$`Isolate Name`=as.factor(ed50_diff$`Isolate Name`)
ed50_diff$`Isolate Name` <- factor(ed50_diff$`Isolate Name`,levels = rev(c("Alteromonas alba", "Alteromonas portus", "Brevibacterium sanguinis", "Cellulosimicrobium funkei", "Marinobacter adhaerens", "Mesorhizobium terrae", "Microbacterium enclense",
                                                                           "Microbacterium ginsengisoli", "Microbacterium oxydans","Microbacterium paraoxydans", "Microbacterium phyllosphaerae", "Microbacterium zeae",
                                                                           "Psychrobacter nivimaris", "Rhodococcus ruber", "Rossellomorea aquimaris", "Sphingomonas melonis", "Vibrio xuii")))

All_IsolateColors_FvFm <- rev(c("#BD6868", "#F08383", "#F5B676", "#BE80BB", "#F5DE76", "#9C80B9", "#A2CB7E", "#7CB6AC","#5E90A2","#74B2C9", "#8EE4E5", "#84DCF9","#99AED0","#C5B5EF","#FFAD94","#AF84AD","#F2A8C5"))
                              
ed50_diff$Timepoint <- gsub("T 1", "Timepoint 1 (post stress)", ed50_diff$Timepoint)
ed50_diff$Timepoint <- gsub("T 2", "Timepoint 2 (post recovery)", ed50_diff$Timepoint)

ed50_diff_FvFm <- filter(ed50_diff, Parameter == "Fv/Fm")
ed50_diff_PS <- filter(ed50_diff, Parameter == "PS") 

ed50_diff_FvFm <- ed50_diff_FvFm %>%
  mutate(Strain = factor(Strain, levels = c("H2", "F003")))

# Add Presence column 
ed50_diff_FvFm <- ed50_diff_FvFm %>%
  group_by(`Isolate Name`) %>%
  mutate(
    Presence = case_when(
      n_distinct(Strain) == 1 & Strain[1] == "F003" ~ "F003_only",
      n_distinct(Strain) == 1 & Strain[1] == "H2" ~ "H2_only",
      TRUE ~ "Both"
    )
  ) %>%
  ungroup()

# Sort by Presence THEN alphabetical order
ed50_diff_FvFm <- ed50_diff_FvFm %>%
  arrange(`Isolate Name`) %>%  # Sort A-Z
  mutate(`Isolate Name` = factor(`Isolate Name`, levels = unique(`Isolate Name`)))

# ED50 difference to control barplot FvFm
ed50_diff_plot_FvFm <- ggplot(
                       ed50_diff_FvFm, aes(x = delta, y = `Isolate Name`, fill = `Isolate Name`)) +
                       geom_errorbar(aes(xmin = LowerErrorDelta, xmax = UpperErrorDelta), width = 0.25) +
                       geom_bar(stat = "identity", color = "black", width = 0.75, aes(fill = `Isolate Name`), position = "dodge") +
                       geom_label (data = ed50_diff_FvFm, aes(label = round(delta, 2), x = -2.75), size = 4, #, position = position_dodge(width = 0.75),
                       inherit.aes = TRUE) +
                       scale_x_continuous(limits=c(-3, 3), breaks = c(-2, -1, 0, 1, 2)) +
                       geom_vline(xintercept = 0, linewidth = 0.75) +
                       labs(y= "", x = expression("Δ F"[v]*"/F"[m]*" derived popED50 [°C]"), title = "H2") +
                       scale_fill_manual(values = All_IsolateColors_FvFm, name = "Isolate Name") +
                       facet_grid(~ Timepoint, labeller = label_wrap_gen(width = 17)) #+
                       #scale_fill_brewer(palette = "Set2")

ed50_diff_plot_FvFm_comb <- ggplot(
                            ed50_diff_FvFm, aes(x = delta, y = `Isolate Name`, fill = `Isolate Name`, pattern = Strain)) +
                            geom_errorbar(aes(xmin = LowerErrorDelta, xmax = UpperErrorDelta), width = 0.25, position = position_dodge(width = 0.75), linewidth = 0.5) +
                            # Bars (fixed width, forced alignment)
                            geom_bar_pattern(
                              stat = "identity",
                              color = "black",        # Outline color
                              width = 0.75,           # Fixed width for ALL bars
                              position = position_dodge(width = 0.75),  # Align with error bars
                              pattern_fill = "white", # Background for stripes
                              pattern_colour = NA,    # Stripes use species colors
                              pattern_density = 0.2,
                              pattern_spacing = 0.03) +
                            geom_label(aes(label = round(delta, 2), x = -2.75), position = position_dodge(width = 0.75), size = 4) +
                            scale_fill_manual(values = All_IsolateColors_FvFm, guide = "none") +  # Hide fill legend
                            scale_pattern_manual(values = c("F003" = "none", "H2" = "stripe"),  # H2 = stripes
                            guide = "none") + # Hide pattern legend
                            labs(y = "", x = expression("Δ F"[v]*"/F"[m]*" derived popED50 [°C]")) +
                            geom_vline(xintercept = 0, linewidth = 0.75) +
                            facet_grid(~ Timepoint, labeller = label_wrap_gen(width = 17)) 

ed50_diff_plot_FvFm_comb


ed50_diff_plot_FvFm_comb_mod <- ed50_diff_plot_FvFm_comb + 
                                theme (plot.title = element_text(hjust = 0.5), 
                                legend.position= "none", 
                                line = element_line(linewidth = 0.8),
                                axis.line = element_line(colour = "black"),
                                axis.ticks = element_line(colour = "black"),
                                axis.ticks.length = unit(0.2 , "cm"),
                                axis.text.x = element_markdown(color = "black", size = 13),
                                axis.text.y = element_text(hjust = 0, face = "italic", size = 13),
                                text = element_text(size = 13, colour = "black"),
                                panel.grid.major = element_blank(),
                                panel.grid.minor = element_blank(),
                                panel.background = element_blank(),
                                strip.background = element_blank(), 
                                strip.text.x = element_text(color = "black", size = 12, hjust = 0.5, vjust = 0.5, face = "bold"))

ed50_diff_plot_FvFm_comb_mod

#### repeat for VPA/VPS ED50s ####
ed50_diff_PS <- ed50_diff_PS %>%
  mutate(Strain = factor(Strain, levels = c("H2", "F003")))

# Add Presence column 
ed50_diff_PS <- ed50_diff_PS %>%
  group_by(`Isolate Name`) %>%
  mutate(
    Presence = case_when(
      n_distinct(Strain) == 1 & Strain[1] == "F003" ~ "F003_only",
      n_distinct(Strain) == 1 & Strain[1] == "H2" ~ "H2_only",
      TRUE ~ "Both"
    )
  ) %>%
  ungroup()

# Sort by Presence THEN alphabetical order
ed50_diff_PS <- ed50_diff_PS %>%
  arrange(`Isolate Name`) %>%  # Sort A-Z
  mutate(`Isolate Name` = factor(`Isolate Name`, levels = unique(`Isolate Name`)))

ed50_diff_PS$`Isolate Name` <- factor(ed50_diff_PS$`Isolate Name`,levels = rev(c("Alteromonas portus", "Brevibacterium sanguinis", "Cellulosimicrobium funkei", "Marinobacter adhaerens", "Mesorhizobium terrae",
                                                                           "Microbacterium ginsengisoli", "Microbacterium oxydans","Microbacterium paraoxydans", "Microbacterium phyllosphaerae", "Microbacterium zeae",
                                                                           "Psychrobacter nivimaris", "Rhodococcus ruber", "Sphingomonas melonis", "Vibrio xuii")))

All_IsolateColors_PS <- rev(c("#F08383", "#F5B676", "#BE80BB", "#F5DE76", "#9C80B9", "#7CB6AC","#5E90A2","#74B2C9", "#8EE4E5", "#84DCF9","#99AED0","#C5B5EF","#AF84AD","#F2A8C5"))

# ED50 difference to control barplot PS
ed50_diff_plot_PS <- ggplot(
                     ed50_diff_PS, aes(x = delta, y = `Isolate Name`, fill = `Isolate Name`)) +
                     geom_errorbar(aes(xmin = LowerErrorDelta, xmax = UpperErrorDelta), width = 0.25) +
                     geom_bar(stat = "identity", color = "black", width = 0.75, aes(fill = `Isolate Name`), position = "dodge") +
                     geom_label (data = ed50_diff_PS, aes(label = round(delta, 2), x = -2.75), size = 4, #, position = position_dodge(width = 0.75),
                     inherit.aes = TRUE) +
                     scale_x_continuous(limits=c(-3, 2), breaks = c(-2, -1, 0, 1, 2)) +
                     geom_vline(xintercept = 0, linewidth = 0.75) +
                     labs(y= "", x = expression("Δ F"[v]*"/F"[m]*" derived popED50 [°C]"), title = "H2") +
                     scale_fill_manual(values = All_IsolateColors_PS, name = "Isolate Name") +
                     facet_grid(~ Timepoint, labeller = label_wrap_gen(width = 17)) #+
                     #scale_fill_brewer(palette = "Set2")

ed50_diff_plot_PS_comb <- ggplot(
                          ed50_diff_PS, aes(x = delta, y = `Isolate Name`, fill = `Isolate Name`, pattern = Strain)) +
                          geom_errorbar(aes(xmin = LowerErrorDelta, xmax = UpperErrorDelta), width = 0.25, position = position_dodge(width = 0.75), linewidth = 0.5) +
                          # Bars (fixed width, forced alignment)
                          geom_bar_pattern(
                            stat = "identity",
                            color = "black",        # Outline color
                            width = 0.75,           # Fixed width for ALL bars
                            position = position_dodge(width = 0.75),  # Align with error bars
                            pattern_fill = "white", # Background for stripes
                            pattern_colour = NA,    # Stripes use species colors
                            pattern_density = 0.2,
                            pattern_spacing = 0.03) +
                          geom_label(aes(label = round(delta, 2), x = -2.75), position = position_dodge(width = 0.75), size = 4) +
                          scale_fill_manual(values = All_IsolateColors_PS, guide = "none") +  # Hide fill legend
                          scale_pattern_manual(values = c("F003" = "none", "H2" = "stripe"),  # H2 = stripes
                          guide = "none") + # Hide pattern legend
                          labs(y = "", x = expression("Δ BPS derived popED50 [°C]")) +
                          scale_x_continuous(limits=c(-2.75, 2.1), breaks = c(-2, -1, 0, 1, 2)) +
                          geom_vline(xintercept = 0, linewidth = 0.75) +
                          facet_grid(~ Timepoint, labeller = label_wrap_gen(width = 17)) 

ed50_diff_plot_PS_comb


ed50_diff_plot_PS_comb_mod <- ed50_diff_plot_PS_comb + 
                                theme (plot.title = element_text(hjust = 0.5), 
                                legend.position= "none", 
                                line = element_line(linewidth = 0.8),
                                axis.line = element_line(colour = "black"),
                                axis.ticks = element_line(colour = "black"),
                                axis.ticks.length = unit(0.2 , "cm"),
                                axis.text.x = element_markdown(color = "black", size = 13),
                                axis.text.y = element_text(hjust = 0, face = "italic", size = 13),
                                text = element_text(size = 13, colour = "black"),
                                panel.grid.major = element_blank(),
                                panel.grid.minor = element_blank(),
                                panel.background = element_blank(),
                                strip.background = element_blank(), 
                                strip.text.x = element_text(color = "black", size = 12, hjust = 0.5, vjust = 0.5, face = "bold"))

ed50_diff_plot_PS_comb_mod

#### Figure 5 ####
Fig5 <- plot_grid(ed50_diff_plot_FvFm_comb_mod, ed50_diff_plot_PS_comb_mod, ncol = 1, align = "v", axis = "lr")
Fig5
