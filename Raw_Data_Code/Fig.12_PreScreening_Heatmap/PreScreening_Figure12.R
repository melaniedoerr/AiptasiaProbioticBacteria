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
Input_Means<- readxl::read_excel("Raw_Data_Code/Fig.12_PreScreening_Heatmap/PreScreeningHeatMap_Mean_F003H2_Figure12_Input.xlsx")

hgd()

Input_Means$Timepoint <- gsub("5", "5 h", Input_Means$Timepoint)
Input_Means$Timepoint <- gsub("24", "24 h", Input_Means$Timepoint)
Input_Means$Timepoint <- gsub("48", "48 h", Input_Means$Timepoint)

Input_Means_F003 <- filter(Input_Means, Strain == "F003")
Input_Means_H2 <- filter(Input_Means, Strain == "H2")

# Define lower and upper limits for the color bar
lower_limit_FvFm <- -0.1  
upper_limit_FvFm <- 0.1 

lower_limit_Score <- -1  
upper_limit_Score <- 1 

#Define the desired labels with expressions
concentration_labels <- c(
  "10^5" = expression("10" * phantom(.)^5),
  "10^6" = expression("10" * phantom(.)^6),
  "10^7" = expression("10" * phantom(.)^7))

##### F003 #####
# Ensure factors are ordered as desired
Input_Means_F003 <- Input_Means_F003 %>%
  mutate(
    Delta_PAM = as.numeric(as.character(Delta_PAM)),
    Delta_Score = as.numeric(as.character(Delta_Score)),
    Timepoint = factor(Timepoint, levels = c("5 h", "24 h", "48 h")),
    Concentration = factor(Concentration, levels = c("10^5", "10^6", "10^7")),
    Species = factor(Species, levels = sort(unique(Species), decreasing = TRUE))
  )

Input_Means_F003$na_flag <- is.na(Input_Means_F003$Delta_PAM)

# Plot FvFm with custom red-white-green gradient
HeatMap_Means_F003_gradient <- ggplot(Input_Means_F003, aes(x = Concentration, y = Species, fill = Delta_PAM)) +
                               geom_tile(color = "#FAFAFA", size=0.3, na.rm = TRUE) + # na.rm = TRUE removes NA values before plotting
                               scale_x_discrete(
                                  position = "top",
                                  labels = concentration_labels) +  # Move x-axis labels to the top
                               scale_fill_gradientn(
                                  colours = c(
                                  #F47099",
                                  "#F47099",#low=bad
                                  "#FFAD94",
                                  "#FFAD94",
                                  "#F3F3F3",
                                  "#C5E96A",
                                  "#C5E96A",
                                  "#7CB6AC",
                                  "#7CB6AC" #high=good
                                   ),
                               values = scales::rescale(c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1)),  # Type of legend to use
                               limits = c(lower_limit_FvFm, upper_limit_FvFm),
                               breaks = c(-0.1, 0, 0.1),
                               #labels = c("adverse", "control", "improved"),
                               oob = scales::squish,
                               na.value = "white",# This keeps all values colored, even if outside limits
                               ) +
                               theme_minimal() +
                               labs(x = "Bacterial Concentration (cells/mL)", y = "", fill = expression("Δ F"[v]*"/F"[m]*" from control")) +
                               facet_grid(. ~ Timepoint, scales = "free_x", space = "free_x")

HeatMap_Means_F003_gradient

HeatMap_Means_F003_gradient_mod <- HeatMap_Means_F003_gradient + 
                                   theme (plot.title = element_text(hjust = 0.5), 
                                   legend.position= "bottom", 
                                   line = element_line(linewidth = 0.8),
                                   #axis.line = element_line(colour = "black"),
                                   #axis.ticks = element_line(colour = "black"),
                                   #axis.ticks.length = unit(0.2 , "cm"),
                                   axis.text.x = element_text(color = "black", size = 13),
                                   axis.text.y = element_text(hjust = 0, face = "italic", size = 13),
                                   text = element_text(size = 13, colour = "black"),
                                   panel.grid.major = element_blank(),
                                   panel.grid.minor = element_blank(),
                                   panel.background = element_blank(),
                                   #strip.background = element_rect(fill = "white", color = "black"), 
                                   strip.placement = "top", # Place strip labels outside
                                   panel.spacing = unit(0.1, "lines"), # Adjust spacing between facets
                                   strip.text.x = element_text(color = "black", size = 12, hjust = 0.5, vjust = 0.5, face = "bold"))

HeatMap_Means_F003_gradient_mod


# Plot BPS with custom red-white-green gradient
HeatMap_Means_F003_BPS_gradient <- ggplot(Input_Means_F003, aes(x = Concentration, y = Species, fill = Delta_Score)) +
                                   geom_tile(color = "#FAFAFA", size=0.3, na.rm = TRUE) + # na.rm = TRUE removes NA values before plotting
                               scale_x_discrete(
                                  position = "top",
                                  labels = concentration_labels) +  # Move x-axis labels to the top
                               scale_fill_gradientn(
                                  colours = c(
                                  "#F47099",
                                  "#F47099",#low=bad
                                  "#FFAD94",
                                  "#FFAD94",
                                  "#F3F3F3",
                                  "#C5E96A",
                                  "#C5E96A",
                                  "#7CB6AC",
                                  "#7CB6AC" #high=good
                                   ),
                               values = scales::rescale(c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)),  # Type of legend to use
                               limits = c(lower_limit_Score, upper_limit_Score),
                               breaks = c(-1, -0.5, 0, 0.5, 1),
                               #labels = c("adverse", "control", "improved"),
                               oob = scales::squish,
                               na.value = "white",# This keeps all values colored, even if outside limits
                               ) +
                               theme_minimal() +
                               labs(x = "Bacterial Concentration (cells/mL)", y = "", fill = expression("Δ BPS from control")) +
                               facet_grid(. ~ Timepoint, scales = "free_x", space = "free_x")

HeatMap_Means_F003_BPS_gradient

HeatMap_Means_F003_BPS_gradient_mod <- HeatMap_Means_F003_BPS_gradient + 
                                   theme (plot.title = element_text(hjust = 0.5), 
                                   legend.position= "bottom", 
                                   line = element_line(linewidth = 0.8),
                                   #axis.line = element_line(colour = "black"),
                                   #axis.ticks = element_line(colour = "black"),
                                   #axis.ticks.length = unit(0.2 , "cm"),
                                   axis.text.x = element_text(color = "black", size = 13),
                                   axis.text.y = element_text(hjust = 0, face = "italic", size = 13),
                                   text = element_text(size = 13, colour = "black"),
                                   panel.grid.major = element_blank(),
                                   panel.grid.minor = element_blank(),
                                   panel.background = element_blank(),
                                   #strip.background = element_rect(fill = "white", color = "black"), 
                                   strip.placement = "top", # Place strip labels outside
                                   panel.spacing = unit(0.1, "lines"), # Adjust spacing between facets
                                   strip.text.x = element_text(color = "black", size = 12, hjust = 0.5, vjust = 0.5, face = "bold"))

HeatMap_Means_F003_BPS_gradient_mod

F003 <- plot_grid(HeatMap_Means_F003_gradient_mod, HeatMap_Means_F003_BPS_gradient_mod, ncol = 2, align = "h", axis = "lr")
F003
##### H2 #####
# Ensure factors are ordered as desired
Input_Means_H2 <- Input_Means_H2 %>%
  mutate(
    Delta_PAM = as.numeric(as.character(Delta_PAM)),
    Delta_Score = as.numeric(as.character(Delta_Score)),
    Timepoint = factor(Timepoint, levels = c("5 h", "24 h", "48 h")),
    Concentration = factor(Concentration, levels = c("10^5", "10^6", "10^7")),
    Species = factor(Species, levels = sort(unique(Species), decreasing = TRUE))
  )

# Plot with custom red-white-green gradient
HeatMap_Means_H2_gradient <- ggplot(Input_Means_H2, aes(x = Concentration, y = Species, fill = Delta_PAM)) +
                             geom_tile(color = "#FAFAFA", size=0.3, na.rm = TRUE) + # na.rm = TRUE removes NA values before plotting
                             scale_x_discrete(
                               position = "top",
                             labels = concentration_labels) +  # Move x-axis labels to the top
                             scale_fill_gradientn(
                               colours = c(
                               "#F47099",
                               "#F47099",#low=bad
                               "#FFAD94",
                               "#FFAD94",
                               "#F3F3F3",#EBEBEB
                               "#C5E96A",
                               "#C5E96A",
                               "#7CB6AC",
                               "#7CB6AC" #high=good
                                ),
                             values = scales::rescale(c(-0.1, -0.075, -0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1)),  # Type of legend to use
                             limits = c(lower_limit_FvFm, upper_limit_FvFm),
                             breaks = c(-0.1, 0, 0.1),
                             #labels = c("adverse", "control", "improved"),
                             oob = scales::squish,
                             na.value = "white",# This keeps all values colored, even if outside limits
                             ) +
                             theme_minimal() +
                             labs(x = "Bacterial Concentration (cells/mL)", y = "", fill = expression("Δ F"[v]*"/F"[m]*" from control")) +
                             facet_grid(. ~ Timepoint, scales = "free_x", space = "free_x")

HeatMap_Means_H2_gradient

HeatMap_Means_H2_gradient_mod <- HeatMap_Means_H2_gradient + 
                                 theme (plot.title = element_text(hjust = 0.5), 
                                 legend.position= "bottom", 
                                 line = element_line(linewidth = 0.8),
                                 #axis.line = element_line(colour = "black"),
                                 #axis.ticks = element_line(colour = "black"),
                                 #axis.ticks.length = unit(0.2 , "cm"),
                                 axis.text.x = element_text(color = "black", size = 13),
                                 axis.text.y = element_text(hjust = 0, face = "italic", size = 13),
                                 text = element_text(size = 13, colour = "black"),
                                 panel.grid.major = element_blank(),
                                 panel.grid.minor = element_blank(),
                                 panel.background = element_blank(),
                                 #strip.background = element_rect(fill = "white", color = "black"), 
                                 strip.placement = "top", # Place strip labels outside
                                 panel.spacing = unit(0.1, "lines"), # Adjust spacing between facets
                                 strip.text.x = element_text(color = "black", size = 12, hjust = 0.5, vjust = 0.5, face = "bold"))

HeatMap_Means_H2_gradient_mod

# Plot BPS with custom red-white-green gradient
HeatMap_Means_H2_BPS_gradient <- ggplot(Input_Means_H2, aes(x = Concentration, y = Species, fill = Delta_Score)) +
                                   geom_tile(color = "#FAFAFA", size=0.3, na.rm = TRUE) + # na.rm = TRUE removes NA values before plotting
                               scale_x_discrete(
                                  position = "top",
                                  labels = concentration_labels) +  # Move x-axis labels to the top
                               scale_fill_gradientn(
                                  colours = c(
                                  "#F47099",
                                  "#F47099",#low=bad
                                  "#FFAD94",
                                  "#FFAD94",
                                  "#F3F3F3",
                                  "#C5E96A",
                                  "#C5E96A",
                                  "#7CB6AC",
                                  "#7CB6AC" #high=good
                                   ),
                               values = scales::rescale(c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)),  # Type of legend to use
                               limits = c(lower_limit_Score, upper_limit_Score),
                               breaks = c(-1, -0.5, 0, 0.5, 1),
                               #labels = c("adverse", "control", "improved"),
                               oob = scales::squish,
                               na.value = "white",# This keeps all values colored, even if outside limits
                               ) +
                               theme_minimal() +
                               labs(x = "Bacterial Concentration (cells/mL)", y = "", fill = expression("Δ BPS from control")) +
                               facet_grid(. ~ Timepoint, scales = "free_x", space = "free_x")

HeatMap_Means_H2_BPS_gradient

HeatMap_Means_H2_BPS_gradient_mod <- HeatMap_Means_H2_BPS_gradient + 
                                   theme (plot.title = element_text(hjust = 0.5), 
                                   legend.position= "bottom", 
                                   line = element_line(linewidth = 0.8),
                                   #axis.line = element_line(colour = "black"),
                                   #axis.ticks = element_line(colour = "black"),
                                   #axis.ticks.length = unit(0.2 , "cm"),
                                   axis.text.x = element_text(color = "black", size = 13),
                                   axis.text.y = element_text(hjust = 0, face = "italic", size = 13),
                                   text = element_text(size = 13, colour = "black"),
                                   panel.grid.major = element_blank(),
                                   panel.grid.minor = element_blank(),
                                   panel.background = element_blank(),
                                   #strip.background = element_rect(fill = "white", color = "black"), 
                                   strip.placement = "top", # Place strip labels outside
                                   panel.spacing = unit(0.1, "lines"), # Adjust spacing between facets
                                   strip.text.x = element_text(color = "black", size = 12, hjust = 0.5, vjust = 0.5, face = "bold"))

HeatMap_Means_H2_BPS_gradient_mod

