library(drc)
library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(ggpubr)
library(stringr)
library(ggh4x)
library(egg)
library(readxl)


##### F003 CBASS ####
# repeat plotting for each CBASS run - choose a different sheet from the excel file
setwd("~/Desktop/PhD_VoolstraLab/Paper_BacteriaScreen/Manuscript/Raw_Data/Fig.5_CBASS")
HOBO_data <- read_excel("CBASS_F003_HOBOs.xlsx", sheet = 1)

#change table formatting
F_CBASS_1 <- pivot_longer(HOBO_data, starts_with("T", ignore.case = FALSE, vars = NULL))
#add colors 
F_CBASS_1$name <- as.factor(F_CBASS_1$name)
Temperature_colors <- c("#8AC8F1", "#F7A857", "#F15758", "#AB1E22")
as.factor(Temperature_colors)

#plot HOBO data as line plot - F003A
F_CBASS_1_plot <- ggplot(data = F_CBASS_1, aes(run_time, value, color = name, group = name)) + 
                  geom_line() + 
                  theme_classic() + 
                  scale_y_continuous(breaks=c(30, 34, 36, 39), limits = c(28, 40.0), expand = c(0, 0)) +
                  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +#####if R reads spreadsheet time as time and date use this row and comment the one below (if not, invert comment)
                  scale_color_manual("name", values = Temperature_colors) +
                  labs(y= "Temperature Profile", x = "CBASS run time (h)", title = "F003 - CBASS 1") +
                  labs(color = "Temperatures")

F_CBASS_1_plot <- F_CBASS_1_plot + theme (legend.position= "none", 
                                   legend.title = element_text(colour="black", size=13,face="bold"),
                                   legend.text=element_text(size = 13),
                                   line = element_line(linewidth = 0.8),
                                   axis.line = element_line(colour = "black"),
                                   axis.ticks = element_line(colour = "black"),
                                   axis.ticks.length = unit(0.2 , "cm"),
                                   axis.text = element_text(size = 13, colour = "black"),
                                   text = element_text(size = 12, colour = "black"),
                                   panel.grid.major = element_blank(),
                                   panel.grid.minor = element_blank(),
                                   panel.background = element_blank(),
                                   strip.background = element_blank(), 
                                   strip.text.x = element_text(color = "black", size = 12, angle = 0, hjust = 0.5, vjust = 0.5, face = "plain"))
F_CBASS_1_plot

##### H2 CBASS ####
# repeat plotting for each CBASS run - choose a different sheet from the excel file
setwd("~/Desktop/PhD_VoolstraLab/Paper_BacteriaScreen/Manuscript/Raw_Data/Fig.5_CBASS")
HOBO_data <- read_excel("CBASS_H2_HOBOs.xlsx", sheet = 1)

#change table formatting
H_CBASS_1 <- pivot_longer(HOBO_data, starts_with("T", ignore.case = FALSE, vars = NULL))
#add colors 
H_CBASS_1$name <- as.factor(H_CBASS_1$name)
Temperature_colors <- c("#8AC8F1", "#F7A857", "#F15758", "#AB1E22")
as.factor(Temperature_colors)

#plot HOBO data as line plot - F003A
H_CBASS_1_plot <- ggplot(data = H_CBASS_1, aes(run_time, value, color = name, group = name)) + 
                  geom_line() + 
                  theme_classic() + 
                  scale_y_continuous(breaks=c(30, 34, 36, 39), limits = c(28, 40.0), expand = c(0, 0)) +
                  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +#####if R reads spreadsheet time as time and date use this row and comment the one below (if not, invert comment)
                  scale_color_manual("name", values = Temperature_colors) +
                  labs(y= "Temperature Profile", x = "CBASS run time (h)", title = "H2 - CBASS 1") +
                  labs(color = "Temperatures")

H_CBASS_1_plot <- H_CBASS_1_plot + theme (legend.position= "none", 
                                   legend.title = element_text(colour="black", size=13,face="bold"),
                                   legend.text=element_text(size = 13),
                                   line = element_line(linewidth = 0.8),
                                   axis.line = element_line(colour = "black"),
                                   axis.ticks = element_line(colour = "black"),
                                   axis.ticks.length = unit(0.2 , "cm"),
                                   axis.text = element_text(size = 13, colour = "black"),
                                   text = element_text(size = 12, colour = "black"),
                                   panel.grid.major = element_blank(),
                                   panel.grid.minor = element_blank(),
                                   panel.background = element_blank(),
                                   strip.background = element_blank(), 
                                   strip.text.x = element_text(color = "black", size = 12, angle = 0, hjust = 0.5, vjust = 0.5, face = "plain"))
H_CBASS_1_plot


