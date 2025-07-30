##### Loading libraries ####
library(drc)
library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(ggpubr)
library(stringr)
library(ggh4x)
library(httpgd)
library(viridis)
library(viridisLite)
library(cowplot)
library(VennDiagram)

hgd()

#### Figure 2 A = Scheme ####
#### Figure 2 B.1 ####

# read input file
setwd("path/to/directory")
input <- readxl::read_excel("Raw_Data_Code/Fig.10_IsolateLibrary/BacterialIsolatesByStrain_Figure10_Input.xlsx")

show(input$Family)
unique(input$Family)

input_count <- input  %>% 
               transmute(Strain, Family) %>% 
               count(Strain, Family)

#set colors
families<-c("Alteromonadaceae","Bacillaceae", "Brevibacteriaceae", "Gordoniaceae", "Marinobacteraceae", "Microbacteriaceae", "Moraxellaceae", "Nocardiaceae", "Phyllobacteriaceae", "Promicromonosporaceae", "Pseudomonadaceae", "Sphingomonadaceae", "Vibrionaceae")
colors_families<-c("#BD6868","#FFAD94","#FFB94F","#EAC88D","#F5DE76","#C0DE75","#99AED0","#B4A0EB","#9C80B9","#985693","#7F6273","#AF84A5","#F2A8C5")

#plot all isolates by family (no splitting by media/temperature) 
Fig2_B.1_plot <- ggplot(input_count, aes(Strain, n, fill= Family))  +
                 geom_bar(position="stack", stat="identity") +
                 labs(x ="", y ="Number of Bacterial Isolate Species") + #ggtitle("Unique bacterial isolates")+
                 scale_fill_manual(values = colors_families, name = "Family") +
                 scale_y_continuous(limits = c(0, 18), breaks = seq(0, 18, 2)) +
                 theme_bw()
Fig2_B.1_plot

Fig2_B.1_plot_mod <- Fig2_B.1_plot + theme (legend.position= "none", 
                                     line = element_line(linewidth = 0.8),
                                     axis.line = element_line(colour = "black"),
                                     axis.ticks = element_line(colour = "black"),
                                     axis.ticks.length = unit(0.2 , "cm"),
                                     axis.text = element_text(size = 13, colour = "black"),
                                     text = element_text(size = 13, colour = "black"),
                                     panel.grid.major = element_blank(),
                                     panel.grid.minor = element_blank(),
                                     panel.background = element_blank(),
                                     panel.border = element_blank(),
                                     strip.background = element_blank(),
                                     strip.text.x = element_text(color = "black", size = 12, angle = 0, hjust = 0.5, vjust = 0.5, face = "plain"))

Fig2_B.1_plot_mod

#### Figure 1 B.2 ####
setwd("path/to/directory")
input2 <- readxl::read_excel("Raw_Data_Code/Fig.10_IsolateLibrary/BacterialIsolatesByStatus_Figure10_Input.xlsx")

show(input2$Family)
unique(input2$Family)

input2_count <- input2  %>% 
                transmute(Status, Family) %>% 
                count(Status, Family)

input2$Status <- as.factor(input2$Status)
input2$Status <- factor(input2$Status, levels = c("Unique to F003", "Unique to H2", "Shared"))


#plot all isolates by family (no splitting by media/temperature) 
Fig2_B.2_plot <- ggplot(input2_count, aes(Status, n, fill= Family))  +
                 geom_bar(position="stack", stat="identity") +
                 labs(x ="", y ="Number of Bacterial Isolate Species") + #ggtitle("Unique bacterial isolates")+
                 scale_fill_manual(values = colors_families, name = "Family") +
                 scale_y_continuous(limits = c(0, 18), breaks = seq(0, 18, 2)) +
                 scale_x_discrete(limits = c("Unique to F003", "Unique to H2", "Shared")) +
                 theme_bw()
Fig2_B.2_plot

Fig2_B.2_plot_mod <- Fig2_B.2_plot + theme (legend.position= "none", 
                                     line = element_line(linewidth = 0.8),
                                     axis.line = element_line(colour = "black"),
                                     axis.ticks = element_line(colour = "black"),
                                     axis.ticks.length = unit(0.2 , "cm"),
                                     axis.text = element_text(size = 13, colour = "black"),
                                     text = element_text(size = 13, colour = "black"),
                                     panel.grid.major = element_blank(),
                                     panel.grid.minor = element_blank(),
                                     panel.background = element_blank(),
                                     panel.border = element_blank(),
                                     strip.background = element_blank(),
                                     strip.text.x = element_text(color = "black", size = 12, angle = 0, hjust = 0.5, vjust = 0.5, face = "plain"))

Fig2_B.2_plot_mod

##### Figure 1 B #####
Fig2_B <- ggarrange(Fig2_B.1_plot_mod, Fig2_B.2_plot_mod, 
                    ncol = 2, nrow = 1, 
                    widths = c(1, 1),  # Adjust relative widths
                    align = "h", axis = "y")  # Align by y-axis

Fig2_B


Fig2_B <- plot_grid(Fig2_B.1_plot_mod, Fig2_B.2_plot_mod)
Fig2_B

#### Figure 1 C (Venn Diagram) ####
setwd("path/to/directory")
input <- readxl::read_excel("Raw_Data_Code/Fig.10_IsolateLibrary/BacterialIsolatesByStrain_Figure10_Input.xlsx")

# Create sets for each anemone strain
set1 <- unique(input$Species[input$Strain == "F003"])
set2 <- unique(input$Species[input$Strain == "H2"])

# Combine sets into a list
venn_list <- list(F003 = set1, H2 = set2)

# Plot the venn diagram
grid.newpage()
vennPlot <- venn.diagram(x = venn_list, filename = NULL,        # Colors for each circle
                         category.names = c("F003", "H2"),
                         height = 480 , 
                         width = 480 , 
                         resolution = 400,
                         compression = "lzw",
                         lwd = 1,
                         col=c("#B772B3", '#FFB94E'),
                         fill = c(alpha("#B772B3",0.3), alpha('#FFB94E',0.3)),
                         cex = 1.5,
                         fontfamily = "sans",
                         cat.cex = 1.3,
                         cat.default.pos = "outer",
                         cat.pos = c(-27, 27),
                         cat.dist = c(0.055, 0.055),
                         cat.fontfamily = "sans",
                         cat.col = c("#B772B3", '#FFB94E'))

grid.draw(vennPlot)           


#### Figure 1 D ####
#plot all isolates split by the media/temperature they were isolated from
setwd("path/to/directory")
input3 <- readxl::read_excel("Raw_Data_Code/Fig.10_IsolateLibrary/BacterialIsolatesByMedium_Figure10_Input.xlsx")

show(input3$Family)
unique(input3$Family)

input3$Medium <- factor(input3$Medium, levels = c("BA", "MA", "M1"))
input3$Temperature <- factor(input3$Temperature, levels = c("18", "25"))

input3_count <- input3  %>% 
                transmute(Strain, Family, Temperature, Medium) %>% 
                count(Family, Medium, Temperature, Strain) 

input3_count$Temperature <-  gsub("18", "18 °C", input3_count$Temperature)
input3_count$Temperature <-  gsub("25", "25 °C", input3_count$Temperature)

input3_count$MediumTemperature <- paste(input3_count$Medium, "-", input3_count$Temperature, sep = " ")

#plot all isolates by family and faceted by medium and temperature
Fig2_D_plot <- ggplot(input3_count, aes(Strain, n, fill= factor(Family)))  +
                      geom_bar(position="stack", stat="identity") +
                      labs(x ="", y ="Number of Bacterial Isolate Species") + #ggtitle("Unique bacterial isolates")+
                      scale_fill_manual(values = colors_families, name = "Family") +
                      scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
                      theme_bw() + 
                      facet_grid(Temperature ~ Medium, scales = "free_x")
               
Fig2_D_plot

Fig2_D_plot_mod <- Fig2_D_plot + theme (legend.position= "right", 
                                 legend.title = element_text(colour="black", size=13,face="bold"),
                                 legend.text=element_text(size = 13),
                                 line = element_line(linewidth = 0.8),
                                 axis.line = element_line(colour = "black"),
                                 axis.ticks = element_line(colour = "black"),
                                 axis.ticks.length = unit(0.2 , "cm"),
                                 axis.text = element_text(size = 13, colour = "black"),
                                 text = element_text(size = 13, colour = "black"),
                                 panel.grid.major = element_blank(),
                                 panel.grid.minor = element_blank(),
                                 panel.background = element_blank(),
                                 strip.background = element_blank(), 
                                 strip.text.x = element_text(color = "black", size = 12, angle = 0, hjust = 0.5, vjust = 0.5, face = "plain"))

Fig2_D_plot_mod


#### Figure 2 ####
Fig2 <- plot_grid(Fig2_B, Fig2_D_plot_mod)

Fig2




