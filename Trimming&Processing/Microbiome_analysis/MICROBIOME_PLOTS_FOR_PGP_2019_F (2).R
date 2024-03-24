# Plots for 2019 field trial # 


meco_Physeq_PC_2019_16s_Potato_CONT
# bacteria and fungal rel abund. together # 
#control over time , 16 s
meco_Physeq_PC_2019_16s_Potato_CONT$sample_table$Sampling_Date %<>% factor(., levels = c("08-07-19", "06-08-19", "03-09-19"))
G1_Meco_2019_16s_Cont <- trans_abund$new(dataset = meco_Physeq_PC_2019_16s_Potato_CONT, taxrank = "Phylum", ntaxa = 10, groupmean = "Sampling_Date")
g1_Meco_2019_16s_Cont <- G1_Meco_2019_16s_Cont$plot_bar(others_color = "grey70", legend_text_italic = TRUE) 
g1<- g1_Meco_2019_16s_Cont + 
  theme_classic() +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 12, face ="bold"),
        axis.text.y = element_text(size = 12, face ="bold"))
#PO over time 16s 
meco_Physeq_PC_2019_16s_Potato_Po$sample_table$Sampling_Date %<>% factor(., levels = c("08-07-19",  "06-08-19", "03-09-19"))
G2_Meco_2019_16s_Po <- trans_abund$new(dataset = meco_Physeq_PC_2019_16s_Potato_Po, taxrank = "Phylum", ntaxa = 10, groupmean = "Sampling_Date")
g2_Meco_2019_16s_Po <- G2_Meco_2019_16s_Po$plot_bar(others_color = "grey70", legend_text_italic = FALSE) 
g2_Meco_2019_16s_Po + theme_classic() + theme(axis.title.y = element_text(size = 15)) 
g2<- g2_Meco_2019_16s_Po + 
  theme_classic() +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 12, face ="bold"),
        axis.text.y = element_text(size = 12, face ="bold"))

#Fungal over time, control 

meco_Physeq_PC_2019_ITS_Potato_cont$sample_table$Sampling_Date %<>% factor(., levels = c("08-07-19",  "06-08-19", "03-09-19"))
G3_Meco_2019_ITS_Cont <- trans_abund$new(dataset = meco_Physeq_PC_2019_ITS_Potato_cont, taxrank = "Phylum", ntaxa = 10, groupmean = "Sampling_Date")
g3_Meco_2019_ITS_Cont <- G3_Meco_2019_ITS_Cont$plot_bar(others_color = "grey70", legend_text_italic = FALSE) 
g3_Meco_2019_ITS_Cont + theme_classic() + theme(axis.title.y = element_text(size = 15)) 
g3<- g3_Meco_2019_ITS_Cont + 
  theme_classic() +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 12, face ="bold"),
        axis.text.y = element_text(size = 12, face ="bold"))

# Fungal over time PO. 

meco_Physeq_PC_2019_ITS_Potato_PO$sample_table$Sampling_Date %<>% factor(., levels = c("08-07-19",  "06-08-19", "03-09-19"))
G4_Meco_2019_ITS_Po <- trans_abund$new(dataset = meco_Physeq_PC_2019_ITS_Potato_PO, taxrank = "Phylum", ntaxa = 10, groupmean = "Sampling_Date")
g4_Meco_2019_ITS_Po <- G4_Meco_2019_ITS_Po$plot_bar(others_color = "grey70", legend_text_italic = FALSE) 
g4_Meco_2019_ITS_Po + theme_classic() + theme(axis.title.y = element_text(size = 15)) 
g4<- g4_Meco_2019_ITS_Po + 
  theme_classic() +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 12, face ="bold"),
        axis.text.y = element_text(size = 12, face ="bold"))

library(gridExtra)
# Assume g1, g2, g3 and g4 are ggplot objects
# Align plots g1 and g2 on the top row
top_row <- grid.arrange(g1 + ggtitle("A"), g2 + ggtitle("B"), ncol = 2)
# Align plots g3 and g4 on the bottom row
bottom_row <- grid.arrange(g3 + ggtitle("C"), g4 + ggtitle("D"), ncol = 2)
# Combine the top and bottom rows into one plot
grid.arrange(top_row, bottom_row, ncol = 1)
# Combine the plots using grid.arrange()
combined_plot_OT_2019 <- grid.arrange(top_row, bottom_row, ncol = 1)


library(gridExtra)

# Assume g1, g2, g3 and g4 are ggplot objects

# Align plots g1 and g2 on the top row
top_row <- grid.arrange(g1 + ggtitle("A") + theme(plot.title = element_text(size = 16, face = "bold")),
                        g2 + ggtitle("B") + theme(plot.title = element_text(size = 16, face = "bold")),
                        ncol = 2)

# Align plots g3 and g4 on the bottom row
bottom_row <- grid.arrange(g3 + ggtitle("C") + theme(plot.title = element_text(size = 16, face = "bold")),
                           g4 + ggtitle("D") + theme(plot.title = element_text(size = 16, face = "bold")),
                           ncol = 2)

# Combine the top and bottom rows into one plot
grid.arrange(top_row, bottom_row, ncol = 1)

# Combine the plots using grid.arrange()
combined_plot_OT_2019 <- grid.arrange(top_row, bottom_row, ncol = 1)









# Save the combined plot with the same zoom level as the original plots
ggsave("combined_plot_OT_2019.png", plot = combined_plot, width = 10, height = 10, dpi = 900)



## beta diversity over time 16 s control and p.oligandrum

b1 <- B_meco_Physeq_PC_2019_16s_Potato_CONT$plot_ordination(plot_color = "Sampling_Date", plot_shape = "Sampling_Date", plot_type = c("point", "ellipse"))

b2 <- B_meco_Physeq_PC_2019_16s_Potato_Po$plot_ordination(plot_color = "Sampling_Date", plot_shape = "Sampling_Date", plot_type = c("point", "ellipse"))

b3 <- B_meco_Physeq_PC_2019_ITS_Potato_cont$plot_ordination(plot_color = "Sampling_Date", plot_shape = "Sampling_Date", plot_type = c("point", "ellipse"))

b4 <- B_meco_Physeq_PC_2019_ITS_Potato_PO$plot_ordination(plot_color = "Sampling_Date", plot_shape = "Sampling_Date", plot_type = c("point", "ellipse"))

# Load required packages
library(ggplot2)
library(gridExtra)

# Create top row of plots (b1 and b2)
top_row <- grid.arrange(b1 + ggtitle("A"), b2 + ggtitle("B"), ncol = 2)

# Create bottom row of plots (b3 and b4)
bottom_row <- grid.arrange(b3 + ggtitle("C"), b4 + ggtitle("D"), ncol = 2)

# Combine the top and bottom rows into a single plot
combined_plot <- grid.arrange(top_row, bottom_row, ncol = 1)

# Increase the legend size and font weight
combined_plot <- combined_plot +
  theme(legend.text = element_text(size = 14, face = "bold"),
        legend.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"))

# Save the combined plot as a PNG file
ggsave("combined_plot_Beta_ot_2019.png", combined_plot, width = 8, height = 8, dpi = 900)

#Relative abundance of p.olig vs control at august and september for its and 16s 
library(gridExtra)

# Assume c1, c2, c3 and c4 are ggplot objects
# Arrange plots c1 and c2 on the top row
top_row <- grid.arrange(c1 + ggtitle("A", face = "bold", size = 14), 
                        c2 + ggtitle("B", face = "bold", size = 14), 
                        ncol = 2)
# Arrange plots c3 and c4 on the bottom row
bottom_row <- grid.arrange(c3 + ggtitle("C", face = "bold", size = 14), 
                           c4 + ggtitle("D", face = "bold", size = 14), 
                           ncol = 2)
# Combine the top and bottom rows into one plot
grid.arrange(top_row, bottom_row, ncol = 1)
# Combine the plots using grid.arrange()
combined_plot <- grid.arrange(top_row, bottom_row, ncol = 1)
# Save the combined plot with the same zoom level as the original plots
ggsave("combined_plot_povs.png", plot = combined_plot, width = 10, height = 10, dpi = 900)




#cont vs po 2019 


# bacteria and fungal rel abund. together # 
#control over time , 16 s
G1_Meco_2019_16s_aug <- trans_abund$new(dataset = meco_Physeq_PC_2019_16s_august, taxrank = "Phylum", ntaxa = 10, groupmean = "Treatment")
g1_Meco_2019_16s_aug <- G1_Meco_2019_16s_aug$plot_bar(others_color = "grey70", legend_text_italic = FALSE)
c1 <- g1_Meco_2019_16s_aug + theme_classic() + theme(axis.title.y = element_text(size = 15))
c1<- g1_Meco_2019_16s_Cont + 
  theme_classic() +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 12, face ="bold"),
        axis.text.y = element_text(size = 12, face ="bold"))
c1
#PO over time 16s 
meco_Physeq_PC_2019_16s_Potato_Po$sample_table$Sampling_Date %<>% factor(., levels = c("08-07-19",  "06-08-19", "03-09-19"))
G2_Meco_2019_16s_Po <- trans_abund$new(dataset = meco_Physeq_PC_2019_16s_Potato_Po, taxrank = "Phylum", ntaxa = 10, groupmean = "Sampling_Date")
g2_Meco_2019_16s_Po <- G2_Meco_2019_16s_Po$plot_bar(others_color = "grey70", legend_text_italic = FALSE) 
g2_Meco_2019_16s_Po + theme_classic() + theme(axis.title.y = element_text(size = 15)) 
g2<- g2_Meco_2019_16s_Po + 
  theme_classic() +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 12, face ="bold"),
        axis.text.y = element_text(size = 12, face ="bold"))
g2

#Fungal over time, control 

meco_Physeq_PC_2019_ITS_Potato_cont$sample_table$Sampling_Date %<>% factor(., levels = c("08-07-19",  "06-08-19", "03-09-19"))
G3_Meco_2019_ITS_Cont <- trans_abund$new(dataset = meco_Physeq_PC_2019_ITS_Potato_cont, taxrank = "Phylum", ntaxa = 10, groupmean = "Sampling_Date")
g3_Meco_2019_ITS_Cont <- G3_Meco_2019_ITS_Cont$plot_bar(others_color = "grey70", legend_text_italic = FALSE) 
g3_Meco_2019_ITS_Cont + theme_classic() + theme(axis.title.y = element_text(size = 15)) 
g3<- g3_Meco_2019_ITS_Cont + 
  theme_classic() +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 12, face ="bold"),
        axis.text.y = element_text(size = 12, face ="bold"))
View(g3)
View(G3_Meco_2019_ITS_Cont)

View(g3_Meco_2019_ITS_Cont)
# Fungal over time PO. 

meco_Physeq_PC_2019_ITS_Potato_PO$sample_table$Sampling_Date %<>% factor(., levels = c("08-07-19",  "06-08-19", "03-09-19"))
G4_Meco_2019_ITS_Po <- trans_abund$new(dataset = meco_Physeq_PC_2019_ITS_Potato_PO, taxrank = "Phylum", ntaxa = 10, groupmean = "Sampling_Date")
g4_Meco_2019_ITS_Po <- G4_Meco_2019_ITS_Po$plot_bar(others_color = "grey70", legend_text_italic = FALSE) 
g4_Meco_2019_ITS_Po + theme_classic() + theme(axis.title.y = element_text(size = 15)) 
g4<- g4_Meco_2019_ITS_Po + 
  theme_classic() +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 12, face ="bold"),
        axis.text.y = element_text(size = 12, face ="bold"))
g4







## po vs control rel abundance plot for manus 2019. 
C1_Meco_2019_16s_aug <- trans_abund$new(dataset = meco_Physeq_PC_2019_16s_august, taxrank = "Phylum", ntaxa = 10, groupmean = "Treatment")
c1_Meco_2019_16s_aug <- C1_Meco_2019_16s_aug$plot_bar(others_color = "grey70", legend_text_italic = FALSE)
c1_Meco_2019_16s_aug + theme_classic() + theme(axis.title.y = element_text(size = 15))
c1 <- c1_Meco_2019_16s_aug + theme_classic() +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 12, face ="bold"),
        axis.text.y = element_text(size = 12, face ="bold"))

c1
View(C1_Meco_2019_16s_aug)

C2_Meco_2019_16s_sep <- trans_abund$new(dataset = meco_Physeq_PC_2019_16s_September, taxrank = "Phylum", ntaxa = 10, groupmean = "Treatment")
c2_Meco_2019_16s_sep <- C2_Meco_2019_16s_sep$plot_bar(others_color = "grey70", legend_text_italic = FALSE)
c2_Meco_2019_16s_sep + theme_classic() + theme(axis.title.y = element_text(size = 15))
c2 <- c2_Meco_2019_16s_sep + theme_classic() +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 12, face ="bold"),
        axis.text.y = element_text(size = 12, face ="bold"))

c2
View(C2_Meco_2019_16s_sep)


C3_Meco_2019_ITS_aug <- trans_abund$new(dataset = meco_Physeq_PC_2019_ITS_august, taxrank = "Phylum", ntaxa = 10, groupmean = "Treatment")
c3_Meco_2019_ITS_aug <- C3_Meco_2019_ITS_aug$plot_bar(others_color = "grey70", legend_text_italic = FALSE)
c3 <- c3_Meco_2019_ITS_aug + theme_classic() + theme(axis.title.y = element_text(size = 15))
c3 <- c3_Meco_2019_ITS_aug + theme_classic() +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 12, face ="bold"),
        axis.text.y = element_text(size = 12, face ="bold"))
c3

View(C3_Meco_2019_ITS_aug)


C4_Meco_2019_ITS_sep <- trans_abund$new(dataset = meco_Physeq_PC_2019_ITS_September, taxrank = "Phylum", ntaxa = 10, groupmean = "Treatment")
c4_Meco_2019_ITS_sep <- C4_Meco_2019_ITS_sep$plot_bar(others_color = "grey70", legend_text_italic = FALSE)
c4 <- c4_Meco_2019_ITS_sep + theme_classic() + theme(axis.title.y = element_text(size = 15))
c4 <- c4_Meco_2019_ITS_sep + theme_classic() +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 12, face ="bold"),
        axis.text.y = element_text(size = 12, face ="bold"))
c4
View(C4_Meco_2019_ITS_sep)

library(gridExtra)
# Assume g1, g2, g3 and g4 are ggplot objects
# Align plots g1 and g2 on the top row
top_row <- grid.arrange(c1 + ggtitle("A"), c2 + ggtitle("B"), ncol = 2)
# Align plots g3 and g4 on the bottom row
bottom_row <- grid.arrange(c3 + ggtitle("C"), c4 + ggtitle("D"), ncol = 2)
# Combine the top and bottom rows into one plot
grid.arrange(top_row, bottom_row, ncol = 1)
# Combine the plots using grid.arrange()
combined_plot_povsCont_2019 <- grid.arrange(top_row, bottom_row, ncol = 1)
combined_plot_povsCont_2019
# Save the combined plot with the same zoom level as the original plots
ggsave("combined_plot_povsCont_2019.png", plot = combined_plot, width = 10, height = 10, dpi = 900)





# Assume g1, g2, g3 and g4 are ggplot objects
# Align plots g1 and g2 on the top row
top_row <- grid.arrange(c1 + ggtitle("A") + theme(plot.title = element_text(size = 20, face = "bold")),
                        c2 + ggtitle("B") + theme(plot.title = element_text(size = 20, face = "bold")), ncol = 2)
# Align plots g3 and g4 on the bottom row
bottom_row <- grid.arrange(c3 + ggtitle("C") + theme(plot.title = element_text(size = 20, face = "bold")),
                           c4 + ggtitle("D") + theme(plot.title = element_text(size = 20, face = "bold")), ncol = 2)
# Combine the top and bottom rows into one plot
grid.arrange(top_row, bottom_row, ncol = 1)
# Combine the plots using grid.arrange()
combined_plot_povsCont_2019_1 <- grid.arrange(top_row, bottom_row, ncol = 1)
combined_plot_povsCont_2019_1
# Save the combined plot with the same zoom level as the original plots
ggsave("combined_plot_povsCont_2019.png", plot = combined_plot_povsCont_2019_1, width = 10, height = 10, dpi = 900)



## shannon index po vs co 

d1 <- T_meco_Physeq_PC_2019_16s_august$plot_alpha(measure = "Shannon")

d2 <- T_meco_Physeq_PC_2019_16s_September$plot_alpha(measure = "Shannon")

d3 <- T_meco_Physeq_PC_2019_ITS_august$plot_alpha(measure = "Shannon")

d4 <- T_meco_Physeq_PC_2019_ITS_September$plot_alpha(measure = "Shannon")




library(ggplot2)
library(gridExtra)

# Create each individual plot
d1 <- T_meco_Physeq_PC_2019_16s_august$plot_alpha(measure = "Shannon") + ggtitle("A")
d2 <- T_meco_Physeq_PC_2019_16s_September$plot_alpha(measure = "Shannon") + ggtitle("B")
d3 <- T_meco_Physeq_PC_2019_ITS_august$plot_alpha(measure = "Shannon") + ggtitle("C")
d4 <- T_meco_Physeq_PC_2019_ITS_September$plot_alpha(measure = "Shannon") + ggtitle("D")

library(gridExtra)
library(ggplot2)

d1 <- T_meco_Physeq_PC_2019_16s_august$plot_alpha(measure = "Shannon") + 
  ggtitle("A") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"))

d2 <- T_meco_Physeq_PC_2019_16s_September$plot_alpha(measure = "Shannon") + 
  ggtitle("B") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"))

d3 <- T_meco_Physeq_PC_2019_ITS_august$plot_alpha(measure = "Shannon") + 
  ggtitle("C") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"))

d4 <- T_meco_Physeq_PC_2019_ITS_September$plot_alpha(measure = "Shannon") + 
  ggtitle("D") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"))

# Combine plots with grid.arrange
top_row <- grid.arrange(d1, d2, ncol = 2)
bottom_row <- grid.arrange(d3, d4, ncol = 2)
combined_plot <- grid.arrange(top_row, bottom_row, ncol = 1)

ggsave("combined_plot_povsco_shannon.png", plot = combined_plot, width = 10, height = 10, dpi = 900)

## beta diversity 

e1 <- B_meco_Physeq_PC_2019_16s_august$plot_ordination(plot_color = "Treatment", plot_shape = "Sampling_Date", plot_type = c("point", "ellipse"))
e2 <- B_meco_Physeq_PC_2019_16s_September$plot_ordination(plot_color = "Treatment", plot_shape = "Sampling_Date", plot_type = c("point", "ellipse"))
e3 <- B_meco_Physeq_PC_2019_ITS_august$plot_ordination(plot_color = "Treatment", plot_shape = "Sampling_Date", plot_type = c("point", "ellipse"))
e4 <- B_meco_Physeq_PC_2019_ITS_September$plot_ordination(plot_color = "Treatment", plot_shape = "Sampling_Date", plot_type = c("point", "ellipse"))

# Define plots with titles
e1 <- B_meco_Physeq_PC_2019_16s_august$plot_ordination(plot_color = "Treatment", plot_shape = "Sampling_Date", plot_type = c("point", "ellipse")) + ggtitle("A")
e2 <- B_meco_Physeq_PC_2019_16s_September$plot_ordination(plot_color = "Treatment", plot_shape = "Sampling_Date", plot_type = c("point", "ellipse")) + ggtitle("B")
e3 <- B_meco_Physeq_PC_2019_ITS_august$plot_ordination(plot_color = "Treatment", plot_shape = "Sampling_Date", plot_type = c("point", "ellipse")) + ggtitle("C")
e4 <- B_meco_Physeq_PC_2019_ITS_September$plot_ordination(plot_color = "Treatment", plot_shape = "Sampling_Date", plot_type = c("point", "ellipse")) + ggtitle("D")

# Add bold text to ggtitle and axis labels
bold_text_theme <- theme(plot.title = element_text(face = "bold", size = 16),
                         axis.title = element_text(face = "bold", size = 14))

# Combine plots with grid.arrange
top_row <- grid.arrange(e1, e2, ncol = 2)
bottom_row <- grid.arrange(e3, e4, ncol = 2)
combined_plot <- grid.arrange(top_row, bottom_row, ncol = 1)

# Apply bold_text_theme to the combined plot
combined_plot <- combined_plot + bold_text_theme

# Save the combined plot with the same zoom level as the original plots
ggsave("combined_plot_beta_2019_povsco.png", plot = combined_plot, width = 10, height = 10, dpi = 900)

##ancom bc package, combined plots 
f1 <- t1_ancombc_meco_Physeq_PC_2019_16s_august$plot_diff_abund((use_number = 1:2),  add_sig = TRUE)
f2 <- t1_ancombc_meco_Physeq_PC_2019_ITS_august$plot_diff_abund((use_number = 1:2),  add_sig = TRUE)
f3 <- t1_ancombc_meco_Physeq_PC_2019_ITS_September$plot_diff_abund((use_number = 1:1),  add_sig = TRUE)

# Load necessary libraries
library(gridExtra)
library(ggplot2)

# Add titles to plots
f1 <- t1_ancombc_meco_Physeq_PC_2019_16s_august$plot_diff_abund((use_number = 1:2),  add_sig = TRUE) +
  ggtitle("A")
f2 <- t1_ancombc_meco_Physeq_PC_2019_ITS_august$plot_diff_abund((use_number = 1:2),  add_sig = TRUE) +
  ggtitle("B")
f3 <- t1_ancombc_meco_Physeq_PC_2019_ITS_September$plot_diff_abund((use_number = 1:1),  add_sig = TRUE) +
  ggtitle("C")

# Add bold text to ggtitle and axis labels
bold_text_theme <- theme(plot.title = element_text(face = "bold", size = 16),
                         axis.title = element_text(face = "bold", size = 14))

# Combine plots with grid.arrange
top_row <- grid.arrange(f1, ncol = 1)
bottom_row <- grid.arrange(f2, f3, ncol = 2)
combined_plot <- grid.arrange(top_row, bottom_row, ncol = 1, heights = c(1, 2))

# Apply bold text theme
combined_plot + bold_text_theme


#library(cowplot)

f1 <- t1_ancombc_meco_Physeq_PC_2019_16s_august$plot_diff_abund((use_number = 1:2), add_sig = TRUE)
f2 <- t1_ancombc_meco_Physeq_PC_2019_ITS_august$plot_diff_abund((use_number = 1:2), add_sig = TRUE)
f3 <- t1_ancombc_meco_Physeq_PC_2019_ITS_September$plot_diff_abund((use_number = 1:1), add_sig = TRUE)

# Add bold text to ggtitle and axis labels
bold_text_theme <- theme(plot.title = element_text(face = "bold", size = 16),
                         axis.title = element_text(face = "bold", size = 14))

# Combine plots with plot_grid and align vertically
combined_plot <- plot_grid(f1, plot_grid(f2, f3, ncol = 2), nrow = 2, align = "v")

# Add titles and bold text
combined_plot <- combined_plot +
  ggtitle("Title") +
  theme(plot.title = element_text(face = "bold", size = 20)) +
  bold_text_theme

combined_plot


library(gridExtra)

# create the plots
f1 <- t1_ancombc_meco_Physeq_PC_2019_16s_august$plot_diff_abund((use_number = 1:2), add_sig = TRUE)
f2 <- t1_ancombc_meco_Physeq_PC_2019_ITS_august$plot_diff_abund((use_number = 1:2), add_sig = TRUE)
f3 <- t1_ancombc_meco_Physeq_PC_2019_ITS_September$plot_diff_abund((use_number = 1:1), add_sig = TRUE)

# Add bold text to ggtitle and axis labels
bold_text_theme <- theme(plot.title = element_text(face = "bold", size = 16),
                         axis.title = element_text(face = "bold", size = 14))

# combine the plots and set the axis parameter to "tb" for vertical alignment
combined_plots <- grid.arrange(f1, grid.arrange(f2, f3, ncol = 2), ncol = 1, axis = "tb")

# add the titles
combined_plots <- ggtitle("My Title") + 
  labs(subtitle = "My Subtitle") +
  plot_layout(design = "
                    1 2
                    3 3
                  ")

# apply the theme to the combined plot
combined_plots <- combined_plots + bold_text_theme

