

##############            16 s, 
Physeq_PC_2019_16s

Physeq_PC_2019_16s <- readRDS("/Users/christian/Desktop/physeq 2019 /Physeq_PC_2019_16s.RDS")
meta_2019_16s <- as.matrix(sample_data(Physeq_PC_2019_16s))
write.csv(meta_2019_16s, file = "meta_2019_16s_1.csv")
str(meta_2019_16s)


View(Physeq_PC_2019_16s)
meta_adjust_2019 <- read.csv2("/Users/christian/Desktop/all desktop august 2023/Meta_adjust_PGP_2019_Bacteria_Final.csv", row = 1)
sample_data(Physeq_PC_2019_16s) <- meta_adjust_2019

View(Physeq_PC_2019_16s)
Physeq_PC_2019_16s
View(Physeq_PC_2019_16s)
Physeq_PC_2019_16s
Physeq_PC_2019_16s_Potato <- subset_samples(Physeq_PC_2019_16s, Species == "Potato_Kuras")
View(Physeq_PC_2019_16s_Potato)
saveRDS(Physeq_PC_2019_16s, file ="Physeq_PC_2019_16s_Final.RDS")
Physeq_PC_2019_16s
Physeq_PC_2019_16s_july <- subset_samples(Physeq_PC_2019_16s_Potato, Sampling_Date == "July")
View(Physeq_PC_2019_16s_july)
Physeq_PC_2019_16s_august <- subset_samples(Physeq_PC_2019_16s_Potato, Sampling_Date == "August")
Physeq_PC_2019_16s_august
Physeq_PC_2019_16s_September <- subset_samples(Physeq_PC_2019_16s_Potato, Sampling_Date == "September")
Physeq_PC_2019_16s_September

install.packages("devtools")
library(devtools)
devtools::install_github("ChiLiubio/microeco")
library(microeco)
## 16s control over time 


Physeq_PC_2019_16s_Potato_Control <- subset_samples(Physeq_PC_2019_16s_Potato, Treatment =="Control")
Physeq_PC_2019_16s_Potato_Control
meco_Physeq_PC_2019_16s_Potato_CONT <- phyloseq2meco(Physeq_PC_2019_16s_Potato_Control)
meco_Physeq_PC_2019_16s_Potato_CONT$tidy_dataset()
View(meco_Physeq_PC_2019_16s_Potato_CONT)
Physeq_PC_2019_16s_Potato_Control

## 16s p.oligandrum over time ## 

Physeq_PC_2019_16s_Potato_Po <- subset_samples(Physeq_PC_2019_16s_Potato, Treatment =="P. oligandrum ")
Physeq_PC_2019_16s_Potato_Po
meco_Physeq_PC_2019_16s_Potato_Po <- phyloseq2meco(Physeq_PC_2019_16s_Potato_Po)
meco_Physeq_PC_2019_16s_Potato_Po$tidy_dataset()
meco_Physeq_PC_2019_16s_Potato_Po


#make them into microeco objects 

meco_Physeq_PC_2019_16s_Potato <- phyloseq2meco(Physeq_PC_2019_16s_Potato)
meco_Physeq_PC_2019_16s_Potato$tidy_dataset()
meco_Physeq_PC_2019_16s_Potato
meco_Physeq_PC_2019_16s_july <- phyloseq2meco(Physeq_PC_2019_16s_july)
meco_Physeq_PC_2019_16s_july$tidy_dataset()
View(meco_Physeq_PC_2019_16s_july)
meco_Physeq_PC_2019_16s_august <- phyloseq2meco(Physeq_PC_2019_16s_august)
meco_Physeq_PC_2019_16s_august$tidy_dataset()
meco_Physeq_PC_2019_16s_September <- phyloseq2meco(Physeq_PC_2019_16s_September)
meco_Physeq_PC_2019_16s_September$tidy_dataset()

##16 s plots abundancy plots

G1_Meco_2019_16s_july <- trans_abund$new(dataset = meco_Physeq_PC_2019_16s_july, taxrank = "Phylum", ntaxa = 10, groupmean = "Treatment")
g1_Meco_2019_16s_july <- G1_Meco_2019_16s_july$plot_bar(others_color = "grey70", legend_text_italic = FALSE)
g1_Meco_2019_16s_july + theme_classic() + theme(axis.title.y = element_text(size = 15)) 

G1_Meco_2019_16s_aug <- trans_abund$new(dataset = meco_Physeq_PC_2019_16s_august, taxrank = "Phylum", ntaxa = 10, groupmean = "Treatment")
g1_Meco_2019_16s_aug <- G1_Meco_2019_16s_aug$plot_bar(others_color = "grey70", legend_text_italic = FALSE)
g1_Meco_2019_16s_aug + theme_classic() + theme(axis.title.y = element_text(size = 15))

G1_Meco_2019_16s_sep <- trans_abund$new(dataset = meco_Physeq_PC_2019_16s_September, taxrank = "Phylum", ntaxa = 10, groupmean = "Treatment")
g1_Meco_2019_16s_sep <- G1_Meco_2019_16s_sep$plot_bar(others_color = "grey70", legend_text_italic = FALSE)
g1_Meco_2019_16s_sep + theme_classic() + theme(axis.title.y = element_text(size = 15))

#over time 16s LGC_2019 
View(meco_Physeq_PC_2019_16s_Potato_CONT)
meco_Physeq_PC_2019_16s_Potato_CONT$sample_table$Sampling_Date %<>% factor(., levels = c("08-07-19",  "06-08-19", "03-09-19"))
G1_Meco_2019_16s_Cont <- trans_abund$new(dataset = meco_Physeq_PC_2019_16s_Potato_CONT, taxrank = "Phylum", ntaxa = 10, groupmean = "Sampling_Date")
g1_Meco_2019_16s_Cont <- G1_Meco_2019_16s_Cont$plot_bar(others_color = "grey70", legend_text_italic = FALSE) 
g1_Meco_2019_16s_Cont + theme_classic() + theme(axis.title.y = element_text(size = 15)) 
##increased size 

library(ggplot2)

meco_Physeq_PC_2019_16s_Potato_CONT$sample_table$Sampling_Date %<>% factor(., levels = c("08-07-19", "06-08-19", "03-09-19"))
G1_Meco_2019_16s_Cont <- trans_abund$new(dataset = meco_Physeq_PC_2019_16s_Potato_CONT, taxrank = "Phylum", ntaxa = 10, groupmean = "Sampling_Date")
g1_Meco_2019_16s_Cont <- G1_Meco_2019_16s_Cont$plot_bar(others_color = "grey70", legend_text_italic = TRUE) 

g1<- g1_Meco_2019_16s_Cont + 
  theme_classic() +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 13, face ="bold"),
        axis.text.y = element_text(size = 13, face ="bold"))
g1


##
meco_Physeq_PC_2019_16s_Potato_Po

meco_Physeq_PC_2019_16s_Potato_Po$sample_table$Sampling_Date %<>% factor(., levels = c("08-07-19",  "06-08-19", "03-09-19"))
G2_Meco_2019_16s_Po <- trans_abund$new(dataset = meco_Physeq_PC_2019_16s_Potato_Po, taxrank = "Phylum", ntaxa = 10, groupmean = "Sampling_Date")
g2_Meco_2019_16s_Po <- G2_Meco_2019_16s_Po$plot_bar(others_color = "grey70", legend_text_italic = FALSE) 
g2_Meco_2019_16s_Po + theme_classic() + theme(axis.title.y = element_text(size = 15)) 

g2<- g2_Meco_2019_16s_Po + 
  theme_classic() +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 13, face ="bold"),
        axis.text.y = element_text(size = 13, face ="bold"))
g2






meco_Physeq_PC_2019_16s_august
## 16s alpha diversity plots # 

#july
meco_Physeq_PC_2019_16s_july$cal_alphadiv(PD = FALSE)
T_meco_Physeq_PC_2019_16s_july<- trans_alpha$new(dataset = meco_Physeq_PC_2019_16s_july, group = "Treatment")
T_meco_Physeq_PC_2019_16s_july$cal_diff(method = "anova")
T_meco_Physeq_PC_2019_16s_july$plot_alpha(measure = "Observed")
T_meco_Physeq_PC_2019_16s_july$plot_alpha(measure = "Shannon")
T_meco_Physeq_PC_2019_16s_july$plot_alpha(pair_compare = TRUE, shape = "Treatment")
T_meco_Physeq_PC_2019_16s_july$res_diff

#august

meco_Physeq_PC_2019_16s_august$cal_alphadiv(PD = FALSE)
T_meco_Physeq_PC_2019_16s_august<- trans_alpha$new(dataset = meco_Physeq_PC_2019_16s_august, group = "Treatment")
T_meco_Physeq_PC_2019_16s_august$cal_diff(method = "anova")
d1 <- T_meco_Physeq_PC_2019_16s_august$plot_alpha(measure = "Shannon")
T_meco_Physeq_PC_2019_16s_august$plot_alpha(measure = "Observed")
T_meco_Physeq_PC_2019_16s_august$plot_alpha(pair_compare = TRUE, shape = "Treatment")
T_meco_Physeq_PC_2019_16s_august$res_diff

#september

meco_Physeq_PC_2019_16s_September$cal_alphadiv(PD = FALSE)
T_meco_Physeq_PC_2019_16s_September<- trans_alpha$new(dataset = meco_Physeq_PC_2019_16s_September, group = "Treatment")
T_meco_Physeq_PC_2019_16s_September$cal_diff(method = "anova")
d2 <- T_meco_Physeq_PC_2019_16s_September$plot_alpha(measure = "Shannon")

T_meco_Physeq_PC_2019_16s_September$plot_alpha(measure = "Shannon")
T_meco_Physeq_PC_2019_16s_September$plot_alpha(pair_compare = TRUE, shape = "Treatment")
T_meco_Physeq_PC_2019_16s_September$res_diff


#over time 16s LGC_2019  control
meco_Physeq_PC_2019_16s_Potato_CONT

meco_Physeq_PC_2019_16s_Potato_CONT$cal_alphadiv(PD = FALSE)
T_meco_Physeq_PC_2019_16s_Potato_CONT<- trans_alpha$new(dataset = meco_Physeq_PC_2019_16s_Potato_CONT, group = "Sampling_Date")
T_meco_Physeq_PC_2019_16s_Potato_CONT$cal_diff(method = "anova")
T_meco_Physeq_PC_2019_16s_Potato_CONT$plot_alpha(measure = "Shannon")
T_meco_Physeq_PC_2019_16s_Potato_CONT$plot_alpha(measure = "Shannon")
T_meco_Physeq_PC_2019_16s_Potato_CONT$plot_alpha(pair_compare = TRUE, shape = "Sampling_Date")
T_meco_Physeq_PC_2019_16s_Potato_CONT$res_diff

# over time 16 s LGC p. oligandrum 
meco_Physeq_PC_2019_16s_Potato_Po

meco_Physeq_PC_2019_16s_Potato_Po$cal_alphadiv(PD = FALSE)
T_meco_Physeq_PC_2019_16s_Potato_Po<- trans_alpha$new(dataset = meco_Physeq_PC_2019_16s_Potato_Po, group = "Sampling_Date")
T_meco_Physeq_PC_2019_16s_Potato_Po$cal_diff(method = "anova")
T_meco_Physeq_PC_2019_16s_Potato_Po$plot_alpha(measure = "Observed")
T_meco_Physeq_PC_2019_16s_Potato_Po$plot_alpha(measure = "Shannon")
T_meco_Physeq_PC_2019_16s_Potato_Po$plot_alpha(pair_compare = FALSE, shape = "Sampling_Date")
T_meco_Physeq_PC_2019_16s_Potato_Po$res_diff


## Beta diversity of the 2019 16 s samples ## 
#July
meco_Physeq_PC_2019_16s_july
meco_Physeq_PC_2019_16s_july$cal_betadiv(unifrac = FALSE)
meco_Physeq_PC_2019_16s_july$cal_betadiv(ordination = "PCoA")
class(meco_Physeq_PC_2019_16s_july$res_ordination)
B_meco_Physeq_PC_2019_16s_july <- trans_beta$new(dataset = meco_Physeq_PC_2019_16s_july, group = "Treatment", measure = "bray")
B_meco_Physeq_PC_2019_16s_july$cal_ordination(ordination = "PCoA")
class(B_meco_Physeq_PC_2019_16s_july$res_ordination)
B_meco_Physeq_PC_2019_16s_july$plot_ordination(plot_color = "Treatment", plot_shape = "Sampling_Date", plot_type = c("point", "ellipse"))
B_meco_Physeq_PC_2019_16s_july$cal_manova(manova_all = FALSE)
B_meco_Physeq_PC_2019_16s_july$res_manova

#August
meco_Physeq_PC_2019_16s_august
meco_Physeq_PC_2019_16s_august$cal_betadiv(unifrac = FALSE)
meco_Physeq_PC_2019_16s_august$cal_betadiv(ordination = "PCoA")
class(meco_Physeq_PC_2019_16s_august$res_ordination)
B_meco_Physeq_PC_2019_16s_august<- trans_beta$new(dataset = meco_Physeq_PC_2019_16s_august, group = "Treatment", measure = "bray")
B_meco_Physeq_PC_2019_16s_august$cal_ordination(ordination = "PCoA")
class(B_meco_Physeq_PC_2019_16s_august$res_ordination)
B_meco_Physeq_PC_2019_16s_august$plot_ordination(plot_color = "Treatment", plot_shape = "Sampling_Date", plot_type = c("point", "ellipse"))
B_meco_Physeq_PC_2019_16s_august$cal_manova(manova_all = TRUE)
B_meco_Physeq_PC_2019_16s_august$res_manova

#september

meco_Physeq_PC_2019_16s_September
meco_Physeq_PC_2019_16s_September$cal_betadiv(unifrac = FALSE)
meco_Physeq_PC_2019_16s_September$cal_betadiv(ordination = "PCoA")
class(meco_Physeq_PC_2019_16s_September$res_ordination)
B_meco_Physeq_PC_2019_16s_September<- trans_beta$new(dataset = meco_Physeq_PC_2019_16s_September, group = "Treatment", measure = "bray")
B_meco_Physeq_PC_2019_16s_September$cal_ordination(ordination = "PCoA")
class(B_meco_Physeq_PC_2019_16s_September$res_ordination)
B_meco_Physeq_PC_2019_16s_September$plot_ordination(plot_color = "Treatment", plot_shape = "Sampling_Date", plot_type = c("point", "ellipse"))
B_meco_Physeq_PC_2019_16s_September$cal_manova(manova_all = TRUE)
B_meco_Physeq_PC_2019_16s_September$res_manova

##control over time ## 
meco_Physeq_PC_2019_16s_Potato_CONT

meco_Physeq_PC_2019_16s_Potato_CONT$cal_betadiv(unifrac = FALSE)
meco_Physeq_PC_2019_16s_Potato_CONT$cal_betadiv(ordination = "PCoA")
class(meco_Physeq_PC_2019_16s_Potato_CONT$res_ordination)
B_meco_Physeq_PC_2019_16s_Potato_CONT<- trans_beta$new(dataset = meco_Physeq_PC_2019_16s_Potato_CONT, group = "Sampling_Date", measure = "bray")
B_meco_Physeq_PC_2019_16s_Potato_CONT$cal_ordination(ordination = "PCoA")
class(B_meco_Physeq_PC_2019_16s_Potato_CONT$res_ordination)
B_meco_Physeq_PC_2019_16s_Potato_CONT$plot_ordination(plot_color = "Sampling_Date", plot_shape = "Sampling_Date", plot_type = c("point", "ellipse"))
B_meco_Physeq_PC_2019_16s_Potato_CONT$cal_manova(manova_all = FALSE)
B_meco_Physeq_PC_2019_16s_Potato_CONT$res_manova

# p.oligandrum beta diversity over time 
meco_Physeq_PC_2019_16s_Potato_Po

meco_Physeq_PC_2019_16s_Potato_Po$cal_betadiv(unifrac = FALSE)
meco_Physeq_PC_2019_16s_Potato_Po$cal_betadiv(ordination = "PCoA")
class(meco_Physeq_PC_2019_16s_Potato_Po$res_ordination)
B_meco_Physeq_PC_2019_16s_Potato_Po<- trans_beta$new(dataset = meco_Physeq_PC_2019_16s_Potato_Po, group = "Sampling_Date", measure = "bray")
B_meco_Physeq_PC_2019_16s_Potato_Po$cal_ordination(ordination = "PCoA")
class(B_meco_Physeq_PC_2019_16s_Potato_Po$res_ordination)
B_meco_Physeq_PC_2019_16s_Potato_Po$plot_ordination(plot_color = "Sampling_Date", plot_shape = "Sampling_Date", plot_type = c("point", "ellipse"))
B_meco_Physeq_PC_2019_16s_Potato_Po$cal_manova(manova_all = FALSE)
B_meco_Physeq_PC_2019_16s_Potato_Po$res_manova



## ancom_BC for the august and september samples, 16s 

View(meco_Physeq_PC_2019_16s_august)
t1_ancombc_meco_Physeq_PC_2019_16s_august<- trans_diff$new(dataset = meco_Physeq_PC_2019_16s_august, method = "ANCOMBC", group = "Treatment", taxa_level = "Genus")
t1_ancombc_meco_Physeq_PC_2019_16s_august$sample_table$Treatment %<>% factorc(., levels = c("Control", "P. oligandrum")
t1_ancombc_meco_Physeq_PC_2019_16s_august$plot_diff_abund(use_number = 1:20, group_order = c("Control", "P. oligandrum"),  add_sig = TRUE)
t1_ancombc_meco_Physeq_PC_2019_16s_august$plot_diff_abund((use_number = 1:2),  add_sig = TRUE)
t1_ancombc_meco_Physeq_PC_2019_16s_august<- as.data.frame(t1_ancombc_meco_Physeq_PC_2019_16s_august[["res_diff"]])
write.csv(t1_ancombc_meco_Physeq_PC_2019_16s_august, file = "t1_ancombc_meco_Physeq_PC_2019_16s_august.csv")
View(t1_ancombc_meco_Physeq_PC_2019_16s_august)

# september 
meco_Physeq_PC_2019_16s_September
t1_ancombc_meco_Physeq_PC_2019_16s_September<- trans_diff$new(dataset = meco_Physeq_PC_2019_16s_September, method = "ANCOMBC", group = "Treatment", taxa_level = "Genus")
t1_ancombc_meco_Physeq_PC_2019_16s_September$plot_diff_abund(use_number = 1:20, group_order = c("Control", "P. oligandrum"),  add_sig = TRUE)
t1_ancombc_meco_Physeq_PC_2019_16s_September$plot_diff_abund((use_number = 1:2),  add_sig = TRUE)
t1_ancombc_meco_Physeq_PC_2019_16s_September<- as.data.frame(t1_ancombc_meco_Physeq_PC_2019_16s_September[["res_diff"]])
write.csv(t1_ancombc_meco_Physeq_PC_2019_16s_September, file = "t1_ancombc_meco_Physeq_PC_2019_16s_august.csv")
View(t1_ancombc_meco_Physeq_PC_2019_16s_September)

##ancombc control over time 2019 16s # 

t1_ancombc_meco_Physeq_PC_2019_16s_Potato_cont<- trans_diff$new(dataset = meco_Physeq_PC_2019_16s_Potato_CONT, method = "ANCOMBC", group = "Sampling_Date", taxa_level = "Genus")
t1_ancombc_meco_Physeq_PC_2019_16s_Potato_cont$plot_diff_abund((use_number = 1:20),  add_sig = TRUE)
t1_ancombc_meco_Physeq_PC_2019_16s_Potato_cont<- as.data.frame(t1_ancombc_meco_Physeq_PC_2019_16s_Potato_cont[["res_diff"]])
write.csv(t1_ancombc_meco_Physeq_PC_2019_16s_Potato_cont, file = "t1_2019_16s_Potato_cont_done.csv")
View(t1_ancombc_meco_Physeq_PC_2019_16s_Potato_cont)
View(meco_Physeq_PC_2019_ITS_September)  


## ancombc p. oligandrum over time 2019 16s # 
meco_Physeq_PC_2019_16s_Potato_Po

t1_ancombc_meco_Physeq_PC_2019_16s_Potato_Po<- trans_diff$new(dataset = meco_Physeq_PC_2019_16s_Potato_Po, method = "ANCOMBC", group = "Sampling_Date", taxa_level = "Genus")
t1_ancombc_meco_Physeq_PC_2019_16s_Potato_Po%<>% subset(Significance %in% "***")
t1_ancombc_meco_Physeq_PC_2019_16s_Potato_Po$plot_diff_abund((use_number = 1:22),  add_sig = TRUE)
t1_ancombc_meco_Physeq_PC_2019_16s_Potato_Po<- as.data.frame(t1_ancombc_meco_Physeq_PC_2019_16s_Potato_Po[["res_diff"]])
write.csv(t1_ancombc_meco_Physeq_PC_2019_16s_Potato_Po, file = "t1_2019_16s_Potato_Po.csv")
View(t1_ancombc_meco_Physeq_PC_2019_16s_Potato_Po)
View(meco_Physeq_PC_2019_ITS_September)  

#rda august
View(meco_Physeq_PC_2019_16s_august)
t1_meco_Physeq_PC_2019_16s_august_rda <- trans_env$new(dataset = meco_Physeq_PC_2019_16s_august, add_data = meta_adjust_2019[, 7:8])
t1_meco_Physeq_PC_2019_16s_august_rda$cal_ordination(method = "dbRDA", use_measure = "bray")
# t1$res_rda is the result list stored in the object
t1_meco_Physeq_PC_2019_16s_august_rda$trans_ordination(adjust_arrow_length = TRUE, max_perc_env = 1.5)
View(t1_meco_Physeq_PC_2019_16s_august_rda)
# t1$res_rda_trans is the transformed result for plotting
t1_meco_Physeq_PC_2019_16s_august_rda$plot_ordination(plot_color = "Treatment")
View(t1_meco_Physeq_PC_2019_16s_august_rda)


#rda september
View(meco_Physeq_PC_2019_16s_September)
t1_meco_Physeq_PC_2019_16s_September_rda <- trans_env$new(dataset = meco_Physeq_PC_2019_16s_September, add_data = meta_adjust_2019[, 7:8])
t1_meco_Physeq_PC_2019_16s_September_rda$cal_ordination(method = "dbRDA", use_measure = "bray")
# t1$res_rda is the result list stored in the object
t1_meco_Physeq_PC_2019_16s_September_rda$trans_ordination(adjust_arrow_length = TRUE, max_perc_env = 1.5)
View(t1_meco_Physeq_PC_2019_16s_September_rda)
# t1$res_rda_trans is the transformed result for plotting
t1_meco_Physeq_PC_2019_16s_September_rda$plot_ordination(plot_color = "Treatment")

View(t1_meco_Physeq_PC_2019_16s_September_rda)







