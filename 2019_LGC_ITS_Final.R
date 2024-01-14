

/Users/christian/Desktop/physeq 2019 /Physeq_2019_ITS.RDS
Physeq_PC_2019_ITS <- readRDS("/Users/christian/Desktop/physeq 2019 /Physeq_2019_ITS.RDS")
OTU_2019_ITS <- as.data.frame(otu_table(Physeq_PC_2019_ITS))
write.csv2(OTU_2019_ITS, file = "OTU_2019_ITS.csv")

Tax_2019_ITS <- as.data.frame(tax_table(Physeq_PC_2019_ITS))
write.csv2(Tax_2019_ITS, file = "Tax_2019_ITS.csv")

meta_2019_ITS <- as.data.frame(sample_data(Physeq_PC_2019_ITS))
write.csv2(meta_2019_ITS, file = "meta_2019_ITS.csv")
/Users/christian/Desktop/physeq 2019 /meta_2019_ITS.csv
meta_adjust_2019_ITS <- read.csv2("/Users/christian/Desktop/physeq 2019 /meta_2019_ITS.csv", row = 1)
sample_data(Physeq_PC_2019_ITS) <- meta_adjust_2019_ITS
View(Physeq_PC_2019_ITS)
/Users/christian/Desktop/physeq 2019 /OTU_2019_ITS.csv
OTU_2019_ITS<- as.matrix(read.csv2("/Users/christian/Desktop/physeq 2019 /OTU_2019_ITS.csv", row = 1))
Tax_2019_ITS<- as.matrix(read.csv2("/Users/christian/Desktop/physeq 2019 /Tax_2019_ITS.csv", row = 1))
meta_2019_ITS <- read.csv2("/Users/christian/Desktop/Meta_adjust_PGP_2019_Fungi_Final.csv", row = 1)
View(meta_2019_ITS)
View(OTU_2019_ITS)
SAM = sample_data(meta_2019_ITS, errorIfNULL = T)
OTU = otu_table(OTU_2019_ITS, taxa_are_rows = TRUE)
TAX = tax_table(Tax_2019_ITS)

Physeq_PC_2019_ITS = phyloseq(OTU, TAX, SAM)
Physeq_PC_2019_ITS

View(Physeq_PC_2019_ITS)

phyllo_ITS_mocks

Physeq_PC_2019_ITS_and_mock <- readRDS("/Users/christian/Desktop/PGP_Microbiome_Final/Fungi/Physeq_PC_2019_ITS_and_mock.RDS")
meco_Physeq_PC_2019_ITS_and_mock <- phyloseq2meco(Physeq_PC_2019_ITS_and_mock)
meco_Physeq_PC_2019_ITS_and_mock
meta_2019_ITS_adjust <- as.matrix(sample_data(Physeq_PC_2019_ITS_and_mock))

write.csv(meta_2019_ITS_adjust, file = "meta_2019_ITS_adjust.csv")
meta_adjust_2019_fungi <- read.csv2("/Users/christian/Desktop/Meta_adjust_PGP_2019_Fungi_Final.csv", row = 1)
sample_data(Physeq_PC_2019_ITS_and_mock) <- meta_adjust_2019_fungi
View(Physeq_PC_2019_ITS_and_mock)

saveRDS(Physeq_PC_2019_ITS_and_mock, file ="Physeq_PC_2019_ITS_and_mock.RDS")
Physeq_PC_2019_ITS_and_mock

View(Physeq_PC_2019_ITS_and_mock)
Physeq_PC_2019_ITS_and_mock <- subset_samples(Physeq_PC_2019_ITS_and_mock, Species == "Potato_Kuras")
Physeq_PC_2019_ITS_and_mock
Physeq_PC_2019_ITS_and_mock_cont <- subset_samples(Physeq_PC_2019_ITS_and_mock, Treatment =="Control")
Physeq_PC_2019_ITS_and_mock_cont

Physeq_PC_2019_ITS_july <- subset_samples(Physeq_PC_2019_ITS_and_mock, Sampling_Date == "July")
Physeq_PC_2019_ITS_july
Physeq_PC_2019_ITS_august <- subset_samples(Physeq_PC_2019_ITS_and_mock, Sampling_Date == "August")
Physeq_PC_2019_ITS_august
Physeq_PC_2019_ITS_September <- subset_samples(Physeq_PC_2019_ITS_and_mock, Sampling_Date == "September")
Physeq_PC_2019_ITS_September

#control over time 
Physeq_PC_2019_ITS_and_mock_cont <- subset_samples(Physeq_PC_2019_ITS_and_mock, Treatment =="Control")
Physeq_PC_2019_ITS_and_mock_cont

meco_Physeq_PC_2019_ITS_and_mock_cont <- phyloseq2meco(Physeq_PC_2019_ITS_and_mock_cont)
meco_Physeq_PC_2019_ITS_and_mock_cont$tidy_dataset()
meco_Physeq_PC_2019_ITS_and_mock_cont
# p. oligandrum over time 
Physeq_PC_2019_ITS_and_mock_Po <- subset_samples(Physeq_PC_2019_ITS_and_mock, Treatment =="P. oligandrum")
Physeq_PC_2019_ITS_and_mock_Po
meco_Physeq_PC_2019_ITS_and_mock_PO <- phyloseq2meco(Physeq_PC_2019_ITS_and_mock_Po)
meco_Physeq_PC_2019_ITS_and_mock_PO$tidy_dataset()
meco_Physeq_PC_2019_ITS_and_mock_PO

#make them into microeco objects 

meco_Physeq_PC_2019_ITS_Potato_cont

meco_Physeq_PC_2019_ITS_and_mock <- phyloseq2meco(Physeq_PC_2019_ITS_and_mock)
meco_Physeq_PC_2019_ITS_and_mock$tidy_dataset()
meco_Physeq_PC_2019_ITS_and_mock
meco_Physeq_PC_2019_ITS_july <- phyloseq2meco(Physeq_PC_2019_ITS_july)
meco_Physeq_PC_2019_ITS_july$tidy_dataset()
meco_Physeq_PC_2019_ITS_july
meco_Physeq_PC_2019_ITS_august <- phyloseq2meco(Physeq_PC_2019_ITS_august)
meco_Physeq_PC_2019_ITS_august$tidy_dataset()
meco_Physeq_PC_2019_ITS_September <- phyloseq2meco(Physeq_PC_2019_ITS_September)
meco_Physeq_PC_2019_ITS_September$tidy_dataset()
View(meco_Physeq_PC_2019_ITS_and_mock)

##its s plots abundancy plots

G1_Meco_2019_ITS_july <- trans_abund$new(dataset = meco_Physeq_PC_2019_ITS_july, taxrank = "Phylum", ntaxa = 10, groupmean = "Treatment")
g1_Meco_2019_ITS_july <- G1_Meco_2019_ITS_july$plot_bar(others_color = "grey70", legend_text_italic = FALSE)
g1_Meco_2019_ITS_july + theme_classic() + theme(axis.title.y = element_text(size = 15)) 
View(G1_Meco_2019_ITS_july)



G1_Meco_2019_ITS_aug <- trans_abund$new(dataset = meco_Physeq_PC_2019_ITS_august, taxrank = "Phylum", ntaxa = 10, groupmean = "Treatment")
g1_Meco_2019_ITS_aug <- G1_Meco_2019_ITS_aug$plot_bar(others_color = "grey70", legend_text_italic = FALSE)
g1_Meco_2019_ITS_aug + theme_classic() + theme(axis.title.y = element_text(size = 15))

G1_Meco_2019_ITS_sep <- trans_abund$new(dataset = meco_Physeq_PC_2019_ITS_September, taxrank = "Phylum", ntaxa = 10, groupmean = "Treatment")
g1_Meco_2019_ITS_sep <- G1_Meco_2019_ITS_sep$plot_bar(others_color = "grey70", legend_text_italic = FALSE)
g1_Meco_2019_ITS_sep + theme_classic() + theme(axis.title.y = element_text(size = 15))

#over time its LGC_2019 

meco_Physeq_PC_2019_ITS_and_mock_cont$sample_table$Sampling_Date %<>% factor(., levels = c("July",  "August", "September"))
G1_Meco_2019_ITS_Cont <- trans_abund$new(dataset = meco_Physeq_PC_2019_ITS_and_mock_cont, taxrank = "Phylum", ntaxa = 10, groupmean = "Sampling_Date")
g1_Meco_2019_ITS_Cont <- G1_Meco_2019_ITS_Cont$plot_bar(others_color = "grey70", legend_text_italic = FALSE) 
g1_Meco_2019_ITS_Cont + theme_classic() + theme(axis.title.y = element_text(size = 15)) 

## pythium oligandrum over time 2019 
meco_Physeq_PC_2019_ITS_and_mock_PO

meco_Physeq_PC_2019_ITS_and_mock_PO$sample_table$Sampling_Date %<>% factor(., levels = c("July",  "August", "September"))
G1_Meco_2019_ITS_Po <- trans_abund$new(dataset = meco_Physeq_PC_2019_ITS_and_mock_PO, taxrank = "Phylum", ntaxa = 10, groupmean = "Sampling_Date")
g1_Meco_2019_ITS_Po <- G1_Meco_2019_ITS_Po$plot_bar(others_color = "grey70", legend_text_italic = FALSE) 
g1_Meco_2019_ITS_Po + theme_classic() + theme(axis.title.y = element_text(size = 15)) 





## ITS alpha diversity plots # 

#july
meco_Physeq_PC_2019_ITS_july$cal_alphadiv(PD = FALSE)
T_meco_Physeq_PC_2019_ITS_july<- trans_alpha$new(dataset = meco_Physeq_PC_2019_ITS_july, group = "Treatment")
T_meco_Physeq_PC_2019_ITS_july$cal_diff(method = "anova")
T_meco_Physeq_PC_2019_ITS_july$plot_alpha(measure = "Shannon")
T_meco_Physeq_PC_2019_ITS_july$plot_alpha(measure = "Shannon")
T_meco_Physeq_PC_2019_ITS_july$plot_alpha(pair_compare = TRUE, shape = "Treatment")
T_meco_Physeq_PC_2019_ITS_july$res_diff

#august

meco_Physeq_PC_2019_ITS_august$cal_alphadiv(PD = FALSE)
T_meco_Physeq_PC_2019_ITS_august<- trans_alpha$new(dataset = meco_Physeq_PC_2019_ITS_august, group = "Treatment")
T_meco_Physeq_PC_2019_ITS_august$cal_diff(method = "anova")
T_meco_Physeq_PC_2019_ITS_august$plot_alpha(measure = "Shannon")
T_meco_Physeq_PC_2019_ITS_august$plot_alpha(measure = "Shannon")
T_meco_Physeq_PC_2019_ITS_august$plot_alpha(pair_compare = TRUE, shape = "Treatment")
T_meco_Physeq_PC_2019_ITS_august$res_diff

#september

meco_Physeq_PC_2019_ITS_September$cal_alphadiv(PD = FALSE)
T_meco_Physeq_PC_2019_ITS_September<- trans_alpha$new(dataset = meco_Physeq_PC_2019_ITS_September, group = "Treatment")
T_meco_Physeq_PC_2019_ITS_September$cal_diff(method = "anova")
T_meco_Physeq_PC_2019_ITS_September$plot_alpha(measure = "Observed")
T_meco_Physeq_PC_2019_ITS_September$plot_alpha(measure = "Shannon")
T_meco_Physeq_PC_2019_ITS_September$plot_alpha(pair_compare = TRUE, shape = "Treatment")
T_meco_Physeq_PC_2019_ITS_September$res_diff

#over time its LGC_2019  control

meco_Physeq_PC_2019_ITS_and_mock_cont$cal_alphadiv(PD = FALSE)
T_meco_Physeq_PC_2019_ITS_and_mock_cont<- trans_alpha$new(dataset = meco_Physeq_PC_2019_ITS_and_mock_cont, group = "Sampling_Date")
T_meco_Physeq_PC_2019_ITS_and_mock_cont$cal_diff(method = "anova")
T_meco_Physeq_PC_2019_ITS_and_mock_cont$plot_alpha(measure = "Observed")
T_meco_Physeq_PC_2019_ITS_and_mock_cont$plot_alpha(measure = "Shannon")
T_meco_Physeq_PC_2019_ITS_and_mock_cont$plot_alpha(pair_compare = TRUE, shape = "Sampling_Date")
T_meco_Physeq_PC_2019_ITS_and_mock_cont$res_diff

# over time ITS LGC p. oligandrum 
meco_Physeq_PC_2019_ITS_and_mock_PO

meco_Physeq_PC_2019_ITS_and_mock_PO$cal_alphadiv(PD = FALSE)
T_meco_Physeq_PC_2019_ITS_and_mock_Po<- trans_alpha$new(dataset = meco_Physeq_PC_2019_ITS_and_mock_PO, group = "Sampling_Date")
T_meco_Physeq_PC_2019_ITS_and_mock_Po$cal_diff(method = "anova")
T_meco_Physeq_PC_2019_ITS_and_mock_Po$plot_alpha(measure = "Observed")
T_meco_Physeq_PC_2019_ITS_and_mock_Po$plot_alpha(measure = "Shannon")
T_meco_Physeq_PC_2019_ITS_and_mock_Po$plot_alpha(pair_compare = TRUE, shape = "Sampling_Date")
T_meco_Physeq_PC_2019_ITS_and_mock_Po$res_diff



## Beta diversity of the 2019 ITS  samples ## 
#July
meco_Physeq_PC_2019_ITS_july
meco_Physeq_PC_2019_ITS_july$cal_betadiv(unifrac = FALSE)
meco_Physeq_PC_2019_ITS_july$cal_betadiv(ordination = "PCoA")
class(meco_Physeq_PC_2019_ITS_july$res_ordination)
B_meco_Physeq_PC_2019_ITS_july <- trans_beta$new(dataset = meco_Physeq_PC_2019_ITS_july, group = "Treatment", measure = "bray")
B_meco_Physeq_PC_2019_ITS_july$cal_ordination(ordination = "PCoA")
class(B_meco_Physeq_PC_2019_ITS_july$res_ordination)
B_meco_Physeq_PC_2019_ITS_july$plot_ordination(plot_color = "Treatment", plot_shape = "Sampling_Date", plot_type = c("point", "ellipse"))
B_meco_Physeq_PC_2019_ITS_july$cal_manova(manova_all = FALSE)
B_meco_Physeq_PC_2019_ITS_july$res_manova

#August
meco_Physeq_PC_2019_ITS_august
meco_Physeq_PC_2019_ITS_august$cal_betadiv(unifrac = FALSE)
meco_Physeq_PC_2019_ITS_august$cal_betadiv(ordination = "PCoA")
class(meco_Physeq_PC_2019_ITS_august$res_ordination)
B_meco_Physeq_PC_2019_ITS_august<- trans_beta$new(dataset = meco_Physeq_PC_2019_ITS_august, group = "Treatment", measure = "bray")
B_meco_Physeq_PC_2019_ITS_august$cal_ordination(ordination = "PCoA")
class(B_meco_Physeq_PC_2019_ITS_august$res_ordination)
B_meco_Physeq_PC_2019_ITS_august$plot_ordination(plot_color = "Treatment", plot_shape = "Sampling_Date", plot_type = c("point", "ellipse"))
B_meco_Physeq_PC_2019_ITS_august$cal_manova(manova_all = FALSE)
B_meco_Physeq_PC_2019_ITS_august$res_manova

#september

meco_Physeq_PC_2019_ITS_September
meco_Physeq_PC_2019_ITS_September$cal_betadiv(unifrac = FALSE)
meco_Physeq_PC_2019_ITS_September$cal_betadiv(ordination = "PCoA")
class(meco_Physeq_PC_2019_ITS_September$res_ordination)
B_meco_Physeq_PC_2019_ITS_September<- trans_beta$new(dataset = meco_Physeq_PC_2019_ITS_September, group = "Treatment", measure = "bray")
B_meco_Physeq_PC_2019_ITS_September$cal_ordination(ordination = "PCoA")
class(B_meco_Physeq_PC_2019_ITS_September$res_ordination)
B_meco_Physeq_PC_2019_ITS_September$plot_ordination(plot_color = "Treatment", plot_shape = "Sampling_Date", plot_type = c("point", "ellipse"))
B_meco_Physeq_PC_2019_ITS_September$cal_manova(manova_all = TRUE)
B_meco_Physeq_PC_2019_ITS_September$res_manova

##control over time ## 

meco_Physeq_PC_2019_ITS_and_mock_cont
meco_Physeq_PC_2019_ITS_and_mock_cont$cal_betadiv(unifrac = FALSE)
meco_Physeq_PC_2019_ITS_and_mock_cont$cal_betadiv(ordination = "PCoA")
class(meco_Physeq_PC_2019_ITS_and_mock_cont$res_ordination)
B_meco_Physeq_PC_2019_ITS_and_mock_cont<- trans_beta$new(dataset = meco_Physeq_PC_2019_ITS_and_mock_cont, group = "Sampling_Date", measure = "bray")
B_meco_Physeq_PC_2019_ITS_and_mock_cont$cal_ordination(ordination = "PCoA")
class(B_meco_Physeq_PC_2019_ITS_and_mock_cont$res_ordination)
B_meco_Physeq_PC_2019_ITS_and_mock_cont$plot_ordination(plot_color = "Sampling_Date", plot_shape = "Sampling_Date", plot_type = c("point", "ellipse"))
B_meco_Physeq_PC_2019_ITS_and_mock_cont$cal_manova(manova_all = FALSE)
B_meco_Physeq_PC_2019_ITS_and_mock_cont$res_manova

# p.oligandrum beta diversity over time 

meco_Physeq_PC_2019_ITS_and_mock_PO
meco_Physeq_PC_2019_ITS_and_mock_PO$cal_betadiv(unifrac = FALSE)
meco_Physeq_PC_2019_ITS_and_mock_PO$cal_betadiv(ordination = "PCoA")
class(meco_Physeq_PC_2019_ITS_and_mock_PO$res_ordination)
B_meco_Physeq_PC_2019_ITS_and_mock_PO<- trans_beta$new(dataset = meco_Physeq_PC_2019_ITS_and_mock_PO, group = "Sampling_Date", measure = "bray")
B_meco_Physeq_PC_2019_ITS_and_mock_PO$cal_ordination(ordination = "PCoA")
class(B_meco_Physeq_PC_2019_ITS_and_mock_PO$res_ordination)
B_meco_Physeq_PC_2019_ITS_and_mock_PO$plot_ordination(plot_color = "Sampling_Date", plot_shape = "Sampling_Date", plot_type = c("point", "ellipse"))
B_meco_Physeq_PC_2019_ITS_and_mock_PO$cal_manova(manova_all = FALSE)
B_meco_Physeq_PC_2019_ITS_and_mock_PO$res_manova


## ancom_BC for the august and september samples, ITS 
meco_Physeq_PC_2019_ITS
View(meco_Physeq_PC_2019_ITS_august)
t1_ancombc_meco_Physeq_PC_2019_ITS_august<- trans_diff$new(dataset = meco_Physeq_PC_2019_ITS_august, method = "ancombc2", group = "Treatment", taxa_level = "Species")
t1_ancombc_meco_Physeq_PC_2019_ITS_august$plot_diff_abund((use_number = 1:4),  add_sig = TRUE)
t1_ancombc_meco_Physeq_PC_2019_ITS_august<- as.data.frame(t1_ancombc_meco_Physeq_PC_2019_ITS_august[["res_diff"]])
 write.csv(t1_ancombc_meco_Physeq_PC_2019_ITS_august, file = "t1_ancombc_meco_Physeq_PC_2019_ITS_august.csv")
View(t1_ancombc_meco_Physeq_PC_2019_ITS_august)
                                                                              
                                                                              # september 
meco_Physeq_PC_2019_ITS_September
t1_ancombc_meco_Physeq_PC_2019_ITS_September<- trans_diff$new(dataset = meco_Physeq_PC_2019_ITS_September, method = "ancombc2", group = "Treatment", taxa_level = "Genus")
t1_ancombc_meco_Physeq_PC_2019_ITS_September$plot_diff_abund(use_number = 1:20, group_order = c("Control", "P. oligandrum"),  add_sig = TRUE)
t1_ancombc_meco_Physeq_PC_2019_ITS_September$plot_diff_abund((use_number = 1:10),  add_sig = TRUE)
 t1_ancombc_meco_Physeq_PC_2019_ITS_September<- as.data.frame(t1_ancombc_meco_Physeq_PC_2019_ITS_September[["res_diff"]])
 write.csv(t1_ancombc_meco_Physeq_PC_2019_ITS_September, file = "t1_ancombc_meco_Physeq_PC_2019_ITS_September.csv")
 View(t1_ancombc_meco_Physeq_PC_2019_ITS_September)
 View(meco_Physeq_PC_2019_ITS_September)  

 
 ## ancombc over time ITS 
 meco_Physeq_PC_2019_ITS_and_mock_cont
 View(meco_Physeq_PC_2019_ITS_and_mock_cont)
 t1_ancombc_meco_Physeq_PC_2019_ITS_and_mock_cont<- trans_diff$new(dataset = meco_Physeq_PC_2019_ITS_and_mock_cont, method = "ancombc2", group = "Sampling_Date", taxa_level = "OTU")
 t1_ancombc_meco_Physeq_PC_2019_ITS_and_mock_cont$plot_diff_abund((use_number = 1:18),  add_sig = TRUE)
 t1_ancombc_meco_Physeq_PC_2019_ITS_and_mock_cont<- as.data.frame(t1_ancombc_meco_Physeq_PC_2019_ITS_and_mock_cont[["res_diff"]])
 write.csv(t1_ancombc_meco_Physeq_PC_2019_ITS_and_mock_cont, file = "t1_2019_ITS_cont_ancombc.csv")
 View(t1_ancombc_meco_Physeq_PC_2019_ITS_and_mock_cont)
 View(meco_Physeq_PC_2019_ITS_September)  
 
 
 ## ancombc p.oligandrum over time 2019 
 
 meco_Physeq_PC_2019_ITS_and_mock_PO
 t1_ancombc_meco_Physeq_PC_2019_ITS_and_mock_Po<- trans_diff$new(dataset = meco_Physeq_PC_2019_ITS_and_mock_PO, method = "ancombc2", group = "Sampling_Date", taxa_level = "Genus")
 t1_ancombc_meco_Physeq_PC_2019_ITS_and_mock_Po$plot_diff_abund((use_number = 1:18),  add_sig = TRUE)
 t1_ancombc_meco_Physeq_PC_2019_ITS_and_mock_Po<- as.data.frame(t1_ancombc_meco_Physeq_PC_2019_ITS_and_mock_Po[["res_diff"]])
 write.csv(t1_ancombc_meco_Physeq_PC_2019_ITS_and_mock_Po, file = "t1_2019_ITS_Po_ancombc.csv")
 View(t1_ancombc_meco_Physeq_PC_2019_ITS_and_mock_Po)
 t1_ancombc_meco_Physeq_PC_2019_ITS_and_mock_Po
 
 
 #rda august
 View(meco_Physeq_PC_2019_ITS_august)
t1_meco_Physeq_PC_2019_ITS_august_rda <- trans_env$new(dataset = meco_Physeq_PC_2019_ITS_august, add_data = meta_adjust_2019_ITS[, 7:8])
 t1_meco_Physeq_PC_2019_ITS_august_rda$cal_ordination(method = "dbRDA", use_measure = "bray")
 # t1$res_rda is the result list stored in the object
t1_meco_Physeq_PC_2019_ITS_august_rda$trans_ordination(adjust_arrow_length = TRUE, max_perc_env = 1.5)
View(t1_meco_Physeq_PC_2019_ITS_august_rda)
 # t1$res_rda_trans is the transformed result for plotting
 t1_meco_Physeq_PC_2019_ITS_august_rda$plot_ordination(plot_color = "Treatment")
  View(t1_meco_Physeq_PC_2019_ITS_august_rda)
                                                                              
                                                                              
 #rda september
 View(meco_Physeq_PC_2019_ITS_September)
t1_meco_Physeq_PC_2019_ITS_September_rda <- trans_env$new(dataset = meco_Physeq_PC_2019_ITS_September, add_data = meta_adjust_2019_ITS[, 7:8])
t1_meco_Physeq_PC_2019_ITS_September_rda$cal_ordination(method = "dbRDA", use_measure = "bray")
# t1$res_rda is the result list stored in the object
t1_meco_Physeq_PC_2019_ITS_September_rda$trans_ordination(adjust_arrow_length = TRUE, max_perc_env = 1.5)
View(t1_meco_Physeq_PC_2019_ITS_September_rda)
 # t1$res_rda_trans is the transformed result for plotting
t1_meco_Physeq_PC_2019_ITS_September_rda$plot_ordination(plot_color = "Treatment")
View(t1_meco_Physeq_PC_2019_ITS_September_rda)

View(meco_Physeq_PC_2019_ITS_august)



t1_meco_Physeq_PC_2019_ITS_august <- trans_abund$new(dataset = meco_Physeq_PC_2019_ITS_august, taxrank = "Genus", ntaxa = 20)
t1_meco_Physeq_PC_2019_ITS_august$plot_heatmap(facet = "Treatment", xtext_keep = FALSE, withmargin = FALSE, add_sig_label = "Significance")

t1 <- trans_diff$new(dataset = meco_Physeq_PC_2019_ITS_august, method = "ALDEx2_kw", group = "Treatment", taxa_level = "Genus", filter_thres = 0.001)
# filter something not needed to show
t1$res_diff %<>% subset(Significance %in% "*")
t1$plot_diff_abund(use_number =10, add_sig = T)

View(meco_Physeq_PC_2019_ITS_august[["otu_table"]])

View(Physeq_PC_2019_ITS_and_mock)

Physeq_PC_2019_ITS_and_mock <- subset_samples(Physeq_PC_2019_ITS, Species == "Potato_Kuras")

Physeq_PC_2019_ITS_and_mock_polig <- subset_samples(Physeq_PC_2019_ITS, Treatment == "P. oligandrum " )                                                                   
Physeq_PC_2019_ITS_and_mock_cont <- subset_samples(Physeq_PC_2019_ITS, Treatment == "Control" )                                                                   

Physeq_PC_2019_ITS_and_mock_cont
Physeq_PC_2019_ITS_and_mock_polig                                 




t1_ta <- trans_diff$new(dataset = Physeq_PC_2019_ITS_and_mock, method = "anova", group = "Treatment", taxa_level = "Phylum", filter_thres = 0.001)
# filter something not needed to show
t1$res_diff %<>% subset(Significance %in% "***")
t1$plot_diff_abund(use_number = 1:10, add_sig = T, add_sig_label = "Significance")