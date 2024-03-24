                            
                    # Pythium oligandrum induces growth promotion in starch potato without significantly altering the rhizosphere microbiome 

#Christian B. Andersen1*,  Kristin Aleklett1,2, Garima Digdarshika , Åsa Lankinen1*, Laura J. Grenville-Briggs1*
 # 1Department of Plant Protection Biology, Swedish University of Agricultural Sciences, Alnarp, Sweden,
´#2Current affiliation: Department of Biology, Lund University, Lund, Sweden
#*corresponding author 

#_____________________________________________________________________________________________________________________________________________________________#

 #Plant phenotypic data analysis 
#load the following packages 
library(car)
library(dplyr)
library(rstatix)
library(ggplot2)
library(ggsignif)
library(agricolae)

# Genotype cv. Kuras 

# Load the data 
Kuras_greenhouse_final <- read.csv("C:/Users/Aron/Desktop/PGP_data_comparison/Kuras_Final_PGP.csv")
View(Kuras_greenhouse_final)

#standard anova and check for normal distribution 
GH_aov_Kuras = aov(Plant_height ~ Treatment + Experiment, data = Kuras_greenhouse_final)
summary(GH_aov_Kuras)
qqnorm(residuals(GH_aov_Kuras))
hist(residuals(GH_aov_Kuras))
plot(residuals(GH_aov_Kuras))
shapiro.test(residuals(GH_aov_Kuras))
#compute ad hoc test
pwc_Sept_AVR<- Kuras_greenhouse_final %>% tukey_hsd(Plant_height ~ Treatment + Experiment)
pwc_Sept_AVR

#plot the data 

ggplot(Kuras_greenhouse_final, aes(x = Treatment, y = Plant_height, fill = Treatment)) +
  geom_boxplot() +
  geom_jitter(shape = 15,
              color = "steelblue",
              position = position_jitter(0.21)) +
  theme_minimal() +
  labs(y = "Plant height in Cm.") +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.y = element_text(face = "bold"))


## fresh weight shoot 
GH_aov_Kuras_FWS = aov(FW_Shoots ~ Treatment + Experiment, data = Kuras_greenhouse_final)
summary(GH_aov_Kuras_FWS)
qqnorm(residuals(GH_aov_Kuras_FWS))
hist(residuals(GH_aov_Kuras_FWS))
plot(residuals(GH_aov_Kuras_FWS))
shapiro.test(residuals(GH_aov_Kuras_FWS))

pwc_Sept_FW_shoots<- Kuras_greenhouse_final %>% tukey_hsd(FW_Shoots ~ Treatment + Experiment)
pwc_Sept_FW_shoots

#plot the data
ggplot(Kuras_greenhouse_final, aes(x = Treatment, y = FW_Shoots, fill = Treatment)) +
  geom_boxplot() +
  geom_jitter(shape = 15,
              color = "steelblue",
              position = position_jitter(0.21)) +
  theme_minimal() +
  labs(y = "Fresh weight shoots in grams") +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.y = element_text(face = "bold"))

## dry weight shoot kuras has to be log transformed to get normal distribution 

GH_aov_Kuras_DWS = aov(DW_Shoots_log ~ Treatment + Experiment , data = Kuras_greenhouse_final)
summary(GH_aov_Kuras_DWS)
qqnorm(residuals(GH_aov_Kuras_DWS))
hist(residuals(GH_aov_Kuras_DWS))
plot(residuals(GH_aov_Kuras_DWS))
shapiro.test(residuals(GH_aov_Kuras_DWS))

pwc_Sept_DW_shoots<- Kuras_greenhouse_final %>% tukey_hsd(DW_Shoots_log ~ Treatment + Experiment)
pwc_Sept_DW_shoots

#Plot the data 
ggplot(Kuras_greenhouse_final, aes(x = Treatment, y = DW_Shoots_log, fill = Treatment)) +
  geom_boxplot() +
  geom_jitter(shape = 15,
              color = "steelblue",
              position = position_jitter(0.21)) +
  theme_minimal() +
  labs(y = "Log(Dry weighed shoots in grams)") +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.y = element_text(face = "bold"))
        
###kuras Roots
GH_aov_Kuras_FWR = aov(FW_Roots_log ~ Treatment + Experiment, data = Kuras_greenhouse_final)
summary(GH_aov_Kuras_FWR)
qqnorm(residuals(GH_aov_Kuras_FWR))
hist(residuals(GH_aov_Kuras_FWR))
plot(residuals(GH_aov_Kuras_FWR))
shapiro.test(residuals(GH_aov_Kuras_FWR))

pwc_Sept_DW_shoots<- Kuras_greenhouse_final %>% tukey_hsd(FW_Roots_log ~ Treatment + Experiment)
pwc_Sept_DW_shoots

#Plot the data 
ggplot(Kuras_greenhouse_final, aes(x = Treatment, y = FW_Roots_log, fill = Treatment)) +
  geom_boxplot() +
  geom_jitter(shape = 15,
              color = "steelblue",
              position = position_jitter(0.21)) +
  theme_minimal() +
  labs(y = "Log(Fresh weighed of roots in grams)") +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.y = element_text(face = "bold"))

#### Dw_roots_sqrt Kuras 

GH_aov_Kuras_DWR = aov(Sqrt_DW_Roots ~ Treatment + Experiment, data = Kuras_greenhouse_final)
summary(GH_aov_Kuras_DWR)
qqnorm(residuals(GH_aov_Kuras_DWR))
hist(residuals(GH_aov_Kuras_DWR))
plot(residuals(GH_aov_Kuras_DWR))
shapiro.test(residuals(GH_aov_Kuras_DWR))
pwc_Sept_DW_roots_sqrt<- Kuras_greenhouse_final %>% tukey_hsd(Sqrt_DW_Roots ~ Treatment + Experiment)
pwc_Sept_DW_roots_sqrt

#Plot the data 
ggplot(Kuras_greenhouse_final, aes(x = Treatment, y = Sqrt_DW_Roots, fill = Treatment)) +
  geom_boxplot() +
  geom_jitter(shape = 15,
              color = "steelblue",
              position = position_jitter(0.21)) +
  theme_minimal() +
  labs(y = "Sqrt.(Dry weighed of roots in grams)") +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.y = element_text(face = "bold"))

                                                                   ### Genotype Cv. Desiree ###

Desiree_greenhouse_final <- read.csv("C:/Users/Aron/Desktop/PGP_data_comparison/Desiree_Final_PGP.csv")
View(Desiree_greenhouse_final)
GH_aov_Desiree = aov(Plant_height ~ Treatment + Experiment, data = Desiree_greenhouse_final)
summary(GH_aov_Desiree)
qqnorm(residuals(GH_aov_Desiree))
hist(residuals(GH_aov_Desiree))
plot(residuals(GH_aov_Desiree))
shapiro.test(residuals(GH_aov_Desiree))
pwc_Desiree<- Desiree_greenhouse_final %>% tukey_hsd(Plant_height ~ Treatment + Experiment)
pwc_Desiree

#Plot the data 
ggplot(Desiree_greenhouse_final, aes(x = Treatment, y = Plant_height, fill = Treatment)) +
  geom_boxplot() +
  geom_jitter(shape = 15,
              color = "steelblue",
              position = position_jitter(0.21)) +
  theme_minimal() +
  labs(y = "Plant height in Cm.") +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.y = element_text(face = "bold"))
## fresh weight shoot 
GH_aov_Desiree_FWS = aov(FW_Shoots ~ Treatment + Experiment, data = Desiree_greenhouse_final)
summary(GH_aov_Desiree_FWS)
qqnorm(residuals(GH_aov_Desiree_FWS))
hist(residuals(GH_aov_Desiree_FWS))
plot(residuals(GH_aov_Desiree_FWS))
shapiro.test(residuals(GH_aov_Desiree_FWS))
pwc_Desiree_FW_shoots<- Desiree_greenhouse_final %>% tukey_hsd(FW_Shoots ~ Treatment + Experiment)
pwc_Desiree_FW_shoots

#Plot the data
ggplot(Desiree_greenhouse_final, aes(x = Treatment, y = FW_Shoots, fill = Treatment)) +
  geom_boxplot() +
  geom_jitter(shape = 15,
              color = "steelblue",
              position = position_jitter(0.21)) +
  theme_minimal() +
  labs(y = "Fresh weighed of shoots in grams") +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.y = element_text(face = "bold"))

## dry weight shoot Desiree has to be log transformed to get normal distribution 

GH_aov_Desiree_DWS = aov(DW_Shoots_log ~ Treatment + Experiment , data = Desiree_greenhouse_final)
summary(GH_aov_Desiree_DWS)
qqnorm(residuals(GH_aov_Desiree_DWS))
hist(residuals(GH_aov_Desiree_DWS))
plot(residuals(GH_aov_Desiree_DWS))
shapiro.test(residuals(GH_aov_Desiree_DWS))
pwc_Desiree_DW_shoots<- Desiree_greenhouse_final %>% tukey_hsd(DW_Shoots_log ~ Treatment + Experiment)
pwc_Desiree_DW_shoots

#Plot the data
ggplot(Desiree_greenhouse_final, aes(x = Treatment, y = DW_Shoots_log, fill = Treatment)) +
  geom_boxplot() +
  geom_jitter(shape = 15,
              color = "steelblue",
              position = position_jitter(0.21)) +
  theme_minimal() +
  labs(y = "Log(Dry weighed of shoots in grams)") +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.y = element_text(face = "bold"))

###Desiree Roots
GH_aov_Desiree_FWR = aov(FW_Roots_log ~ Treatment + Experiment, data = Desiree_greenhouse_final)
summary(GH_aov_Desiree_FWR)
qqnorm(residuals(GH_aov_Desiree_FWR))
hist(residuals(GH_aov_Desiree_FWR))
plot(residuals(GH_aov_Desiree_FWR))
shapiro.test(residuals(GH_aov_Desiree_FWR))
pwc_Desiree_Fw_Roots<- Desiree_greenhouse_final %>% tukey_hsd(FW_Roots_log ~ Treatment + Experiment)
pwc_Desiree_Fw_Roots

#
ggplot(Desiree_greenhouse_final, aes(x = Treatment, y = FW_Roots_log, fill = Treatment)) +
  geom_boxplot() +
  geom_jitter(shape = 15,
              color = "steelblue",
              position = position_jitter(0.21)) +
  theme_minimal() +
  labs(y = "Fresh weight in grams") +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.y = element_text(face = "bold"))
 
##Dw roots Desiree

GH_aov_Desiree_DWR = aov(DW_Roots_log ~ Treatment + Experiment, data = Desiree_greenhouse_final)
summary(GH_aov_Desiree_DWR)
qqnorm(residuals(GH_aov_Desiree_DWR))
hist(residuals(GH_aov_Desiree_DWR))
plot(residuals(GH_aov_Desiree_DWR))
shapiro.test(residuals(GH_aov_Desiree_DWR))
pwc_Desiree_DW_Roots<- Desiree_greenhouse_final %>% tukey_hsd(DW_Roots_log ~ Treatment + Experiment)
pwc_Desiree_DW_Roots

#Plot the data
ggplot(Desiree_greenhouse_final, aes(x = Treatment, y = DW_Roots_log, fill = Treatment)) +
  geom_boxplot() +
  geom_jitter(shape = 15,
              color = "steelblue",
              position = position_jitter(0.21)) +
  theme_minimal() +
  labs(y = "Log(Dry weight in grams)") +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.y = element_text(face = "bold"))

                                                ## Genotype Cv. King Edward 

King_Edward_greenhouse_final <- read.csv("C:/Users/Aron/desktop/PGP_data_comparison/KingEdward_final_PGP.csv")
View(King_Edward_greenhouse_final)
GH_aov_King_Edward = aov(Plant_height ~ Treatment + Experiment, data = King_Edward_greenhouse_final)
summary(GH_aov_King_Edward)
qqnorm(residuals(GH_aov_King_Edward))
hist(residuals(GH_aov_King_Edward))
plot(residuals(GH_aov_King_Edward))
shapiro.test(residuals(GH_aov_King_Edward))
pwc_King_Edward_AVR<- King_Edward_greenhouse_final %>% tukey_hsd(Plant_height ~ Treatment + Experiment)
pwc_King_Edward_AVR

#Plot the data
ggplot(King_Edward_greenhouse_final, aes(x = Treatment, y = Plant_height, fill = Treatment)) +
  geom_boxplot() +
  geom_jitter(shape = 15,
              color = "steelblue",
              position = position_jitter(0.21)) +
  theme_minimal() +
  labs(y = "Plant height in Cm.") +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.y = element_text(face = "bold"))


## fresh weight shoot 
GH_aov_King_Edward_FWS = aov(FW_Shoots ~ Treatment + Experiment, data = King_Edward_greenhouse_final)
summary(GH_aov_King_Edward_FWS)
qqnorm(residuals(GH_aov_King_Edward_FWS))
hist(residuals(GH_aov_King_Edward_FWS))
plot(residuals(GH_aov_King_Edward_FWS))
shapiro.test(residuals(GH_aov_King_Edward_FWS))
pwc_King_Edward_FW_shoots<- King_Edward_greenhouse_final %>% tukey_hsd(FW_Shoots ~ Treatment + Experiment)
pwc_King_Edward_FW_shoots

#Plot the data 
ggplot(King_Edward_greenhouse_final, aes(x = Treatment, y = FW_Shoots, fill = Treatment)) +
  geom_boxplot() +
  geom_jitter(shape = 15,
              color = "steelblue",
              position = position_jitter(0.21)) +
  theme_minimal() +
  labs(y = "Fresh weighed in grams") +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.y = element_text(face = "bold"))

## dry weight shoot King_Edward has to be log transformed to get normal distribution 

GH_aov_King_Edward_DWS = aov(DW_Shoots_log ~ Treatment + Experiment , data = King_Edward_greenhouse_final)
summary(GH_aov_King_Edward_DWS)
qqnorm(residuals(GH_aov_King_Edward_DWS))
hist(residuals(GH_aov_King_Edward_DWS))
plot(residuals(GH_aov_King_Edward_DWS))
shapiro.test(residuals(GH_aov_King_Edward_DWS))
pwc_King_Edward_DW_shoots<- King_Edward_greenhouse_final %>% tukey_hsd(DW_Shoots_log ~ Treatment + Experiment)
pwc_King_Edward_DW_shoots

#Plot the data 
ggplot(King_Edward_greenhouse_final, aes(x = Treatment, y = DW_Shoots_log, fill = Treatment)) +
  geom_boxplot() +
  geom_jitter(shape = 15,
              color = "steelblue",
              position = position_jitter(0.21)) +
  theme_minimal() +
  labs(y = "Log(Dry weighed of shoots in grams)") +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.y = element_text(face = "bold"))

#King_Edward Roots fresh weighed
GH_aov_King_Edward_FWR = aov(FW_Roots_log ~ Treatment + Experiment, data = King_Edward_greenhouse_final)
summary(GH_aov_King_Edward_FWR)
qqnorm(residuals(GH_aov_King_Edward_FWR))
hist(residuals(GH_aov_King_Edward_FWR))
plot(residuals(GH_aov_King_Edward_FWR))
shapiro.test(residuals(GH_aov_King_Edward_FWR))
pwc_King_Edward_Fw_Roots<- King_Edward_greenhouse_final %>% tukey_hsd(FW_Roots_log ~ Treatment + Experiment)
pwc_King_Edward_Fw_Roots

#Plot the data
ggplot(King_Edward_greenhouse_final, aes(x = Treatment, y = FW_Roots_log, fill = Treatment)) +
  geom_boxplot() +
  geom_jitter(shape = 15,
              color = "steelblue",
              position = position_jitter(0.21)) +
  theme_minimal() +
  labs(y = "Log(Fresh weighed of roots in grams)") +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.y = element_text(face = "bold"))

##Dw roots King Edward

GH_aov_King_Edward_DWR = aov(DW_Roots_log ~ Treatment + Experiment, data = King_Edward_greenhouse_final)
summary(GH_aov_King_Edward_DWR)
qqnorm(residuals(GH_aov_King_Edward_DWR))
hist(residuals(GH_aov_King_Edward_DWR))
plot(residuals(GH_aov_King_Edward_DWR))
shapiro.test(residuals(GH_aov_King_Edward_DWR))
pwc_King_Edward_DW_Roots<- King_Edward_greenhouse_final %>% tukey_hsd(DW_Roots_log ~ Treatment + Experiment)
pwc_King_Edward_DW_Roots

#Plot the data 
ggplot(King_Edward_greenhouse_final, aes(x = Treatment, y = DW_Roots_log, fill = Treatment)) +
  geom_boxplot() +
  geom_jitter(shape = 15,
              color = "steelblue",
              position = position_jitter(0.21)) +
  theme_minimal() +
  labs(y = "Log(Dry weight in grams)") +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.y = element_text(face = "bold"))















