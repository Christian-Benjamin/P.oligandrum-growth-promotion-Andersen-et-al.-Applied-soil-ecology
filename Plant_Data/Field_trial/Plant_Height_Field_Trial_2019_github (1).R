## final plant height 2019 field trial , PGP #

Plant_height_2019_field_trial  <- read.csv2("/Users/christian/Desktop/r-projekter/Final_2019_plant_height.csv")
View(Plant_height_2019_field_trial)

Plant_height_Field_2019_AOV = aov(Plant_Height_September ~ Treatment , data = Plant_height_2019_field_trial)
summary(Plant_height_Field_2019_AOV)
qqnorm(residuals(Plant_height_Field_2019_AOV))
plot(residuals(Plant_height_Field_2019_AOV))
shapiro.test(residuals(Plant_height_Field_2019_AOV))
pwc_Plant_height_Field_trial_2019<- Plant_height_2019_field_trial %>% tukey_hsd(Plant_Height_September ~ Treatment)
pwc_Plant_height_Field_trial_2019

#plot the data

ggplot(Plant_height_2019_field_trial, aes(x = Treatment, y = Plant_Height_September, fill = Treatment)) +
  geom_boxplot() +
  geom_jitter(shape = 15,
              color = "steelblue",
              position = position_jitter(0.21)) + 
  theme_minimal() + 
  geom_signif(comparisons = list(c("Control", "P. oligandrum")), map_signif_level = TRUE) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18)) +
  ylab("Plant height cm") +
  guides(fill = FALSE)









#try with significance 

ggplot(Plant_height_2019_field_trial, aes(x = Treatment, y = Plant_Height_September, fill = Treatment)) +
  geom_boxplot() +
  geom_jitter(shape = 15,
              color = "steelblue",
              position = position_jitter(0.21)) + 
  theme_minimal() + 
  geom_signif(comparisons = list(c("Control", "P. oligandrum")), map_signif_level = TRUE) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18)) +
  ylab("Plant height cm") +
  guides(fill = FALSE) +
  geom_text(aes(x = 2, y = max(Plant_height_2019_field_trial$Plant_Height_September[Plant_height_2019_field_trial$Treatment == "P. oligandrum"]) + 0.5, label = "*"),
            size = 10,
            vjust = -12.3)

