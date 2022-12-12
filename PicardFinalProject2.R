setwd('desktop')
library(tidyverse)
library(ggplot2)
##downloading "Kangerlussuaq tundra taxa abundances" to R
plant_data <- read_csv(
  "Kangerlussuaq_tundra_taxa_annual_abundances_2003_2017 (1).csv")

##renaming columns
plant_data2 = as.data.frame(plant_data)
colnames(plant_data2)
colnames(plant_data2) <- c("year", "month", "date", "day_number", "exclosure_grazed_control", 
                           "exclosure_treatment", "warming_treatment", "plot_designation", "forb", 
                           "Betula", "Salix", "graminoids", "Potentilla", "Cerastium", "Polygonum", 
                           "Stellaria", "Draba", "Pyrola", "Viola", "Campanula", "Equisetum", "mosses", 
                           "lichens", "fungi", "ercoids")


##renaming treatments
plant_data3 <- plant_data2 %>%
  mutate_at(vars(warming_treatment), ~ str_replace(., "A", "Ambient")) %>% 
  mutate_at(vars(warming_treatment), ~ str_replace(., "W", "Warming")) %>% 
  mutate_at(vars(exclosure_treatment), ~ str_replace(., "E", "Exclosed")) %>% 
  mutate_at(vars(exclosure_treatment), ~ str_replace(., "G", "Grazed"))

##eliminating any "NA" values
plant_data3 = plant_data3 %>% filter(!is.na(Potentilla)) %>% 
  filter(!is.na(Cerastium)) %>% 
  filter(!is.na(Polygonum)) %>% 
  filter(!is.na(Stellaria)) %>% 
  filter(!is.na(Draba)) %>% 
  filter(!is.na(Pyrola)) %>% 
  filter(!is.na(Viola)) %>% 
  filter(!is.na(Campanula)) %>% 
  filter(!is.na(Equisetum)) %>% 
  filter(!is.na(mosses))

##narrowing data to no warming treatment and plot designation C
plant_data5 = plant_data3 %>% filter(warming_treatment == "Ambient") %>% 
  filter(plot_designation == "C")

library(tidyverse)

##saving table
plant_data6 = as.data.frame(plant_data5)
write_csv(plant_data6, path = "final/plant_data.csv")

##making data numeric for PCA
plant_data_numeric = plant_data5 %>% select(where(is.numeric)) 

##eliminatng rows with date and any low outliers (mean < 1) for PCA
summary(plant_data3)
#potentilla, cerastium, polygonum, stellaria, draba, pyrola, 
#viola, campanula, lichens, fungi, ercoids
colnames(plant_data3)
plant_data_numeric2 = plant_data_numeric[, -c(1, 2, 3, 9, 10, 11, 12, 13, 
                                              14, 15, 16, 19, 20, 21, 22, 23)]

##saving data table used for ordination plot
plant_data_numeric3 = as.data.frame(plant_data_numeric2)
write_csv(plant_data_numeric3, path = "final/plant_data_numeric.csv")


plant_stdev <- scale(plant_data_numeric2, center = T, scale = T)

plant_pca <- prcomp(na.omit(plant_stdev))

##Eliminating data w/ mean < 1 for data going into ordination plot:
summary(plant_data3)
##Means < 1: potentilla, cerastium, polygonum, stellaria, draba, pyrola, 
#viola, campanula, lichens, fungi, ercoids
colnames(plant_data3)

plant_data4 <- plant_data3[, -c(1, 2, 3, 13, 14, 15, 16, 17, 18, 19, 20, 
                                23, 24, 25, 26, 27)]

##eliminating warming treatment and narrowing to plot designation C
#for data going into ordination plot

plant_data4 = plant_data4 %>% filter(warming_treatment == "Ambient") %>% 
  filter(plot_designation == "C")


##Making arrows for ordination plot
# Get principal loadings of first two components
plant_data4$PC1 <- predict(plant_pca)[,1]
plant_data4$PC2 <- predict(plant_pca)[,2]

# Get arrow end point locations (makes the arrows easier to see by lengthening them)
l.x <- plant_pca$rotation[,1]*4
l.y <- plant_pca$rotation[,2]*4

# Get label positions (%25 further than end of arrows)
l.posx <- l.x*1.5
l.posy <- l.y*1.5

# Get labels for arrows on plot (variable names)
labels <- row.names(plant_pca$rotation)
colnames(plant_data4)
labels <- c("days", "forb sum", "Betula nana", "Salix glauca", 
            "graminoids", "Equisetum sp", "mosses")

##This plot is only looking at plant abundances with no warming treatment
#and in plot designation C
ggplot() +
  geom_point(data = plant_data4, aes(PC1, PC2, color = exclosure_treatment), size = 0.5) +
  geom_segment(aes(x=0, y=0, xend = l.x, yend = l.y), 
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
               color = "darkorange4") +
  geom_text(aes(x = l.posx, y = l.posy, label = labels), 
            color = "darkorange4", size = 3, hjust = 0) + 
  theme_grey(base_size = 14) +
  geom_hline(yintercept = 0,
             linetype = "dashed", color = "black") +
  geom_vline(xintercept=0,
             linetype="dashed", color="black") +
  scale_color_discrete(name= "Treatment") +
  labs(main = "Plant Abundance Ordination Plot") +
  xlim(-4, 4) + 
  ylim(-4, 4)
theme_classic()

library(ggplot2)
ggsave("plant_biplot.pdf", plot=last_plot(), device=NULL, path=NULL,
       scale=1, width=6, height=4, dpi=300, limitsize=TRUE)

##filtering data from Bashful control and plot C
Bashful_data <- plant_data3 %>% filter(exclosure_grazed_control == "Bashful") %>% 
  filter(plot_designation == "C")

##plotting so plot points represent grazing treatment and temperature treatments
##plotting Betula nana because the ordination plot shows that grows more in areas 
#that are not grazed
ggplot(data = Bashful_data, aes(x = day_number, y = Betula,
                                color = exclosure_treatment, shape = warming_treatment)) +
  geom_point(size = 1.5) +
  scale_color_discrete(name= "Treatment") +
  scale_shape_discrete(name = "Temperature") +
  labs(title = "Tundra Betula nana abundances around Bashful enclosure") +
  xlab("Time") + 
  ylab("Betula nana Abundances") +
  xlim(195, 235)
theme_grey(base_size = 14)
##this graph shows that Betula nana is likely preferentially grazed

ggsave("Betula_plot.pdf", plot=last_plot(), device=NULL, path=NULL,
       scale=1, width=6, height=4, dpi=300, limitsize=TRUE)

##plotting so plot points represent grazing treatment and temperature treatments
#for graminoids
##based on the ordination plot, graminoids seem to grow more in areas that are grazed
ggplot(data = Bashful_data, aes(x = day_number, y = graminoids,
                                color = exclosure_treatment, shape = warming_treatment)) +
  geom_point(size = 1.5) +
  scale_color_discrete(name= "Treatment") +
  scale_shape_discrete(name = "Temperature") +
  labs(title = "Tundra graminoid abundances around Bashful enclosure") +
  xlab("Time") + 
  ylab("Graminoid Abundances") +
  xlim(195, 235)
theme_grey(base_size = 14)
##this graph shows that graminoids thrive in areas that are grazed, likely 
#because the are not preferred by grazers

ggsave("graminoid_plot.pdf", plot=last_plot(), device=NULL, path=NULL,
       scale=1, width=6, height=4, dpi=300, limitsize=TRUE)

##based on ordination plot, forb sum appears to be mostly unaffected by grazing
ggplot(data = Bashful_data, aes(x = day_number, y = forb,
                                color = exclosure_treatment, shape = warming_treatment)) +
  geom_point(size = 1.5) +
  scale_color_discrete(name= "Treatment") +
  scale_shape_discrete(name = "Temperature") +
  labs(title = "Tundra Forb sum abundances around Bashful enclosure") +
  xlab("Time") + 
  ylab("Forb sum Abundances") +
  xlim(195, 235)
theme_grey(base_size = 14)

ggsave("forb_plot.pdf", plot=last_plot(), device=NULL, path=NULL,
       scale=1, width=6, height=4, dpi=300, limitsize=TRUE)

