## code propre:
# Importation des données
fulltab<- read.table("C:/Users/renax/Desktop/ACO/S9/Visualisation/Projet_Visu/Data/FULLTAB.csv")
# Importation des packages:
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(data.table)
library(RColorBrewer)

# Définition des années El Nino
el_nino_years <- c(1963,1966, 1968,1969, 1972,1973, 1976, 1980, 1982,
                   1983, 1986,1987, 1991, 1994, 1997,1998, 2002,2004, 
                   2009,2010,2014, 2016, 2019,2020)

# Chargez la bibliotheque RColorBrewer

# Definissez le nombre de niveaux de temp?rature et la palette de couleurs
color_palette <- c("lightyellow","navajowhite","sandybrown","orange1","chocolate1","red","red3","red4","#330000")
# Define the thresholds based on deciles
thresholds <- quantile(fulltab$T_meanyear, probs = seq(0.1, 0.9, by = 0.1), na.rm = TRUE)

# Update the assign_color function
assign_color <- function(temperature) {
  if (is.na(temperature)) {
    return("NA")  # or any other value you want to assign for missing data
  } else if (temperature <= thresholds[1]) {
    return(color_palette[1])
  } else if (temperature <= thresholds[2]) {
    return(color_palette[2])
  } else if (temperature <= thresholds[3]) {
    return(color_palette[3])
  } else if (temperature <= thresholds[4]) {
    return(color_palette[4])
  } else if (temperature <= thresholds[5]) {
    return(color_palette[5])
  } else if (temperature <= thresholds[6]) {
    return(color_palette[6])
  } else if (temperature <= thresholds[7]) {
    return(color_palette[7])
  } else if (temperature <= thresholds[8]) {
    return(color_palette[8])
  } else {
    return(color_palette[9])
  } 
}

# Apply the function to create the "color" column in your dataframe
fulltab$color <- sapply(fulltab$T_meanyear, assign_color)

# Filtrer les donn?es pour les ann?es de 1960 ? 2020
filtered_data <- fulltab %>% filter(year >= 1960 & year <= 2020)


#############
# BOUCLE sur les années pour ajouter les area
p<-ggplot(filtered_data, aes(x = year, y = fish))
l <- list()
for (i in seq(0:60)) {
  annee <- 1960 +i
  annee1 <- 1961 + i
  
  # Utilisez la fonction aes_string pour créer dynamiquement les noms des variables
  p <- p + geom_area(data = subset(filtered_data, year >= annee & year <= annee1),
                     aes(y = fish), fill = fulltab[i, 6],alpha=0.9) 
  l[[i]] <- p
}
p<-l[[60]]

p
###################

p<-p+
  # ajout des ann?es el nino
  geom_rect(aes(xmin = 1963, xmax = 1966, ymin = 1.2e7, ymax = 1.25e7),
            fill = "#9ecae1", alpha = 0.2)+
  geom_rect(aes(xmin = 1968, xmax = 1969, ymin = 1.2e7, ymax = 1.25e7),
            fill = "#9ecae1", alpha = 0.2)+
  geom_rect(aes(xmin = 1972, xmax = 1973, ymin = 1.2e7, ymax = 1.25e7), 
            fill = "#9ecae1", alpha = 0.2)+
  geom_rect(aes(xmin = 1976, xmax = 1980, ymin = 1.2e7, ymax = 1.25e7), 
            fill = "#9ecae1", alpha = 0.2)+
  geom_rect(aes(xmin = 1982, xmax = 1983, ymin = 1.2e7, ymax = 1.25e7), 
            fill = "#9ecae1", alpha = 0.2)+
  geom_rect(aes(xmin = 1986, xmax = 1987, ymin = 1.2e7, ymax = 1.25e7), 
            fill = "#9ecae1", alpha = 0.2)+
  geom_rect(aes(xmin = 1991, xmax = 1994, ymin = 1.2e7, ymax = 1.25e7), 
            fill = "#9ecae1", alpha = 0.2)+
  geom_rect(aes(xmin = 1997, xmax = 1998, ymin = 1.2e7, ymax = 1.25e7), 
            fill = "#9ecae1", alpha = 0.2)+
  geom_rect(aes(xmin = 2002, xmax = 2004, ymin = 1.2e7, ymax = 1.25e7), 
            fill = "#9ecae1", alpha = 0.2)+
  geom_rect(aes(xmin = 2009, xmax = 2010, ymin = 1.2e7, ymax = 1.25e7), 
            fill = "#9ecae1", alpha = 0.2)+
  geom_rect(aes(xmin = 2014, xmax = 2016, ymin = 1.2e7, ymax = 1.25e7), 
            fill = "#9ecae1", alpha = 0.2)+
  geom_rect(aes(xmin = 2019, xmax = 2020, ymin = 1.2e7, ymax = 1.25e7), 
            fill = "#9ecae1", alpha = 0.2)+
  # lignes des ann?es el nino
  geom_vline(xintercept = el_nino_years, linetype = "dotted", color = "darkblue") +
  
  geom_line(aes(y = fish), color = "black") +
  # geom_line(aes(y = temp), color = "red") +
  labs(title=" Peche de capture au Pérou et anomalies de températures entre 1960 et 2020",
     subtitle = " ",
     x="",
     y="tonne métrique")+
  labs(caption = "Source pêche de capture: www.worldbank.org \n Source température: www.weatherandclimate")+
  theme_classic()+  
  theme(plot.title = element_text(size=10,face="bold",hjust=0),
                          plot.caption = element_text(face="italic", hjust=1,size=8),
                          axis.title = element_text(size= 9))

p


# plot1<-p

#############################
###########################

ggplot(filtered_data, aes(x = year, y = fish))+
  geom_line(aes(y = fish), color = "black") +
  # geom_line(aes(y = temp), color = "red") +
  labs(title=" Peche de capture au Pérou et anomalies de températures entre 1960 et 2020",
       subtitle = " ",
       x="",
       y="tonne métrique")+
  labs(caption = "Source pêche de capture: www.worldbank.org \n Source température: www.weatherandclimate")+
  theme_classic()+  
  theme(plot.title = element_text(size=10,face="bold",hjust=0),
        plot.caption = element_text(face="italic", hjust=1,size=8),
        axis.title = element_text(size= 9))

