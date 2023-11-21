# Importation des packages:
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(data.table)

## DATA PECHE
# importation des donnÃ©es sur les peches au pÃ©rou
fisherie_capture <- read.csv("C:/Users/renax/Desktop/ACO/S9/Visualisation/Projet_Visu/Data/fisherie_capture.csv", sep=";", header=FALSE)
# on retourne le data frame
fisherie_capture<- as.data.frame(t(fisherie_capture))
fisherie_capture<- fisherie_capture[-c(1:4),]
colnames(fisherie_capture)<- c("year","fish")
fisherie_capture$fish<-as.numeric(fisherie_capture$fish)
fisherie_capture$year<-as.numeric(fisherie_capture$year)


# DATA TEMPERATURE
temp <- read.csv("C:/Users/renax/Desktop/ACO/S9/Visualisation/Projet_Visu/Data/lima_temp.csv", sep=";", na.strings = 999.9)

#DATA CHLORPHYLLE
chlA <- read.csv("C:/Users/renax/Desktop/ACO/S9/Visualisation/Projet_Visu/Data/chlorophyl_a.csv", sep=",") 

# DATA ENSEMBLE

# Fish_19602020$Fish_centre<- as.data.frame(scale(Fish_19602020$Fish))
# Temp_19602020$Temp_year_centre<- as.data.frame(scale(Temp_19602020$meanyear))
str(fisherie_capture)
str(temp)
str(chlA)

# # Merge first pair of data frames
# merge1 <- merge(fisherie_capture, temp[, c("year", "meanyear")], by = 'year')
# 
# # Merge the result with the third data frame
# fulltab <- merge(merge1, chlA[, c("year", "chlo.a", "Cat")], by = 'year')
# colnames(fulltab)
# colnames(fulltab)<- c("year"  ,   "fish"   ,  "T_meanyear", "chlo.a"  , "Cat" )
# # data_centre<- data.table::as.data.table(data_centre)
# data_centre<- as.data.frame(data_centre)
# write.table(fulltab,file="C:/Users/renax/Desktop/ACO/S9/Visualisation/Projet_Visu/Data/FULLTAB.csv")
#colnames(ninocentre)<- c("year", 'fish','temp')

fulltab<- read.table("C:/Users/renax/Desktop/ACO/S9/Visualisation/Projet_Visu/Data/FULLTAB.csv")

#-------- premiers plots exploratoires nuls ---------

# graph de la temperature de l'eau
plot1<- fulltab%>%
  filter(year <2021) %>% 
  ggplot(aes(x=year, y=T_meanyear))+
  geom_point()+
  geom_line()+
  theme_minimal()

# graph des peches de poisson
plot2<- fulltab %>%
  filter(year <2021) %>% 
  ggplot(aes(x=year, y=fish))+
  geom_point()+
  geom_line()+
  theme_minimal()

# graph chlorophyle a en fontion du temps
plot3<- fulltab %>%  
  filter(year <2021) %>% 
  ggplot(aes(x=year, y=chlo.a))+
  geom_point()+
  geom_line()+
  theme_minimal()


# Convertir les graphiques en objets grob
plot1 <- ggplotGrob(plot1)
plot2 <- ggplotGrob(plot2)
plot3 <- ggplotGrob(plot3)

# Organiser les graphiques en utilisant grid.arrange
arrange_plots <- grid.arrange(plot1, plot2,plot3)




####------ GRAPH INITIAL

el_nino_years <- c(1963,1966, 1968,1969, 1972,1973, 1976, 1980, 1982,
                   1983, 1986,1987, 1991, 1994, 1997,1998, 2002,2004, 
                   2009,2010,2014, 2016, 2019,2020)

# data annees x temperature

fulltab %>%filter(year <2021) %>%  ggplot() + 
  aes(x = year,y=fish) +
  # ajout des années el nino
  geom_rect(aes(xmin = 1963, xmax = 1966, ymin = 1.2e7, ymax = 1.25e7),
               fill = "pink", alpha = 0.2)+
  geom_rect(aes(xmin = 1968, xmax = 1969, ymin = 1.2e7, ymax = 1.25e7),
           fill = "pink", alpha = 0.2)+
  geom_rect(aes(xmin = 1972, xmax = 1973, ymin = 1.2e7, ymax = 1.25e7), 
            fill = "pink", alpha = 0.2)+
  geom_rect(aes(xmin = 1976, xmax = 1980, ymin = 1.2e7, ymax = 1.25e7), 
            fill = "pink", alpha = 0.2)+
  geom_rect(aes(xmin = 1982, xmax = 1983, ymin = 1.2e7, ymax = 1.25e7), 
            fill = "pink", alpha = 0.2)+
  geom_rect(aes(xmin = 1986, xmax = 1987, ymin = 1.2e7, ymax = 1.25e7), 
            fill = "pink", alpha = 0.2)+
  geom_rect(aes(xmin = 1991, xmax = 1994, ymin = 1.2e7, ymax = 1.25e7), 
            fill = "pink", alpha = 0.2)+
  geom_rect(aes(xmin = 1997, xmax = 1998, ymin = 1.2e7, ymax = 1.25e7), 
            fill = "pink", alpha = 0.2)+
  geom_rect(aes(xmin = 2002, xmax = 2004, ymin = 1.2e7, ymax = 1.25e7), 
            fill = "pink", alpha = 0.2)+
  geom_rect(aes(xmin = 2009, xmax = 2010, ymin = 1.2e7, ymax = 1.25e7), 
            fill = "pink", alpha = 0.2)+
  geom_rect(aes(xmin = 2014, xmax = 2016, ymin = 1.2e7, ymax = 1.25e7), 
            fill = "pink", alpha = 0.2)+
  geom_rect(aes(xmin = 2019, xmax = 2020, ymin = 1.2e7, ymax = 1.25e7), 
            fill = "pink", alpha = 0.2)+
  # lignes des années el nino
  geom_vline(xintercept = el_nino_years, linetype = "dotted", color = "darkred") +

  geom_line(aes(y = fish), color = "blue") +
  # geom_line(aes(y = temp), color = "red") +
  geom_line(y = 0, linetype = "dashed", color = "black") +
  #geom_ribbon(aes(ymin = 0, ymax = temp), fill = "pink", alpha = 0.5)+  # Zone colorée en rose sous la courbe rouge    theme_minimal()
  labs(title=" Peche de capture au Pérou et anomalies de températures",
       subtitle = " en tonne métrique",
       x="",
       y="")+

  
  
  theme_minimal()











