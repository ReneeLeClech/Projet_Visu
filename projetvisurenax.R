# Importation des packages:
library(ggplot2)
library(tidyverse)
library(gridExtra)


# importation des données sur les peches au pérou
fisherie_capture <- read.csv("C:/Users/renax/Desktop/ACO/S9/Visualisation/Projet_Visu/Data/fisherie_capture.csv", sep=";", header=FALSE)
# on retourne le data frame
fisherie_capture<- as.data.frame(t(fisherie_capture))
fisherie_capture<- fisherie_capture[-c(1:4),]
colnames(fisherie_capture)<- c("Year","Fish_Capture_in_T")
fisherie_capture$Fish_Capture_in_T<-as.numeric(fisherie_capture$Fish_Capture_in_T)
fisherie_capture$Year<-as.numeric(fisherie_capture$Year)

# importation des données météo du phénomène el nino
elnino <- read.csv("C:/Users/renax/Desktop/ACO/S9/Visualisation/Projet_Visu/Data/elnino.csv", sep=";")
elnino$Zonal.Winds<- as.numeric(elnino$Zonal.Winds)
elnino$Meridional.Winds<- as.numeric(elnino$Meridional.Winds)
elnino$Humidity<- as.numeric(elnino$Humidity)
elnino$Air.Temp<- as.numeric(elnino$Air.Temp)
elnino$Sea.Surface.Temp<- as.numeric(elnino$Sea.Surface.Temp)

str(elnino)

#-------- premiers plots exploratoires nuls ---------

# graph de la température de l'eau
elnino_Eau <- elnino%>% group_by(Year) %>% 
  summarise(EauTemp_mean=mean(Sea.Surface.Temp,na.rm=T),
            )

plot2<- ggplot(data=elnino_Eau, aes(x=Year, y=EauTemp_mean))+
  geom_point()+
  geom_line()+
  theme_minimal()

# graph des peches de poisson
Fish <- fisherie_capture%>% filter((Year <1999) &(Year>1979) ) %>% group_by(Year)%>% 
  summarise(Fish=mean(Fish_Capture_in_T,na.rm=T),
  )

plot3 <- ggplot(data=Fish, aes(x=Year, y=Fish))+
  geom_point()+
  geom_line()+
  theme_minimal()


# Convertir les graphiques en objets grob
plot1 <- ggplotGrob(plot1)
plot2 <- ggplotGrob(plot2)
plot3 <- ggplotGrob(plot3)


# Organiser les graphiques en utilisant grid.arrange
arrange_plots <- grid.arrange(plot2, plot3)

