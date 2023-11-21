# Importation des packages:
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(data.table)

## DATA PECHE
# importation des données sur les peches au pérou
fisherie_capture <- read.csv("C:/Users/renax/Desktop/ACO/S9/Visualisation/Projet_Visu/Data/fisherie_capture.csv", sep=";", header=FALSE)
# on retourne le data frame
fisherie_capture<- as.data.frame(t(fisherie_capture))
fisherie_capture<- fisherie_capture[-c(1:4),]
colnames(fisherie_capture)<- c("year","Fish_Capture_in_T")
fisherie_capture$Fish_Capture_in_T<-as.numeric(fisherie_capture$Fish_Capture_in_T)
fisherie_capture$year<-as.numeric(fisherie_capture$year)

##DATA ELNINO
# importation des données météo du phénomène el nino
elnino <- read.csv("C:/Users/renax/Desktop/ACO/S9/Visualisation/Projet_Visu/Data/elnino.csv", sep=";")
elnino$Zonal.Winds<- as.numeric(elnino$Zonal.Winds)
elnino$Meridional.Winds<- as.numeric(elnino$Meridional.Winds)
elnino$Humidity<- as.numeric(elnino$Humidity)
elnino$Air.Temp<- as.numeric(elnino$Air.Temp)
elnino$Sea.Surface.Temp<- as.numeric(elnino$Sea.Surface.Temp)

str(elnino)

# DATA TEMPERATURE
temp <- read.csv("C:/Users/renax/Desktop/ACO/S9/Visualisation/Projet_Visu/Data/lima_temp.csv", sep=";", na.strings = 999.9)


#-------- premiers plots exploratoires nuls ---------

# graph de la temperature de l'eau
Temp_19602020 <- temp%>% filter((year <2021) &(year>1959) )

plot1<- ggplot(data=Temp_19602020, aes(x=year, y=meanyear))+
  geom_point()+
  geom_line()+
  theme_minimal()

# graph des peches de poisson
Fish_19602020 <- fisherie_capture %>%
  filter(year <2021) %>%
  group_by(year)%>% 
  summarise(Fish=mean(Fish_Capture_in_T,na.rm=T),
  )

plot2 <- ggplot(data=Fish_19602020, aes(x=year, y=Fish))+
  geom_point()+
  geom_line()+
  theme_minimal()


# Convertir les graphiques en objets grob
plot1 <- ggplotGrob(plot1)
plot2 <- ggplotGrob(plot2)


# Organiser les graphiques en utilisant grid.arrange
arrange_plots <- grid.arrange(plot1, plot2)


####------ idem avec les gp+raph centr�s/ r�duits:

# Fish_19602020$Fish_centre<- as.data.frame(scale(Fish_19602020$Fish))
# Temp_19602020$Temp_year_centre<- as.data.frame(scale(Temp_19602020$meanyear))
# data_centre<- merge(Fish_19602020[,c(1,3)], Temp_19602020[,c(1,15)], by = 'year')
# data_centre<- data.table::as.data.table(data_centre)
# data_centre<- as.data.frame(data_centre)
# write.table(ninocentre,file="C:/Users/renax/Desktop/ACO/S9/Visualisation/Projet_Visu/Data/data_centree.csv")
#colnames(ninocentre)<- c("year", 'fish','temp')

ninocentre<-read.table("C:/Users/renax/Desktop/ACO/S9/Visualisation/Projet_Visu/Data/data_centree.csv")

# data annees x temperature

ninocentre %>% ggplot() + 
  aes(x = year) +
  geom_line(aes(y = fish), color = "blue") +
  geom_line(aes(y = temp), color = "red") +
  geom_line(y = 0, linetype = "dashed", color = "black") +
  geom_ribbon(data = subset(ninocentre, temp > 0), aes(ymin = 0, ymax = temp), 
              fill = "blue", alpha = 0.5) +
  geom_ribbon(data = subset(ninocentre, temp < 0), aes(ymin = temp, ymax = 0), 
              fill = "pink", alpha = 0.5) +
  theme_minimal()










