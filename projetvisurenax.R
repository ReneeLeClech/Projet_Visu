# Importation des packages:
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(data.table)
library(RColorBrewer)


# ## DATA PECHE
# # importation des données sur les peches au pérou
# fisherie_capture <- read.csv("C:/Users/renax/Desktop/ACO/S9/Visualisation/Projet_Visu/Data/fisherie_capture.csv", sep=";", header=FALSE)
# # on retourne le data frame
# fisherie_capture<- as.data.frame(t(fisherie_capture))
# fisherie_capture<- fisherie_capture[-c(1:4),]
# colnames(fisherie_capture)<- c("year","fish")
# fisherie_capture$fish<-as.numeric(fisherie_capture$fish)
# fisherie_capture$year<-as.numeric(fisherie_capture$year)
# 
# 
# # DATA TEMPERATURE
# temp <- read.csv("C:/Users/renax/Desktop/ACO/S9/Visualisation/Projet_Visu/Data/lima_temp.csv", sep=";", na.strings = 999.9)
# 
# #DATA CHLORPHYLLE
# chlA <- read.csv("C:/Users/renax/Desktop/ACO/S9/Visualisation/Projet_Visu/Data/chlorophyl_a.csv", sep=",") 
# 
# # DATA ENSEMBLE
# 
# # Fish_19602020$Fish_centre<- as.data.frame(scale(Fish_19602020$Fish))
# # Temp_19602020$Temp_year_centre<- as.data.frame(scale(Temp_19602020$meanyear))
# str(fisherie_capture)
# str(temp)
# str(chlA)

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
  # ajout des ann?es el nino
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
  # lignes des ann?es el nino
  geom_vline(xintercept = el_nino_years, linetype = "dotted", color = "darkred") +

  geom_line(aes(y = fish), color = "blue") +
  # geom_line(aes(y = temp), color = "red") +
  geom_line(y = 0, linetype = "dashed", color = "black") +
  #geom_ribbon(aes(ymin = 0, ymax = temp), fill = "pink", alpha = 0.5)+  # Zone color?e en rose sous la courbe rouge    theme_minimal()
  labs(title=" Peche de capture au P?rou et anomalies de temp?ratures",
       subtitle = " en tonne m?trique",
       x="",
       y="")+
  theme_minimal()

####-----------------------------------------
# Chargez la biblioth?que RColorBrewer

# Definissez le nombre de niveaux de temp?rature et la palette de couleurs
color_palette <- c("lightyellow","navajowhite","sandybrown","orange1","chocolate1","red","red3","red4","firebrick4")
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

#############################

p<-ggplot(filtered_data, aes(x = year, y = fish))
l <- list()
for (i in seq(0:60)) {
  annee <- 1960 + i
  annee1 <- 1961 + i
  
  # Utilisez la fonction aes_string pour créer dynamiquement les noms des variables
  p <- p + geom_area(data = subset(filtered_data, year >= annee & year <= annee1),
                     aes(y = fish), fill = fulltab[i, 6] ) 
  l[[i]] <- p
}
p<-l[[60]]


###############################
p+
  # ajout des ann?es el nino
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
  # lignes des ann?es el nino
  geom_vline(xintercept = el_nino_years, linetype = "dotted", color = "darkred") +
  
  geom_line(aes(y = fish), color = "blue") +
  # geom_line(aes(y = temp), color = "red") +
  geom_line(y = 0, linetype = "dashed", color = "black") +
  #geom_ribbon(aes(ymin = 0, ymax = temp), fill = "pink", alpha = 0.5)+  # Zone color?e en rose sous la courbe rouge    theme_minimal()
  labs(title=" Peche de capture au P?rou et anomalies de temp?ratures",
       subtitle = " en tonne m?trique",
       x="",
       y="")+
  theme_minimal()



############"
  
p<-ggplot(filtered_data, aes(x = year, y = fish))+
  # on cr?? autant de area que d'ann?es  geom_area(data = subset(filtered_data, year >= 1960 & year <= 1961), aes(y = fish),color = fulltab[2, 6] +
  geom_area(data = subset(filtered_data, year >= 1960 & year <= 1961), aes(y = fish), fill = fulltab[1, 6] ) +
  
  
p


p<-ggplot(filtered_data, aes(x = year, y = fish))+
  # on cr?? autant de area que d'ann?es  geom_area(data = subset(filtered_data, year >= 1960 & year <= 1961), aes(y = fish),color = fulltab[2, 6] +
  geom_area(data = subset(filtered_data, year >= 1960 & year <= 1961), aes(y = fish), fill = fulltab[1, 6] ) +
  geom_area(data = subset(filtered_data, year >= 1961 & year <= 1962), aes(y = fish), fill = fulltab[2, 6] )+
  geom_area(data = subset(filtered_data, year >= 1962 & year <= 1963), aes(y = fish), fill = fulltab[3, 6] )+
  geom_area(data = subset(filtered_data, year >= 1963 & year <= 1964), aes(y = fish), fill = fulltab[4, 6] )+
  geom_area(data = subset(filtered_data, year >= 1964 & year <= 1965), aes(y = fish), fill = fulltab[5, 6] )+
  geom_area(data = subset(filtered_data, year >= 1965 & year <= 1966), aes(y = fish), fill = fulltab[6, 6] )+
  geom_area(data = subset(filtered_data, year >= 1966 & year <= 1967), aes(y = fish), fill = fulltab[7, 6] )+
  geom_area(data = subset(filtered_data, year >= 1967 & year <= 1968), aes(y = fish), fill = fulltab[8, 6] )+
  geom_area(data = subset(filtered_data, year >= 1968 & year <= 1969), aes(y = fish), fill = fulltab[9, 6])+
  geom_area(data = subset(filtered_data, year >= 1969 & year <= 1970), aes(y = fish), fill = fulltab[10, 6] )
  
  geom_area(data = subset(filtered_data, year >= 1970 & year <= 1971), aes(y = fish), fill = fulltab[11, 6] )+
  geom_area(data = subset(filtered_data, year >= 1971 & year <= 1972), aes(y = fish), fill = fulltab[12, 6] )+
  geom_area(data = subset(filtered_data, year >= 1972 & year <= 1973), aes(y = fish), fill = fulltab[13, 6] )+
  geom_area(data = subset(filtered_data, year >= 1973 & year <= 1974), aes(y = fish), fill = fulltab[14, 6] )+
  geom_area(data = subset(filtered_data, year >= 1974 & year <= 1975), aes(y = fish), fill = fulltab[15, 6] )+
  geom_area(data = subset(filtered_data, year >= 1975 & year <= 1976), aes(y = fish), fill = fulltab[16, 6] )+
  geom_area(data = subset(filtered_data, year >= 1976 & year <= 1977), aes(y = fish), fill = fulltab[17, 6] )+
  geom_area(data = subset(filtered_data, year >= 1977 & year <= 1978), aes(y = fish), fill = fulltab[18, 6] )+
  geom_area(data = subset(filtered_data, year >= 1978 & year <= 1979), aes(y = fish), fill = fulltab[19, 6] )+
  geom_area(data = subset(filtered_data, year >= 1979 & year <= 1980), aes(y = fish), fill = fulltab[20, 6] )+
  
  geom_area(data = subset(filtered_data, year >= 1980 & year <= 1981), aes(y = fish), fill = fulltab[21, 6] )+
  geom_area(data = subset(filtered_data, year >= 1981 & year <= 1982), aes(y = fish), fill = fulltab[22, 6]) +
  geom_area(data = subset(filtered_data, year >= 1982 & year <= 1983), aes(y = fish), fill = fulltab[23, 6]) +
  geom_area(data = subset(filtered_data, year >= 1983 & year <= 1984), aes(y = fish), fill = fulltab[24, 6]) +
  geom_area(data = subset(filtered_data, year >= 1984 & year <= 1985), aes(y = fish), fill = fulltab[25, 6]) +
  geom_area(data = subset(filtered_data, year >= 1985 & year <= 1985), aes(y = fish), fill = fulltab[26, 6]) +
  geom_area(data = subset(filtered_data, year >= 1986 & year <= 1986), aes(y = fish), fill = fulltab[27, 6]) +
  geom_area(data = subset(filtered_data, year >= 1987 & year <= 1987), aes(y = fish), fill = fulltab[28, 6]) +
  geom_area(data = subset(filtered_data, year >= 1988 & year <= 1988), aes(y = fish), fill = fulltab[29, 6]) +
  geom_area(data = subset(filtered_data, year >= 1989 & year <= 1990), aes(y = fish), fill = fulltab[30, 6]) +
  
  geom_area(data = subset(filtered_data, year >= 1990 & year <= 1991), aes(y = fish), fill = fulltab[31, 6] )+
  geom_area(data = subset(filtered_data, year >= 1991 & year <= 1992), aes(y = fish), fill = fulltab[32, 6]) +
  geom_area(data = subset(filtered_data, year >= 1992 & year <= 1993), aes(y = fish), fill = fulltab[33, 6]) +
  geom_area(data = subset(filtered_data, year >= 1993 & year <= 1994), aes(y = fish), fill = fulltab[34, 6]) +
  geom_area(data = subset(filtered_data, year >= 1994 & year <= 1995), aes(y = fish), fill = fulltab[35, 6]) +
  geom_area(data = subset(filtered_data, year >= 1995 & year <= 1995), aes(y = fish), fill = fulltab[36, 6]) +
  geom_area(data = subset(filtered_data, year >= 1996 & year <= 1996), aes(y = fish), fill = fulltab[37, 6]) +
  geom_area(data = subset(filtered_data, year >= 1997 & year <= 1997), aes(y = fish), fill = fulltab[38, 6]) +
  geom_area(data = subset(filtered_data, year >= 1998 & year <= 1998), aes(y = fish), fill = fulltab[39, 6]) +
  geom_area(data = subset(filtered_data, year >= 1999 & year <= 2000), aes(y = fish), fill = fulltab[40, 6]) +
  
  geom_area(data = subset(filtered_data, year >= 2000 & year <= 2001), aes(y = fish), fill = fulltab[41, 6] )+
  geom_area(data = subset(filtered_data, year >= 2001 & year <= 2002), aes(y = fish), fill = fulltab[42, 6]) +
  geom_area(data = subset(filtered_data, year >= 2002 & year <= 2003), aes(y = fish), fill = fulltab[43, 6]) +
  geom_area(data = subset(filtered_data, year >= 2003 & year <= 2004), aes(y = fish), fill = fulltab[44, 6]) +
  geom_area(data = subset(filtered_data, year >= 2004 & year <= 2005), aes(y = fish), fill = fulltab[45, 6]) +
  geom_area(data = subset(filtered_data, year >= 2005 & year <= 2005), aes(y = fish), fill = fulltab[46, 6]) +
  geom_area(data = subset(filtered_data, year >= 2006 & year <= 2006), aes(y = fish), fill = fulltab[47, 6]) +
  geom_area(data = subset(filtered_data, year >= 2007 & year <= 2007), aes(y = fish), fill = fulltab[48, 6]) +
  geom_area(data = subset(filtered_data, year >= 2008 & year <= 2008), aes(y = fish), fill = fulltab[49, 6]) +
  geom_area(data = subset(filtered_data, year >= 2009 & year <= 2010), aes(y = fish), fill = fulltab[50, 6]) +
  
  geom_area(data = subset(filtered_data, year >= 2010 & year <= 2011), aes(y = fish), fill = fulltab[51, 6] )+
  geom_area(data = subset(filtered_data, year >= 2011 & year <= 2012), aes(y = fish), fill = fulltab[52, 6]) +
  geom_area(data = subset(filtered_data, year >= 2012 & year <= 2013), aes(y = fish), fill = fulltab[53, 6]) +
  geom_area(data = subset(filtered_data, year >= 2013 & year <= 2014), aes(y = fish), fill = fulltab[54, 6]) +
  geom_area(data = subset(filtered_data, year >= 2014 & year <= 2015), aes(y = fish), fill = fulltab[55, 6]) +
  geom_area(data = subset(filtered_data, year >= 2015 & year <= 2015), aes(y = fish), fill = fulltab[56, 6]) +
  geom_area(data = subset(filtered_data, year >= 2016 & year <= 2016), aes(y = fish), fill = fulltab[57, 6]) +
  geom_area(data = subset(filtered_data, year >= 2017 & year <= 2017), aes(y = fish), fill = fulltab[58, 6]) +
  geom_area(data = subset(filtered_data, year >= 2018 & year <= 2018), aes(y = fish), fill = fulltab[59, 6]) +
  geom_area(data = subset(filtered_data, year >= 2019 & year <= 2020), aes(y = fish), fill = fulltab[60, 6]) 
  

  geom_vline(xintercept = el_nino_years, linetype = "dotted", color = "darkred") +
  
  # On ajoute le reste du graph
  
  geom_line(aes(y = fish), color = "blue") +
  # ajout des annees el nino
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

  labs(title=" Peche de capture au Pérou et anomalies de températures entre 1960 et 2020",
       subtitle = " ",
       x="",
       y="tonne métrique")+
    labs(caption = "Source: www.ipsos-na.com, Design: Stefan Fichtel")+
  theme_minimal()

# Afficher le graphique
print(p)


############################
############################


# Créer un graphique de la légende
legend_colors <- color_palette
legend_labels <- c(paste("-Inf -", round(thresholds[1], 2)),
                   paste(round(thresholds[1], 2), "-", round(thresholds[2], 2)),
                   paste(round(thresholds[2], 2), "-", round(thresholds[3], 2)),
                   paste(round(thresholds[3], 2), "-", round(thresholds[4], 2)),
                   paste(round(thresholds[4], 2), "-", round(thresholds[5], 2)),
                   paste(round(thresholds[5], 2), "-", round(thresholds[6], 2)),
                   paste(round(thresholds[6], 2), "-", round(thresholds[7], 2)),
                   paste(round(thresholds[7], 2), "-", round(thresholds[8], 2)),
                   paste(round(thresholds[8], 2), "-", "+Inf"))

legend_df <- data.frame(legend_colors, legend_labels)

# Tracer la légende
library(ggplot2)

ggplot() +
  geom_tile(aes(x = 1, y = seq(1, 9), fill = legend_colors), height = 1, width = 1) +
  scale_fill_identity() +
  theme_void() +
  theme(legend.position = "right") +
  guides(fill = guide_legend(title = "Température", keywidth = 1, keyheight = 1,
                             override.aes = list(color = "black"),
                             label.theme = element_text(size = 8))) +
  annotate("text", x = 1.6, y = seq(1, 9), label = legend_labels, vjust = 1, hjust = 0)
