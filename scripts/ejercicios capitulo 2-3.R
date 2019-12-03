library(tidyverse)
library(dplyr)
library(Lahman)

#(Pitcher Strikeout/Walk Ratios) 

#Cargando el dataFrame pitching.csv

Pitching <- read.csv("./data/pitching.csv")

#Crear una funcion para calcular la acumulacion de ponches, base por volas, total inning en la mitad de carrera

stats <- function(d){
  c.SO <- sum(d$SO, na.rm = T)
  c.BB <- sum(d$BB, na.rm = T)
  c.IPouts <- sum(d$IPouts, na.rm = T)
  c.midYear <- median(d$yearID, na.rm = TRUE)
  data.frame(SO= c.SO, BB = c.BB, IPouts = c.IPouts, midYear = c.midYear)
  
 
}

#usando la funcion ddply para calcular los datos a todo el dataset 
career.pitching <- ddply(Pitching, .(playerID), stats)



#usar merge para crear un dataset con todos los datos 

pitching.merge <- merge(Pitching,career.pitching, by='playerID')

#crear un nuevo dataset con los pitcher con almenos 10.000 IPouts

career.10000 <- subset(career.pitching, career.pitching$IPouts >= 10000)

#Calcular el strikeout-walk radio

career.10000$SO.BB.Ratio <- career.10000$SO/career.10000$BB

#crear un scartplot 

with(career.10000, plot(midYear,SO.BB.Ratio))