
#cargando paquete
library(plyr)

#creando DataFrame

head(Batting)

dataframe.AB <- ddply(Batting, .(playerID),  summarise,
                      Career.AB = sum(AB, na.rm = TRUE)
                      )

#usando la funcion Merge para convinar los dos dataframe

Batting <- merge(Batting, dataframe.AB, by="playerID")

#tomar solo los jugadores con mas de 5000 tunos 

Batting.500 <- subset(Batting, Career.AB >= 5000)

#crear la funcion que calcula los turnos al bate , los homeron y los ponches por su carrera

ab.hr.so <- function(d){
  c.AB <- sum(d$AB, na.rm=TRUE)
  c.HR <- sum(d$HR, na.rm=TRUE)
  c.SO <- sum(d$SO, na.rm=TRUE)
  data.frame(AB=c.AB, HR=c.HR, SO=c.SO)
}

#Aplicando la funcion 

d.5000 <- ddply(Batting.500, .(playerID), ab.hr.so)

head(d.5000)

#graficar la relacin hr/turno al bates y ponche/turnos al bate 


with(d.5000, plot(HR/AB, SO/AB))
with(d.5000, lines(lowess(HR/AB, SO/AB)))
  