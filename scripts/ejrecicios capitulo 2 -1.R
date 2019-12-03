
library("Lahman")
library("tidyverse"); options(dplyr.width = Inf)




#Cargando el dataTAble de Bateo 

Batting <- read.csv("./data/hofbatting2.csv")

#Creando los vectores de SB, CS, G

SB <- c(Batting$SB)
CS <- c(Batting$CS)
G  <- c(Batting$G)

str(SB)

#calcular el numero de intentos de robos 

SB.Attempt <- c(SB + CS)


#Calcular la taza de exito para el robo de bases para todos los jugadores 
Batting$Success.Rate <- with(Batting, SB / SB.Attempt)

#Calcular el numero de base robadas por juegos 
Batting$SB.Game <- c(SB/G)


#Grafico de Success.rate con SB.game


summary(Batting)

with(Batting, plot(Batting$SB.Game ~ Batting$Success.Rate)) 


Batting$Success.Rate2 = NULL






ggplot(Batting, aes(x = (SB / (SB + CS)),
                    y = (SB / G)), label = X) +
                    theme_bw() + 
                   geom_point(size = 1.5, colour = "red") +
                   xlab("Stolen Base \nSuccess Rate") + 
                   ylab("Stolen Bases \nper Game") +
                   
                  geom_text(size= 2, aes(label = X), nudge_y = 0.0125)


##******************************************************************************
#Los Mejores robadores de base 2018 
##******************************************************************************

##cargar el csv
SB.Players2018 <- read.csv("./data/SB_Players_2018.csv")

#Creando los vectores de SB, CS, G

SB <- c(SB.Players2018$SB)
CS <- c(SB.Players2018$CS)
G  <- c(SB.Players2018$G)

SB.Attempt <- c(SB+CS)



#Calcular la taza de exito para el robo de bases para todos los jugadores 
SB.Players2018$Success.Rate <- with(SB.Players2018, SB / SB.Attempt)

#Calcular el numero de base robadas por juegos 
SB.Players2018$SB.Game <- c(SB/G)

#Graficar



ggplot(SB.Players2018, aes(x = (SB / (SB + CS)),
                    y = (SB / G)), label = playerName) +
  theme_bw() + 
  geom_point(size = 1.5, colour = "red") +
  xlab("Stolen Base \nSuccess Rate") + 
  ylab("Stolen Bases \nper Game") +
  geom_text(size= 2, aes(label = playerName), nudge_y = 0.0125)




