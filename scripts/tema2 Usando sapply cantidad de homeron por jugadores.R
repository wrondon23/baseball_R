
#los mejors honroneros de la decada de 1960

install.packages("Lahman")
library(Lahman)

#Cargando el dataset de bateo de forma general
Batting <- read.csv("./data/Batting.csv")

#Crear el subconjunto para 1960

Batting.60 <- subset(Batting, yearID >= 1960 & yearID <= 1969)

#creando la funcion para calcular los homero

computer.hr <- function(pid){
  
  d <- subset(Batting.60, playerID == pid)
 sum(d$HR)  
}

#crea un vector con los id  unicos de los jugadores del 60

players <- unique(Batting.60$playerID)

#aplicando la funcion sapply para computar los homeron por jugador

S <- sapply(players, computer.hr)

##crear un nuevo dataset con los computos y los jugadores 

R <- data.frame(Player = players, HR = S)
R <- R[order (R$HR , decreasing = TRUE),]

head(R)


