library(Lahman)
library(tidyverse)

#crear vector con resultados de 10 apariciones al bate
outcomes <- c("Single","Out","Out","Single","Out","Double","Out","Walk","Out","Single")

#tabla de frencuencia
table(outcomes)

#crear en factores este vector 
f.outcomes <- factor(outcomes,
                     levels = c("Out","Walk","Single","Double")
                     )

table(f.outcomes)

outcomes == "Walk"

sum(outcomes == "Walk")


#partes dos pichers 

#selecionar los pichers con mas de 350 juegos ganadso 
###*******************************************************************************************
library(plyr)
Pitching <- read.csv("./data/pitching_W.csv")

dataframe.350 <- ddply(Pitching, .(playerID), summarize, W = sum(W, na.rm = TRUE),
                       L = sum(L, na.rm = TRUE), SO = sum(SO, na.rm = T), BB = sum(BB, na.rm = T)
                       )

Wins.350 <- subset(dataframe.350, W >=350)


#Calcular porcentaje de ganados 
Wins.350$Win.PCT <- (100 * (Wins.350$W/(Wins.350$W + Wins.350$L)))

Wins.350 <- data.frame(Name,W, L, Win.PCT)

#ordernar en forma asencendete 

Wins.350 <- Wins.350[order(Wins.350$Win.PCT , decreasing = TRUE),]


#Calcular la zona de  strikeout-walk ratio

Wins.350$SO.BB.Ration <- Wins.350$SO / Wins.350$BB

#crear un data set con los que exceden el ration > 2.8
win.350.2.8 <- subset(Wins.350, Wins.350$SO.BB.Ration > 2.8)

#ordernar el dataset 

win.350.2.8 <- win.350.2.8[order(win.350.2.8$SO.BB.Ration, decreasing = TRUE),]




