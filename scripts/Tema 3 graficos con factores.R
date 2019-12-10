 
library(dplyr)
library(Lahman)
library(tidyverse)

hof <- read.csv("./data/hofbatting.csv")

hof$MidCareer <- with(hof, (From + To)/2)

#crear un factor
hof$Era <- cut(hof$MidCareer,
                  breaks =  c(1800, 1900, 1919, 1941, 1960, 1976, 1993, 2050),
                  labels = c("19th Century", "Dead Ball","Lively Ball",
                             "Integration", "Expansion", "Free Agency",
                             "Long Ball") )

T.Era <- table(hof$Era)

T.Era
#graficos 
barplot(table(hof$Era),
        xlab = "Era",
        ylab = "Frequecy",
        main = "Era of the Nonpitching Hall of Famers"
        )

plot (table(hof$Era)),

pie(table(hof$Era))

#grafico dot plot
dotchart(as.numeric(T.Era), labels = names(T.Era), xlab = "Frequency")

#crear un subconjuento de datos con los que tienen 500 HR O MAS 

hof.500 <- hof%>%
    filter( HR >= 500)%>%
   arrange(OPS)


#Construir un dot plot
dotchart(hof.500$OPS, labels = hof.500$Player, xlab = "OPS")

#******************************************************************************


  
