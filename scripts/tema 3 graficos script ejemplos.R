
#cargando el dataset de bateo de los miembros en saldo de la fama 

hof <- read.csv("./data/hofbatting.csv")

#agregar la variables MidCareer, que muestra la mitad de la carrera del jugador
hof$MidCareer <- with(hof, (From + To)/2)

#convertir en factor las epocas 
hof$Era <- cut(hof$MidCareer,
               breaks=c(1800, 1900, 1919, 1941, 1960, 1976, 1993, 2050),
               labels=c("19th Century", "Dead Ball","Lively Ball", 
                        "Integration", "Expansion", "Free Agency",
                        "Long Ball"))


T.Era <- table(hof$Era)



#graficos para variables de factores o categoricas 

barplot(table(hof$Era), xlab = "Era", ylab = "Frecuency",
        main = "Era of the Nonpitching Hall of Famers")


plot(table(hof$Era))

pie(table(hof$Era))

#****************************************************************
#variables numericas 

windows(width=7, height = 3.5)
stripchart(hof$MidCareer, method = "jitter", pch = 1,
            xlab = "Mid Career"
           )
hist(hof$MidCareer, xlab = "Mid Career", main ="",
     breaks = seq(1880,200, by = 20))
#************************************************************
with(hof, plot(MidCareer, OPS))
with(hof, lines(lowess(MidCareer,OPS, f=0.3)))
with(hof, identify(MidCareer, OPS,  Player, n=4))

#***********************************************************

with(hof, plot(OBP, SLG, xlim = c(0.25,0.50),
               ylim = c(0.28,0.75), pch=19,
               xlab = "On-Base Percentage",
               ylab = "Slugging Percentaje"
               ))


with(hof, identify(OBP, SLG, Player, n=6))


#******************************************************
#comparacion de homerum y la ERA en baseball

hof$HR.Rate  <- with(hof, HR/AB)

#Grafico stripchart(HR.Rate !)




