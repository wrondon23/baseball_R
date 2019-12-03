
#calcular el PIB (FIELDING INDEPENDENT PITCHING)

spahn$FIP = NULL

spahn$FIP <- with(spahn, (13 * HR + 3 * BB - 2 * SO) / IP)

pos <- order(spahn$FIP)

head(spahn[pos, c("Year","Age","W","L","ERA","FIP")])


#Dividir los numero de los dos equipos en los que jugo

spahn1 <- subset(spahn, Tm == "BSN" | Tm == "MLN")

#Renombrando los nieveles del factor Tm
spahn1$Tm <- factor(spahn1$Tm, levels = c("BSN", "MLN"))

#comparacion por equipso de saphn

by(spahn1[c("W.L", "ERA", "WHIP", "FIP")], spahn1$Tm, summary)

#cargar estadisticas  bateo por ligas 

NLbatting <- read.csv("./data/NLbatting.csv")
ALbatting <- read.csv("./data/ALbatting.csv")
NLpitching <- read.csv("./data/NLpitching.csv")

NL <- merge(NLbatting, NLpitching, by ="Tm")

NL.150 <- subset(NLbatting, HR >150)
