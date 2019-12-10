

library(Lahman)
library(dplyr)
library(tidyverse)
library(baseballr)
library(formattable)

#Web scraping batiadores baseball reference
#*************************************************************
Batter<-daily_batter_bref("2019-03-29", "2019-09-30")
Pitcher <- daily_pitcher_bref("2015-05-10", "2015-06-20")

#************************************************************* 
 
Batter_No_Cero <-  Batter[Batter$SB > 0 ,]

summary(Batter_No_Cero$SB)

Batter_SB <- Batter_No_Cero%>%
   filter(SB >= 10)%>%
  select(season, Name, Team, G, SB, CS)%>%
  arrange(desc(SB))


Batter_SB$SB.Attempt <- with(Batter_SB, (SB+CS)) 

Batter_SB$Success.Rate <- with(Batter_SB, (SB/SB.Attempt))

Batter_SB.20 <- Batter_SB[Batter_SB$SB > 20,]
  
#*****************************************************************

pitcher <- daily_pitcher_bref("2019-03-29", "2019-09-30")

ggplot(Batter_SB.20, aes(x = (SB / (SB + CS)),
                           y = (SB / G)), label = Name) +
  theme_bw() + 
  geom_point(size = 1.5, colour = "red") +
  xlab("Stolen Base \nSuccess Rate") + 
  ylab("Stolen Bases \nper Game") +
  geom_text(size= 2, aes(label = Name), nudge_y = 0.0125)

#********************************************************************

Batter_SB.20$Success.Rate <-round(Batter_SB.20$Success.Rate,2) 

formattable(Batter_SB.20 ,
  align = c("l","c","c","c","c", "c", "c", "c", "r")
  
)
#********************************************************************


 
