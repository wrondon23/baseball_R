library(Lahman)
library(dplyr)
library(tidyverse)
library(baseballr)
library(formattable)

#Web scraping batiadores baseball reference
#*************************************************************
Batter<-daily_batter_bref("2019-03-29", "2019-09-30")
Pitcher <- daily_pitcher_bref("2019-03-20", "2019-09-30")


Pitcher

******************************************************************
  
  #
  Pitcher$FIP<- with(Pitcher, ((13 * HR) + (3 * BB + HBP) - (2 * SO) )/ IP) + 3.20

#******************************************************************
Houston <-Pitcher%>%
  filter(Team == 'Houston' & IP >= 40)

Pitcher$FIP

summary(Pitcher$FIP)