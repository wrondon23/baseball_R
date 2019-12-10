library(Lahman)
library(dplyr)
library(tidyverse)
library(baseballr)
library(formattable)

#Web scraping batiadores baseball reference
#*************************************************************
Batter<-daily_batter_bref("2019-03-29", "2019-09-30")
Pitcher <- daily_pitcher_bref("2015-05-10", "2015-06-20")