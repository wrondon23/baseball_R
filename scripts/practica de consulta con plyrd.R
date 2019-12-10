library(dplyr)
library(Lahman)
library(tidyverse)
 
##pagina de documentacion del paquete baseballr - https://www.rdocumentation.org/packages/baseballr/versions/0.5.0

#practica de dplyr completa 
#usar el paquete lahman para las tablas

#SECION UNO SELECION FILTROS Y ORDENAMIENTO
#*********************************************************
Batting%>%
  select(playerID,HR,yearID)%>%
  filter(HR == 50 )%>%
arrange(desc(HR))

#**********************************************************

Batting%>%
  filter(SO < 25 )%>%
  select(playerID, HR, yearID)%>%
  arrange(desc(HR))

#***********************************************************
Batting%>%
  filter(HR >= 50 & SO < 80)%>%
  select(playerID,HR, yearID)%>%
  arrange(desc(HR))



#***********************************************************
Batting%>%
  filter(HR >= 50 | SO < 80)%>%
  select(playerID,HR, yearID)%>%
  arrange(desc(HR))


#***********************************************************
Batting%>%
  filter(AB > 500 & (HR >= 50 | SO < 80))%>%
  select(playerID,HR, yearID)%>%
  arrange(desc(HR))


#SECION 2- GRUPO Y SUMA , AVERAGE, MAX, MIN, WHERE AND HAVING
#**************************************************************
Batting %>%
  group_by(playerID)%>%
  summarize(Career_HR = sum(HR, na.rm = TRUE))%>%
  arrange(desc(Career_HR))
#*********************************************************
Batting%>%
  group_by(playerID)%>%
  summarize(avg_temporada = round(mean(H, na.rm = TRUE),2))%>%
arrange(desc( avg_temporada))

#*********************************************************
Batting%>%
  group_by(playerID)%>%
   summarize(max_career_HR= max(HR), min_career_HR= min(HR), )%>%
   arrange( desc(max_career_HR))
#*********************************************************
Batting%>%
  group_by(playerID)%>%
  summarize(Count_career_SO = n() )%>%
  arrange( desc(Count_career_SO))

#*********************************************************
Batting%>%
  filter(AB >= 400)%>%
  group_by(playerID)%>%
  summarize(min_career_so = min(SO, na.rm = T))%>%
  filter(min_career_so < 20)%>%
  arrange(desc(min_career_so))

#*********************************************************
#Calcular y agregar una columna 

Bdat <- Batting%>%
        filter(AB >= 400)%>%
        mutate(batting_avg = round(H/AB,3))%>%
        select(playerID, batting_avg,yearID)%>%
arrange( desc(batting_avg))

head(Bdat)


#*********************************************************
Bdat <- Batting%>%
  group_by(playerID)%>%
  summarize(AB_total = sum(AB, na.rm = TRUE), H_total = sum(H, na.rm = TRUE))%>%
  filter(AB_total > 100)%>%
  mutate(batting_avg = round(H_total/AB_total,3))%>%
  select (playerID,batting_avg)%>%
  arrange(desc(batting_avg))
  
#parte 3 inner join 
#*********************************************************

inner_join(Batting, People, by=c("playerID"))%>%
  filter(playerID == "ruthba01" | playerID == "aaronha01")%>%
  select(playerID,nameFirst,nameLast, AB, R, H,yearID)

#*********************************************************

inner_join(Batting, Teams, by = c("teamID","yearID"))%>%
  select(playerID, name, yearID, BB)