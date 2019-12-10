install.packages("data.table")
install.packages("formattable")
install.packages("tidyr")



library(dplyr)
library(Lahman)
library(tidyverse)
library(data.table)
library(formattable)
library(tidyr)



#OBP

#CREANDO EL DATA SET DE TODOS LOS BATEADORES DOMINICANOS EN MLB HASTA EL 2018
#******************************************************************************
Dominican_Batting <- inner_join(Batting, People, by="playerID")%>%
filter(birthCountry == "D.R.")%>%
select(playerID, nameFirst, nameLast, G,  AB, R, H, X2B, X3B, HR, RBI, SB, CS, BB,
       SO, IBB, HBP, SH, SF )

#********************************************************************************


#CREANDO UN DATA SET CON LAS SUMAS DE LA VARIABLES QUE USARAN PARA LOS CALCULOS 
#***************************************************************************************
Sub_Batting_Dominic <-Dominican_Batting%>%
  group_by(playerID,nameFirst,nameLast)%>%
  summarize(H = sum(H), BB = sum(BB), HBP = sum(HBP), AB = sum(AB), HBP=sum(HBP), SF = sum(SF))%>%
filter(H >= 1000)

#***************************************************************************************


#Calcular el OBP
#****************************************************************************************
Sub_Batting_Dominic$OBP <- with(Sub_Batting_Dominic, (H + BB + HBP)/(AB + BB + HBP + SF))

Sub_Batting_Dominic <- Sub_Batting_Dominic %>%  #ORDERNAR
       arrange(OBP)

Prueba <- paste(Sub_Batting_Dominic$nameFirst, Sub_Batting_Dominic$nameLast) #CONCATENAR NOMBRE Y APELLIDO
Sub_Batting_Dominic$Nombre <- Prueba

#****************************************************************************************


#CREANDO EL GRAFICO DOTCHAR 
dotchart(Sub_Batting_Dominic$OBP, 
         labels = Sub_Batting_Dominic$Nombre, 
         xlab = "OBP",
         main = "OBP Jugadores Dominicanos de MLB con 1000 hit o más ",
        
         )



#************************************************************************************
#GENERANDO LA TABLAS CON LOS RESULTADOS 
Tabla_Resultado <- Sub_Batting_Dominic%>%
  select(Nombre, H, BB, HBP, AB, SF, OBP)%>%
arrange(desc(OBP))

Tabla_Resultado$playerID = NULL
Tabla_Resultado$nameFirst = NULL



formattable(Tabla_Resultado,
            align = c("l","c","c","c","c", "c", "c", "c", "r")
           
            )
