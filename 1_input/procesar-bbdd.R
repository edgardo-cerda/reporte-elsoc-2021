
library(pacman)
pacman::p_load(car,dplyr,panelr,stringr,tidyverse,ggplot2,survey,ggrepel,na.tools,scales)
remove(list = ls()) #limpieza del entorno de trabajo
options(scipen=999) #evita notación científica

#-------BASE DE DATOS WIDE---------------
getwd()
load("1_input/ELSOC_Wide_W01_05_temporal.RData")

elsoc_wide <- elsoc_wide_temporal #acortar nombre bbdd wide

#arreglo temporal de ponderadores
elsoc_wide$ponderador02_w05 <- elsoc_wide$ponderador02_w04   #replico el ponderador02 del 2019 al 2021

#-------WIDE A LONG---------------
elsoc_long <- long_panel(data = elsoc_wide, #base de datos formato wide
                         prefix = "_w0", #caracteres antes de la etiqueta de cada ola
                         begin = 1, #etiqueta de la primera ola
                         end = 5, #etiqueta de la última ola
                         label_location = "end", #indica donde se localiza la etiqueta asociada a la ola 
                         id = "idencuesta", #indica identificador individual
                         wave = "ola") #nombre que tomará la variable que indica periodo. 


elsoc_long[elsoc_long==-999 | elsoc_long==-888] <- NA #recodificar No sabe y No responde en NA
elsoc_wide[elsoc_wide==-999 | elsoc_wide==-888] <- NA #recodificar No sabe y No responde en NA

save(elsoc_long, elsoc_wide,
     file = '1_input/bbdd_elsoc.RData')
