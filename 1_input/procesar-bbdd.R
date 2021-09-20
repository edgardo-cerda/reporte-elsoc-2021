
library(pacman)
pacman::p_load(car,dplyr,panelr,stringr,tidyverse,ggplot2,survey,ggrepel,na.tools,scales)
remove(list = ls()) #limpieza del entorno de trabajo
options(scipen=999) #evita notación científica

#-------BASE DE DATOS WIDE---------------
getwd()
load("1_input/ELSOC_Wide_2016_2021_v2.00_R.RData")

elsoc_wide <- elsoc_wide_2016_2021 #acortar nombre bbdd wide

#-------WIDE A LONG---------------
elsoc_long <- long_panel(data = elsoc_wide, prefix = "_w0", begin = 1, end = 5, 
                         label_location = "end", id = "idencuesta", wave = "ola") 

elsoc_long[elsoc_long==-999 | elsoc_long==-888] <- NA #recodificar No sabe y No responde en NA
elsoc_wide[elsoc_wide==-999 | elsoc_wide==-888] <- NA #recodificar No sabe y No responde en NA

save(elsoc_long, elsoc_wide,
     file = '1_input/bbdd_elsoc.RData')
