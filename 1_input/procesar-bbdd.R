
library(pacman)
pacman::p_load(car,dplyr,panelr,stringr,tidyverse,ggplot2,survey,ggrepel,na.tools,scales)
remove(list = ls()) #limpieza del entorno de trabajo
options(scipen=999) #evita notación científica

#-------BASE DE DATOS WIDE---------------
load("/Users/Elisa/Desktop/THESIS/ELSOC_Wide_W01_05_temporal.RData")

elsoc_wide <- elsoc_wide_temporal %>% 
  dplyr::select(starts_with(c("idencuesta","tipo_atricion","muestra","ponderador01","ponderador02","segmento","estrato","region",
                              "dia_entr","mes_entr", "annio_entr",
                              "t01","t02_01", "t02_02","t02_03","t02_04","t03_01","t03_02","t03_03","t03_04",
                              "t04_02","t04_03","t04_04","t04_05","t04_06","t04_07","t05","t06_01","t06_02",
                              "t06_03","t06_04","t06_05","t06_06","t06_07","t06_08","t07_01","t07_02","t08",
                              "t09_01","t09_02","t09_03","t10","t11_01","t11_02","t11_03","t11_04","r06","r07",
                              "r08","r09","r10","r11","r12_01","r12_02","r12_03","r12_04","r12_05","r12_06",
                              "r12_07","c01","c02","c03","c04","c05_01","c05_02","c05_03","c05_04","c05_05",
                              "c05_06","c05_07","c05_08","c07_01","c07_02","c07_03","c07_04","c07_05","c07_06",
                              "c07_07","c07_08","c08_01","c08_02","c08_03","c08_04","c10_01","c10_02","c10_03",
                              "c13","c14_01","c14_02","c15","c16","c16_otro","c17","c17_otro","c18_01","c18_02",
                              "c18_03","c18_04","c18_05","c18_06","c18_07","c18_08","c18_09","c18_10","c18_11","c20",
                              "c20_otro","c21_01","c21_02","c21_03","c21_04","c21_05","c21_06","c21_07","c21_08",
                              "c21_09","c21_10","c21_11","c22","c23","c24","c25","c28","c32_01","c32_02","d01_01",
                              "d01_02","d01_03","d02_01","d02_02","d02_03","d03_01","d03_02","d04_01","d04_02",
                              "d05_01","d05_02","d05_03","d05_04","d06","f05_01","f05_02","f05_03","f05_04",
                              "f05_05","f05_06","f05_07","f06_01","f06_02","s01","s02","s03","s11_01","s11_02",
                              "s11_03","s11_04","s11_05","s11_06","s11_07","s11_08","s11_09","s14", "m0_sexo","m0_edad",
                              "m01","m02","m13","m14","m15","m16","m29","m30","m38","m38_otro","m39","m41", "m60", "m63")))

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

save(elsoc_long, elsoc_wide,
     file = '1_input/bbdd_elsoc.RData')
