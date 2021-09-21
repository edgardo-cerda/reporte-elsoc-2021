#Análisis de Cuarentenas por comuna en base a datos del Ministerio de Ciencia (@MinCiencia)
remove(list = ls()) #limpieza del entorno de trabajo
library(readr)
library(dplyr)
library(scales)
library(ggplot2)
library(reshape2)
library(leaflet)
library(ggplot2)
library(data.table)
library(tidyr)
library(stringr)
library(lubridate)
#abrir bases de datos necesarias
load("1_input/datos_elsoc.RData")

#encontrar los id comuna
#library(readxl)
#datos_cit <- read_excel("~/Desktop/THESIS/DATOS_CIT_CENSO_2016_V2 (1).xlsx")

#els_long <- merge(x = elsoc_long, y = datos_cit[ , c("idencuesta","manzana", "comuna", "ciudad")], 
#                  by = c("idencuesta", "comuna"), all.x=TRUE)


## DATA CUARENTENA ---------
# Url para descarga directa de datos desde el Github del Ministerio de Ciencia
url <- "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output"

## Cuarentenas desde 2020/07/28 al día actualizado.
df_pasos <- read_csv(paste(url,"producto74","paso_a_paso_std.csv", sep="/"))
df_pasos$Fecha<-as.Date(df_pasos$Fecha)
df_pasos$dia <- weekdays(as.Date(df_pasos$Fecha))

#new variable dias_cuarentena
df_pasos$cuarentena <- case_when(df_pasos$Paso==1 ~ 1,
                                 df_pasos$Paso==2 & df_pasos$dia=="Saturday" ~ 1,
                                 df_pasos$Paso==2 & df_pasos$dia=="Sunday" ~ 1,
                                 TRUE ~ 0)
df_pasos <- df_pasos %>% 
  filter(zona=="Urbana" | zona=="Total")

#cuarentena acomulada del 2020/07/27 a la fecha de entrevista
df_pasos2 <- df_pasos %>% 
  group_by(codigo_comuna, zona) %>% 
  mutate(csum = cumsum(cuarentena)) %>% 
  mutate(ola=2021)

elsoc_long$fecha_entr <- as.Date(with(elsoc_long, paste(annio_entr, mes_entr, dia_entr ,sep="-")), "%Y-%m-%d")

els_long <- merge(x = elsoc_long, y = df_pasos2[ , c("Fecha","codigo_comuna", "csum", "ola")], 
                   by.x=c("fecha_entr", "comuna_cod", "ola"), 
                   by.y=c("Fecha", "codigo_comuna", "ola"), all.x = T)

## Cuarentenas desde 2021/03 hasta 2020/07/28 
pre_27jul <- read_csv(paste(url,"producto29","Cuarentenas-Totales.csv", sep="/"))
names(pre_27jul) <- names(pre_27jul) %>% str_to_lower() %>% str_replace_all(" ","_")
pre_27jul$fecha_de_inicio <- as.Date(pre_27jul$fecha_de_inicio)
pre_27jul$fecha_de_término <- as.Date(pre_27jul$fecha_de_término)

# cortar en fecha 2020/07/27 
pre_27jul$fecha_de_termino2 <- ifelse(pre_27jul$fecha_de_término>"2020-07-27",
                                      date("2020-07-27"),
                                      pre_27jul$fecha_de_término)

pre_27jul$fecha_termino <- as.Date(pre_27jul$fecha_de_termino2, 
                                   origin = "1970-01-01")
#filtrar fecha_inicio > 2020/07/27 
pre_27jul <- pre_27jul %>% filter(fecha_de_inicio<"2020/07/27")
#calcular dias en cuarentena
pre_27jul$dias <- as.Date(pre_27jul$fecha_termino) - 
  as.Date(pre_27jul$fecha_de_inicio)

pre_27jul$dias <- as.numeric(pre_27jul$dias, units="days")

## Unir fechas
pre_27jul <- pre_27jul %>% 
  group_by(código_cut_comuna) %>% 
  summarise(cuarentena_pre_27jul = sum(dias))  %>% 
  mutate(ola=2021)

els_long2 <- merge(x = els_long, y = pre_27jul, 
                   by.x=c("ola", "comuna_cod"), 
                   by.y=c("ola", "código_cut_comuna"), all.x = T)
els_long2$cuarentena_pre_27jul[is.na(els_long2$cuarentena_pre_27jul)] <- 0
#VARIABLE 1: cuarentena en els_long3 acomulada
els_long2$ccum <- els_long2$cuarentena_pre_27jul + els_long2$csum

#VARIABLE 2: dummy de cuarentena en els_long3
elsoc_covid <- merge(x = els_long2, y = df_pasos, 
                   by.x=c("fecha_entr", "comuna_cod"), 
                   by.y=c("Fecha", "codigo_comuna"), all.x = T)


elsoc_covid$ccum <- as.numeric(as.character(elsoc_covid$ccum))

elsoc_covid$cuarentena <- factor(elsoc_covid$cuarentena,
                            levels = c(0,1),
                            labels = c("Sin Cuarentena", "Con Cuarentena"))

elsoc_covid$ccum_t <- factor(car::recode(elsoc_covid$ccum, "0='0 Dias';1:99='1-99 Dias';
                                        100:149='100-149 Dias';150:500='150 o más Dias'"),
                           labels = c('0 Dias', '1-99 Dias', '100-149', '150 o más Dias'))

elsoc_covid_panel <- elsoc_covid %>% filter(tipo_atricion == 1 | 
                                            tipo_atricion == 17 )

save(elsoc_covid, elsoc_covid_panel, file = '1_input/elsoc_covid.RData')

##Exportar a .dta
#library(haven)
#write_dta(elsoc_covid_panel, "elsoc_covid_panel.dta")

#revisamos
sjmisc::frq(elsoc_covid$cuarentena)     # 44,1% de los encuestados estaba en cuarentena

sjmisc::frq(elsoc_covid$ccum) 

sjmisc::frq(elsoc_covid$tipo_atricion) 

sjmisc::frq(elsoc_covid$zona) 

