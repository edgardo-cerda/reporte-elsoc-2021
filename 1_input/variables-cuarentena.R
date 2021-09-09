remove(list = ls()) #limpieza del entorno de trabajo
#abrir bases de datos necesarias
load("~/Documents/GitHub/covid-elsoc/1_input/elsoc_data.RData")

#encontrar los id comuna
#library(readxl)
#datos_cit <- read_excel("~/Desktop/THESIS/DATOS_CIT_CENSO_2016_V2 (1).xlsx")

#els_long <- merge(x = elsoc_panel_m1, y = datos_cit[ , c("idencuesta","manzana", "comuna", "ciudad")], 
#                  by = c("idencuesta", "comuna"), all.x=TRUE)
#nota: no tengo la comuna para la segunda muestra

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

elsoc_covid_panel <- elsoc_covid %>% filter(tipo_atricion == 1 | 
                                            tipo_atricion == 3 |
                                            tipo_atricion == 5 |
                                            tipo_atricion == 7 | 
                                            tipo_atricion == 9 |
                                            tipo_atricion == 11 | 
                                            tipo_atricion == 13 | 
                                            tipo_atricion == 15 |
                                            tipo_atricion == 17 | 
                                            tipo_atricion == 19)

save(elsoc_covid, elsoc_covid_panel, file = '1_input/elsoc_covid.RData')

##Exportar a .dta
library(haven)
write_dta(elsoc_covid_panel, "3_output/elsoc_covid_panel.dta")

#revisamos
sjmisc::frq(elsoc_covid$cuarentena)     # 44,1% de los encuestados estaba en cuarentena

sjmisc::frq(elsoc_covid$ccum) 

sjmisc::frq(elsoc_covid$tipo_atricion) 

sjmisc::frq(elsoc_covid$zona) 

