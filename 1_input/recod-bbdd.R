#Limpiar enviroment entrada
remove(list = ls()) #limpieza del entorno de trabajo
options(scipen=999) #evita notación científica
####### Script para generar datos a usar en archivo de insumos para discusion con académicos######
library(tidyverse)
library(sticky)

# Importar bases de datos
load("1_input/bbdd_elsoc.RData")

# Factorizar todas las variables posibles
pasar_a_factor <- function(var) {
  if ((length(na.omit(unique(var))) <= 5 )  & 
      length(na.omit(unique(var))) == length(attr(var, which = 'labels')) & 
      class(var) != 'factor') { 
    return(factor(var, labels = names(attr(var, which = 'labels'))))
  } else {
    return(var)
  }
}

elsoc_long <- lapply(elsoc_long, pasar_a_factor) %>% data.frame()
elsoc_long <- sjlabelled::set_label(elsoc_long, var_labels) 

#---------RECODIFICACION OLAS--------------------
elsoc_long$ola <- factor(elsoc_long$ola,labels = c('2016', '2017', '2018', '2019', '2021'))
elsoc_long$ola <- sjlabelled::set_label(elsoc_long$ola, label = c("Ola de Medición"))

#---------RECODIFICACION SEXO--------------------
elsoc_long$m0_sexo <- factor(elsoc_long$m0_sexo,labels = c('Hombre', 'Mujer'))
elsoc_long$m0_sexo <- sjlabelled::set_label(elsoc_long$m0_sexo, label = c("Sexo del Encuestado"))

#---------A.-RECODIFICACION EDUCACION--------------------
elsoc_long$educ <- car::recode(elsoc_long$m01,"c(1,2,3)=1;c(4,5)=2;c(6,7)=3;c(8,9,10)=4")
elsoc_long$educ <- factor(elsoc_long$educ,labels = c("Basica","Media","Tecnica","Universitaria"))

#-----A.1--PONER ATRIBUTOS LABEL Y LABELS
elsoc_long$educ <- sjlabelled::set_label(elsoc_long$educ, label = c("Nivel Educacional"))
elsoc_long$educ <- sjlabelled::set_labels(elsoc_long$educ, labels = c("Basica", "Media", "Tecnica", "Universitaria"))

#----B.- RECODIFICACION RELIGION
elsoc_long$relig <- car::recode(elsoc_long$m38, "1=1;c(2,3)= 2; c(4,5,6)= 3;c(7,8,9)=4")
elsoc_long$relig <- factor(elsoc_long$relig,labels = c("Católico","Evangélico","Otro Credo","No Creyente"))

#-----B.1 PONER ATRIBUTOS LABEL Y LABELS
elsoc_long$relig <- sjlabelled::set_label(elsoc_long$relig, label = c("Religion entrevistado"))
elsoc_long$relig <- sjlabelled::set_labels(elsoc_long$relig, labels = c("Catolico", "Evangelico", "Otro Credo", "No Creyente"))

elsoc_long$estrato <- factor(elsoc_long$estrato, 
                             levels = c(1,2,3,4,5,6),
                             labels = c('Gran Santiago', 'Gran Valparaíso', 'Gran Concepción', 'Ciudades grandes', 'Ciudades medianas', 'Ciudades pequeñas'))

elsoc_long$estrato <- sjlabelled::set_label(elsoc_long$estrato, label = c("Estrato Muestral"))

#-----E REACOFDICACION EDAD-------------------
elsoc_long$edadt <- factor(car::recode(elsoc_long$m0_edad, "18:29=1;30:49=2;50:64=3;65:150=4"),
                           labels = c('18-29', '30-49', '50-64', '65 o más'))
attr(elsoc_long$edadt, which = 'label') <- 'Tramo de edad'

#-----E.1 ATRIBUTOS EDAD----------------------
elsoc_long$edadt <- sjlabelled::set_label(elsoc_long$edadt, label = c("Edad en Tramos"))
elsoc_long$edadt <- sjlabelled::set_labels(elsoc_long$edadt, labels = c("18 a 29", "30 a 49", "50 a 64", " 65 o mas"))

#------F RECODiFiCACION ZONA GEOGRAFICA-------------------------
elsoc_long$zona1  <- car::recode(elsoc_long$region,"c('Tarapaca','Antofagasta','Atacama','Coquimbo','Arica')= 1;c('Valparaiso','Lib. Gral. B. Ohiggins','B. Ohiggins', 'Maule','Bio Bio')= 2;c('Araucania','Los Lagos','Aysen','Magallanes','Los Rios')=3 ;'Metropolitana'= 4")
elsoc_long$zona1  <- factor(elsoc_long$zona1,levels=c("1","2","3","4"),
                            labels = c("Norte","Centro","Sur","Metropolitana"))
#------F.1 ATRIBUTOS ZONA--------
elsoc_long$zona1 <- sjlabelled::set_label(elsoc_long$zona1, label = c("Zona Geográfica"))
elsoc_long$zona1 <- sjlabelled::set_labels(elsoc_long$zona1, labels = c("Norte", "Centro", "Sur", "Metropolitana"))

#------F.2 ZONA GEOGRÁFICA SIN GRANDES URBES
elsoc_long$zona2  <- car::recode(elsoc_long$region,"c('Tarapaca','Antofagasta','Atacama','Coquimbo','Arica')= 1;c('Lib. Gral. B. Ohiggins','B. Ohiggins', 'Maule')= 2;c('Araucania','Los Lagos','Aysen','Magallanes','Los Rios')=3 ;else=NA")
elsoc_long$zona2  <- factor(elsoc_long$zona2,levels=c("1","2","3"),
                            labels = c("Norte","Centro","Sur"))

elsoc_long$zona2 <- sjlabelled::set_label(elsoc_long$zona2, label = c("Zona Geográfica"))
elsoc_long$zona2 <- sjlabelled::set_labels(elsoc_long$zona2, labels = c("Norte", "Centro", "Sur"))

#-------G.- SINTOMATOLOGIA DEPRESION--------------

# Variables de sintomatologia depresiva: 
elsoc_long$s11_01_rec <- as.numeric(car::recode(elsoc_long$s11_01, " 'Nunca' = 0; 'Algunos dias' = 1; 'Mas de la mitad de los dias' = 2; c('Casi todos los dias', 'Todos los dias') = 3")) - 1
elsoc_long$s11_02_rec <- as.numeric(car::recode(elsoc_long$s11_02, " 'Nunca' = 0; 'Algunos dias' = 1; 'Mas de la mitad de los dias' = 2; c('Casi todos los dias', 'Todos los dias') = 3")) - 1
elsoc_long$s11_03_rec <- as.numeric(car::recode(elsoc_long$s11_03, " 'Nunca' = 0; 'Algunos dias' = 1; 'Mas de la mitad de los dias' = 2; c('Casi todos los dias', 'Todos los dias') = 3")) - 1
elsoc_long$s11_04_rec <- as.numeric(car::recode(elsoc_long$s11_04, " 'Nunca' = 0; 'Algunos dias' = 1; 'Mas de la mitad de los dias' = 2; c('Casi todos los dias', 'Todos los dias') = 3")) - 1
elsoc_long$s11_05_rec <- as.numeric(car::recode(elsoc_long$s11_05, " 'Nunca' = 0; 'Algunos dias' = 1; 'Mas de la mitad de los dias' = 2; c('Casi todos los dias', 'Todos los dias') = 3")) - 1
elsoc_long$s11_06_rec <- as.numeric(car::recode(elsoc_long$s11_06, " 'Nunca' = 0; 'Algunos dias' = 1; 'Mas de la mitad de los dias' = 2; c('Casi todos los dias', 'Todos los dias') = 3")) - 1
elsoc_long$s11_07_rec <- as.numeric(car::recode(elsoc_long$s11_07, " 'Nunca' = 0; 'Algunos dias' = 1; 'Mas de la mitad de los dias' = 2; c('Casi todos los dias', 'Todos los dias') = 3")) - 1
elsoc_long$s11_08_rec <- as.numeric(car::recode(elsoc_long$s11_08, " 'Nunca' = 0; 'Algunos dias' = 1; 'Mas de la mitad de los dias' = 2; c('Casi todos los dias', 'Todos los dias') = 3")) - 1
elsoc_long$s11_09_rec <- as.numeric(car::recode(elsoc_long$s11_09, " 'Nunca' = 0; 'Algunos dias' = 1; 'Mas de la mitad de los dias' = 2; c('Casi todos los dias', 'Todos los dias') = 3")) - 1

#PHQ-9: Índice Aditivo de Puntajes de Sintomas Depresivos
elsoc_long$suma_dep <- with(elsoc_long, s11_01_rec + s11_02_rec + s11_03_rec + s11_04_rec + s11_05_rec + s11_06_rec + s11_07_rec + s11_08_rec + s11_09_rec)

elsoc_long$depr <- car::recode(elsoc_long$suma_dep,"c(0,1,2,3,4)='Sin sintomas o Minima';c(5,6,7,8,9)='Depresion Media';
                                       c(10,11,12,13,14)='Depresion Moderada';c(15,16,17,18,19)='Depresion Moderada Severa a Severa';
                                       c(20,21,22,23,24,25,26,27)='Depresion Moderada Severa a Severa'")

elsoc_long$depr <- factor(elsoc_long$depr,c("Sin sintomas o Minima","Depresion Media","Depresion Moderada",
                                            "Depresion Moderada Severa a Severa"))  

#------------------G.1 ATRIBUTOS DEPR---------------
elsoc_long$depr <- sjlabelled::set_label(elsoc_long$depr, label = c("Sintomatologia Depresiva"))

#-------H RECODIFICACION EMPLEO
elsoc_long$empleo <- car::recode(elsoc_long$m02,"c(1,2,3) = 1; 7 = 2; 6 = 3; 5 = 4; c(4, 8, 9) = 5")
elsoc_long$empleo <- factor(elsoc_long$empleo, 
                            labels = c("Trabajo remunerado", "Trabajo doméstico no remunerado", "Desempleado/a", "Jubilado/a o pensionado/a", "Otras categorías"))

attr(elsoc_long$empleo, which = 'label') <- 'Situacion ocupacional'

#-----------I.- INMOBILIRARIO PUBLICO
elsoc_long$inm.pub <- car::recode(elsoc_long$f05_09,"c('Nunca se justifica', 'Pocas veces se justifica') = 0;c('Algunas veces se justifica', 'Muchas veces se justifica', 'Siempre se justifica')=1")
elsoc_long$inm.pub <- sjlabelled::set_label(elsoc_long$inm.pub, label = c(" Se justifica el daño al inmobiliario publico"))
elsoc_long$inm.pub <- sjlabelled::set_labels(elsoc_long$inm.pub, labels = c("No se Justifica", "Se justifica"))

# --- J.- Quintiles de ingreso (del hogar) --- *

#Imputar punto medio de rangos de ingreso
elsoc_long$m30 <- as.numeric(car::recode(elsoc_long$m30,"1=110000;2=251000;3=305000;4=355000;5=400000;
                                           6=445000;7=490000;8=535000;9=585000;10=640000;11=700000;12=765000;
                                           13=845000;14=935000;15=1040000;16=1180000;17=1375000;18=1670000;
                                           19=2275000;20=2700000;NA=NA"))
elsoc_long$m29_imp <- ifelse(!is.na(elsoc_long$m29), elsoc_long$m29, elsoc_long$m30)


# N hogar:
elsoc_long$n_hogar <- with(elsoc_long, case_when(
  ola == 2016 ~ nhogar1,
  ola == 2017 ~ m46_nhogar,
  ola == 2018 ~ m54,
  ola == 2019 ~ m54,
  ola == 2021 ~ m54
))

# Ingreso per capita del hogar:
elsoc_long$ing_pc <- elsoc_long$m29_imp/elsoc_long$n_hogar

elsoc_long <- elsoc_long %>% 
  group_by(ola) %>% 
  mutate(quintil = ntile(-desc(ing_pc), 5)) %>% 
  ungroup()

elsoc_long$quintil <- factor(elsoc_long$quintil,
                             levels = c(1, 2, 3, 4, 5),
                             labels = c('Q1', 'Q2', 'Q3', 'Q4', 'Q5')) # Quintiles como factores

# Agregar etiquetas
attr(elsoc_long$quintil, which = 'label') <- 'Quintil de Ingreso per capita del hogar'
attr(elsoc_long$ing_pc, which = 'label') <- 'Ingreso per capita del hogar' 


#-----------K.- Identificaciaon politica

#Recodificar posicion ideologica
elsoc_long$pos_id <- car::recode(elsoc_long$c15, recodes = "c(0,1,2,3, 4)=1; c(5)=2; c(6,7,8,9,10)=3; c(11,12)=4; else=NA", as.factor = TRUE)

#Pasar a factor
elsoc_long$pos_id <- factor(elsoc_long$pos_id,
                            levels = c(1, 2, 3, 4),
                            labels = c("Izquierda", "Centro", "Derecha", "No se identifica"))
#Atributos
elsoc_long$pos_id <- sjlabelled::set_label(elsoc_long$pos_id, "Posicion ideologica")
elsoc_long$pos_id <- sjlabelled::set_labels(elsoc_long$pos_id, labels = c("Izquierda", "Centro", "Derecha", "No se identifica"))

# Identificacion con partidos

elsoc_long$idpart <- factor(car::recode(elsoc_long$c16,
                                        "c(1,2) = 'PC + PH'; c(3,4) = 'PRO + RD';
                                 c(6,8,12,13) = 'PPD + PDC + PS + PR'; c(10,11,5) = 'RN + UDI + EVO'; 
                                 c(7,9,14) = 'Otro'; 15 = 'Ninguno'"),
                            levels = c('PC + PH', 'PRO + RD', 'PPD + PDC + PS + PR', 'RN + UDI + EVO', 'Otro', 'Ninguno'))


elsoc_long$idcoal <- factor(car::recode(elsoc_long$c17, "c('Otro. Especifique cual')='Otro'"),
                            levels = c('Chile Vamos', 'Nueva Mayoria', 'Frente Amplio',
                                       'Otro', 'Ninguna')) 

elsoc_long$id_sin <- factor(elsoc_long$idpart == 'Ninguno' & elsoc_long$idcoal == 'Ninguna',
                            levels = c(FALSE, TRUE),
                            labels = c('Se identifica con algun partido y/o coalicion politica', 'No se identifica con ningun partido ni coalicion politica'))

elsoc_long$interes_politica <- factor(with(elsoc_long, 
                                           case_when(c13 == 'Nada interesado' ~ 1,
                                                     c13 == 'Poco interesado' | c13 == 'Algo interesado' ~ 2,
                                                     c13 == 'Bastante interesado' | c13 == 'Muy interesado' ~ 3)),
                                      labels = c('Nada interesado',
                                                 'Poco o algo interesado',
                                                 'Bastante o muy interesado'))

# Participacion electoral retrospectiva
elsoc_long <- elsoc_long %>% mutate(
  particip_electoral = factor(case_when(c11 == 1 & ola == 2016 ~ "Si (en 2013)",
                                        (c11 == 2 | c11 == 3) & ola == 2016 ~ "No (en 2013)",
                                        c11 == 1 & ola == 2018 ~ "Si (en 2017)",
                                        c11 == 2 & ola == 2018 ~ "No (en 2017)")))

#-------------L.- Participacion en movimientos sociales-----

elsoc_long$participa <- car::recode(as.numeric(elsoc_long$c22), recodes = "c(1)=1; c(2,3,4,5)=2; else=NA", as.factor = TRUE)

elsoc_long$participa <- factor(elsoc_long$participa,
                               levels = c(1,2),
                               labels = c("No Participa", "Participa"))
elsoc_long$participa <- sjlabelled::set_label(elsoc_long$participa, "Participacion en Mov. Sociales")
elsoc_long$participa <- sjlabelled::set_labels(elsoc_long$participa, labels = c("No Participa", "Participa"))

elsoc_long$mov <- factor(elsoc_long$c20,
                         labels = c('Estudiantil',
                                    'Laboral',
                                    'Ambiental',
                                    'Indigena',
                                    'Diversidad sexual',
                                    'Provida o Antiaborto',
                                    'Antidelincuencia',
                                    'Feminista',
                                    'Pensiones',
                                    'Mov Social de Octubre (18/O)',
                                    'Otro',
                                    'Ninguno'))

# Frecuencia de participacion en movimientos sociales

elsoc_long$freq_mov <- factor(with(elsoc_long, case_when(
  c22 == 'Nunca' | c22 == 'Casi nunca' ~ 3,
  c22 == 'A veces' ~ 2,
  c22 == 'Frecuentemente' | c22 == 'Muy frecuentemente' ~ 1)),
  labels = c('Frecuentemente o muy frecuentemente', 'A veces', 'Nunca o casi nunca'))

elsoc_wide$freq_mov_w01 <- factor(with(elsoc_wide, case_when(
  c22_w01 == 1 | c22_w01 == 2 ~ 3,
  c22_w01 == 3 ~ 2,
  c22_w01 == 4 | c22_w01 == 5 ~ 1)),
  labels = c('Frecuentemente o muy frecuentemente', 'A veces', 'Nunca o casi nunca'))

elsoc_wide$freq_mov_w02 <- factor(with(elsoc_wide, case_when(
  c22_w02 == 1 | c22_w02 == 2 ~ 3,
  c22_w02 == 3 ~ 2,
  c22_w02 == 4 | c22_w02 == 5 ~ 1)),
  labels = c('Frecuentemente o muy frecuentemente', 'A veces', 'Nunca o casi nunca'))

elsoc_wide$freq_mov_w03 <- factor(with(elsoc_wide, case_when(
  c22_w03 == 1 | c22_w03 == 2 ~ 3,
  c22_w03 == 3 ~ 2,
  c22_w03 == 4 | c22_w03 == 5 ~ 1)),
  labels = c('Frecuentemente o muy frecuentemente', 'A veces', 'Nunca o casi nunca'))

elsoc_wide$freq_mov_w04 <- factor(with(elsoc_wide, case_when(
  c22_w04 == 1 | c22_w04 == 2 ~ 3,
  c22_w04 == 3 ~ 2,
  c22_w04 == 4 | c22_w04 == 5 ~ 1)),
  labels = c('Frecuentemente o muy frecuentemente', 'A veces', 'Nunca o casi nunca'))

elsoc_wide$cambio_freq_mov_w04 <- factor(with(elsoc_wide, case_when(
  c22_w04 == c22_w03 & !is.na(c22_w04) & !is.na(c22_w03) ~ 2,
  c22_w04 > c22_w03  & !is.na(c22_w04) & !is.na(c22_w03) ~ 1,
  c22_w04 < c22_w03  & !is.na(c22_w04) & !is.na(c22_w03) ~ 3),
  labels = c('Aumenta', 'Se mantiene', 'Disminuye')))

elsoc_wide$cambio_freq_mov_w04 <- factor(with(elsoc_wide, case_when(
  c22_w04 == c22_w03 & !is.na(c22_w04) & !is.na(c22_w03) ~ 2,
  c22_w04 > c22_w03  & !is.na(c22_w04) & !is.na(c22_w03) ~ 1,
  c22_w04 < c22_w03  & !is.na(c22_w04) & !is.na(c22_w03) ~ 3)),
  labels = c('Aumenta', 'Se mantiene', 'Disminuye'))

elsoc_wide$cambio_freq_mov_w03 <- factor(with(elsoc_wide, case_when(
  c22_w03 == c22_w02 & !is.na(c22_w03) & !is.na(c22_w02) ~ 2,
  c22_w03 > c22_w02  & !is.na(c22_w03) & !is.na(c22_w02) ~ 1,
  c22_w03 < c22_w02  & !is.na(c22_w03) & !is.na(c22_w02) ~ 3)),
  labels = c('Aumenta', 'Se mantiene', 'Disminuye'))

elsoc_wide$cambio_freq_mov_w02 <- factor(with(elsoc_wide, case_when(
  c22_w02 == c22_w01 & !is.na(c22_w02) & !is.na(c22_w01) ~ 2,
  c22_w02 > c22_w01  & !is.na(c22_w02) & !is.na(c22_w01) ~ 1,
  c22_w02 < c22_w01  & !is.na(c22_w02) & !is.na(c22_w01) ~ 3)),
  labels = c('Aumenta', 'Se mantiene', 'Disminuye'))

#------------- X. Relación con inmigrantes -------------------------

elsoc_long$migrantes <- factor(with(elsoc_long, case_when(cuestion_mig == 'Haitianos' ~ 3,
                                                          cuestion_mig == 'Venezolanos' & ola == 2019 ~ 2,
                                                          cuestion_mig == 'Peruanos' | cuestion_mig == 'Venezolanos' ~ 1)),
                               labels = c('Peruanos', 'Venezolanos', 'Haitianos'))

elsoc_long <- elsoc_long %>% 
  mutate(confianza = factor(ifelse(ola == 2016 | ola == 2018, c06_06, r16)),
         norma = case_when(as.numeric(r12_01) + as.numeric(r12_02) <= 4 ~ 1,
                           as.numeric(r12_01) + as.numeric(r12_02) <= 7 ~ 2,
                           as.numeric(r12_01) + as.numeric(r12_02) <= 10 ~ 3),
         amenaza_simbolica = r12_03,
         amenaza_realista = r12_04,
         frecuencia_contacto = factor(car::recode(r06, "c(1,2)=1;c(3)=2;c(4,5)=3"),
                                      labels = c('Contacto Bajo',
                                                 'Contacto Medio',
                                                 'Contacto Alto')),
         calidad_contacto = r07,
         ola_mig = interaction(ola, cuestion_mig))


# Amenaza realista y simbólica dicotómica (FALTA AGREGAR OLA 5):
elsoc_wide$amenaza_realista_w01 <- factor(car::recode(elsoc_wide$r12_03_w01, "c(1,2,3)='En desacuerdo'; c(4,5)='De acuerdo'"),
                                          levels = c("En desacuerdo", "De acuerdo"))
elsoc_wide$amenaza_realista_w02 <- factor(car::recode(elsoc_wide$r12_03_w02, "c(1,2,3)='En desacuerdo'; c(4,5)='De acuerdo'"),
                                          levels = c("En desacuerdo", "De acuerdo"))
elsoc_wide$amenaza_realista_w03 <- factor(car::recode(elsoc_wide$r12_03_w03, "c(1,2,3)='En desacuerdo'; c(4,5)='De acuerdo'"),
                                          levels = c("En desacuerdo", "De acuerdo"))
elsoc_wide$amenaza_realista_w04 <- factor(car::recode(elsoc_wide$r12_03_w04, "c(1,2,3)='En desacuerdo'; c(4,5)='De acuerdo'"),
                                          levels = c("En desacuerdo", "De acuerdo"))

elsoc_wide$amenaza_simbolica_w01 <- factor(car::recode(elsoc_wide$r12_04_w01, "c(1,2,3)='En desacuerdo'; c(4,5)='De acuerdo'"),
                                           levels = c("En desacuerdo", "De acuerdo"))
elsoc_wide$amenaza_simbolica_w02 <- factor(car::recode(elsoc_wide$r12_04_w02, "c(1,2,3)='En desacuerdo'; c(4,5)='De acuerdo'"),
                                           levels = c("En desacuerdo", "De acuerdo"))
elsoc_wide$amenaza_simbolica_w03 <- factor(car::recode(elsoc_wide$r12_04_w03, "c(1,2,3)='En desacuerdo'; c(4,5)='De acuerdo'"),
                                           levels = c("En desacuerdo", "De acuerdo"))
elsoc_wide$amenaza_simbolica_w04 <- factor(car::recode(elsoc_wide$r12_04_w04, "c(1,2,3)='En desacuerdo'; c(4,5)='De acuerdo'"),
                                           levels = c("En desacuerdo", "De acuerdo"))

# Confianza:
elsoc_wide <-elsoc_wide %>% 
  mutate(confianza_w01 = c06_06_w01,
         confianza_w02 = r16_w02,
         confianza_w03 = c06_06_w03,
         confianza_w04 = r16_w04)

# Normas
elsoc_wide <-elsoc_wide %>% 
  mutate(norma_w01 = case_when(r12_01_w01 + r12_02_w01 <= 3 ~ 1,
                               r12_01_w01 + r12_02_w01 <= 7 ~ 2,
                               r12_01_w01 + r12_02_w01 <= 10 ~ 3),
         norma_w02 = case_when(r12_01_w02 + r12_02_w02 <= 3 ~ 1,
                               r12_01_w02 + r12_02_w02 <= 7 ~ 2,
                               r12_01_w02 + r12_02_w02 <= 10 ~ 3),
         norma_w03 = case_when(r12_01_w03 + r12_02_w03 <= 3 ~ 1,
                               r12_01_w03 + r12_02_w03 <= 7 ~ 2,
                               r12_01_w03 + r12_02_w03 <= 10 ~ 3),
         norma_w04 = case_when(r12_01_w04 + r12_02_w04 <= 3 ~ 1,
                               r12_01_w04 + r12_02_w04 <= 7 ~ 2,
                               r12_01_w04 + r12_02_w04 <= 10 ~ 3))


# Cambio en amenaza simbolica
elsoc_wide$cambio_amenaza_simbolica_w04 <- factor(with(elsoc_wide, case_when(
  r12_03_w04 == r12_03_w03 & !is.na(r12_03_w04) & !is.na(r12_03_w03) ~ 2,
  r12_03_w04 > r12_03_w03  & !is.na(r12_03_w04) & !is.na(r12_03_w03) ~ 1,
  r12_03_w04 < r12_03_w03  & !is.na(r12_03_w04) & !is.na(r12_03_w03) ~ 3)),
  labels = c('Aumenta', 'Se mantiene', 'Disminuye'))

elsoc_wide$cambio_amenaza_simbolica_w03 <- factor(with(elsoc_wide, case_when(
  r12_03_w04 == r12_03_w03 & !is.na(r12_03_w04) & !is.na(r12_03_w03) ~ 2,
  r12_03_w04 > r12_03_w03  & !is.na(r12_03_w04) & !is.na(r12_03_w03) ~ 1,
  r12_03_w04 < r12_03_w03  & !is.na(r12_03_w04) & !is.na(r12_03_w03) ~ 3)),
  labels = c('Aumenta', 'Se mantiene', 'Disminuye'))

elsoc_wide$cambio_amenaza_simbolica_w02 <- factor(with(elsoc_wide, case_when(
  r12_03_w03 == r12_03_w02 & !is.na(r12_03_w03) & !is.na(r12_03_w02) ~ 2,
  r12_03_w03 > r12_03_w02  & !is.na(r12_03_w03) & !is.na(r12_03_w02) ~ 1,
  r12_03_w03 < r12_03_w02  & !is.na(r12_03_w03) & !is.na(r12_03_w02) ~ 3)),
  labels = c('Aumenta', 'Se mantiene', 'Disminuye'))

# Cambio en amenaza realista
elsoc_wide$cambio_amenaza_realista_w04 <- factor(with(elsoc_wide, case_when(
  r12_04_w04 == r12_04_w03 & !is.na(r12_04_w04) & !is.na(r12_04_w03) ~ 2,
  r12_04_w04 > r12_04_w03  & !is.na(r12_04_w04) & !is.na(r12_04_w03) ~ 1,
  r12_04_w04 < r12_04_w03  & !is.na(r12_04_w04) & !is.na(r12_04_w03) ~ 3)),
  labels = c('Aumenta', 'Se mantiene', 'Disminuye'))

elsoc_wide$cambio_amenaza_realista_w03 <- factor(with(elsoc_wide, case_when(
  r12_04_w04 == r12_04_w03 & !is.na(r12_04_w04) & !is.na(r12_04_w03) ~ 2,
  r12_04_w04 > r12_04_w03  & !is.na(r12_04_w04) & !is.na(r12_04_w03) ~ 1,
  r12_04_w04 < r12_04_w03  & !is.na(r12_04_w04) & !is.na(r12_04_w03) ~ 3)),
  labels = c('Aumenta', 'Se mantiene', 'Disminuye'))

elsoc_wide$cambio_amenaza_realista_w02 <- factor(with(elsoc_wide, case_when(
  r12_04_w03 == r12_04_w02 & !is.na(r12_04_w03) & !is.na(r12_04_w02) ~ 2,
  r12_04_w03 > r12_04_w02  & !is.na(r12_04_w03) & !is.na(r12_04_w02) ~ 1,
  r12_04_w03 < r12_04_w02  & !is.na(r12_04_w03) & !is.na(r12_04_w02) ~ 3)),
  labels = c('Aumenta', 'Se mantiene', 'Disminuye'))

attributes(elsoc_wide$cambio_amenaza_realista_w04)

#-----------------M.- Clase subjetiva en cinco categorias-------

#--------------------N.- Clase subjetiva en tres categorias-------------------------
elsoc_long$clase.sub <- car::recode(as.numeric(elsoc_long$d01_01), recodes = "c(0,1,2,3)=1; c(4, 5, 6)=2; c(7,8,9,10)=3;else=NA", as.factor = TRUE)

elsoc_long$clase.sub <- factor(elsoc_long$clase.sub,
                               levels = c(1,2,3),
                               labels = c("Baja", "Media", "Alta"))
elsoc_long$clase.sub <- sjlabelled::set_label(elsoc_long$clase.sub, "Clase social subjetiva")
elsoc_long$clase.sub <- sjlabelled::set_labels(elsoc_long$clase.sub, labels = c("Baja", "Media", "Alta"))

#-------------------CLASE SUBJETIVA-------------------------------
elsoc_long$clase.sub.hijos <- car::recode(as.numeric(elsoc_long$d01_03), recodes = "c(0,1,2,3)=1; c(4, 5, 6)=2; c(7, 8, 9,10)=3; else=NA", as.factor = TRUE)

elsoc_long$clase.sub.hijos <- factor(elsoc_long$clase.sub.hijos,
                                     levels = c(1,2,3),
                                     labels = c("Baja", "Media", "Alta"))

elsoc_long$clase.sub.hijos <- sjlabelled::set_label(elsoc_long$clase.sub.hijos, "Clase social subjetiva de sus hijos")
elsoc_long$clase.sub.hijos <- sjlabelled::set_labels(elsoc_long$clase.sub.hijos, labels = c("Baja", "Media", "Alta"))

#---Ubicacion numerica en la sociedad

elsoc_long$clase.sub.num <- as.numeric(elsoc_long$d01_01)
elsoc_long$clase.sub.16  <-ifelse(elsoc_long$ola == 2016, elsoc_long$clase.sub.num, NA)
elsoc_long$clase.sub.16  <- sjlabelled::set_label(elsoc_long$clase.sub.16, "Clase subjetiva 2016")

elsoc_long$clase.sub.19  <-ifelse(elsoc_long$ola == 2019, elsoc_long$clase.sub.num, NA)

#----------------------------------
#--------TRATAMIENTO DE OUTLIERS (Se crea una nueva variable con el 98% de la distribucion y el resto NA)
#2.- d03_02
elsoc_long$d03_02_r <- elsoc_long$d03_02
elsoc_long$d03_02_r[elsoc_long$d03_02_r < quantile(elsoc_long$d03_02_r, .01, na.rm = TRUE)[[1]] | elsoc_long$d03_02_r > quantile(elsoc_long$d03_02_r, .99, na.rm = TRUE)[[1]]] <- NA
#4.- d04_02
elsoc_long$d04_02_r <- elsoc_long$d04_02
elsoc_long$d04_02_r[elsoc_long$d04_02_r < quantile(elsoc_long$d04_02_r, .01, na.rm = TRUE)[[1]] | elsoc_long$d04_02_r > quantile(elsoc_long$d04_02_r, .99, na.rm = TRUE)[[1]]] <- NA
#5.- m15
elsoc_long$m15_r <- elsoc_long$m15
elsoc_long$m15_r[elsoc_long$m15_r < quantile(elsoc_long$m15_r, .01, na.rm = TRUE)[[1]] | elsoc_long$m15_r > quantile(elsoc_long$m15_r, .99, na.rm = TRUE)[[1]]] <- NA
#1.- d03_01
elsoc_long$d03_01_r <- elsoc_long$d03_01
elsoc_long$d03_01_r[ elsoc_long$d03_01_r < quantile(elsoc_long$d03_01_r, .01, na.rm = TRUE)[[1]] | elsoc_long$d03_01_r > quantile(elsoc_long$d03_01_r, .999, na.rm = TRUE)[[1]]] <- NA

#3.- d04_01
elsoc_long$d04_01_r <- elsoc_long$d04_01
elsoc_long$d04_01_r[elsoc_long$d04_01_r < quantile(elsoc_long$d04_01_r, .01, na.rm = TRUE)[[1]] | elsoc_long$d04_01_r > quantile(elsoc_long$d04_01_r, .99, na.rm = TRUE)[[1]]] <- NA


#----------------------------------------------------------------------------------------------------
#----ESCALAS----

### GENERALES
#1.- PROSOCIAL
elsoc_long$prosocial <-  c(as.numeric(elsoc_long$c07_01) + as.numeric(elsoc_long$c07_02) + as.numeric(elsoc_long$c07_03) +
                             as.numeric(elsoc_long$c07_04) + as.numeric(elsoc_long$c07_05) + as.numeric(elsoc_long$c07_06) +
                             as.numeric(elsoc_long$c07_07) + as.numeric(elsoc_long$c07_08)) / 8


###TERRITORIO

#I.- CONFLICTOS BARRIALES 
elsoc_long$confli.barrial <- c(as.numeric(elsoc_long$t11_01) + as.numeric(elsoc_long$t11_02) +
                                 as.numeric(elsoc_long$t11_03) + as.numeric(elsoc_long$t11_04)) / 4

elsoc_long$apoyo.soci <- c(as.numeric(elsoc_long$c07_01)+ as.numeric(elsoc_long$c07_03)) /2

elsoc_long$confli.barrial.rec <- factor(with(elsoc_long, case_when(confli.barrial == 1 ~ 3,
                                                                   confli.barrial < 3 ~ 2,
                                                                   confli.barrial <= 5 ~ 1)),
                                        labels = c('Muchas veces o siempre', 'Pocas o algunas veces','Nunca' ))

### CAMBIO CONSTITUCIONAL ####

# Recodificar variables de conformidad y acuerdo de cambio constitucion
elsoc_long$conformidad_constitucion <- factor(with(elsoc_long, case_when(
  c26 == 'Muy disconforme' | c26 == 'Disconforme' ~ 1,
  c26 == 'Indiferente' ~ 2,
  c26 == 'Conforme' | c26 == 'Muy conforme' ~ 3)),
  labels = c('Disconforme o muy disconforme', 'Indiferente', 'Conforme o muy conforme'))

elsoc_long$acuerdo_cambio_constitucion <- factor(with(elsoc_long, case_when(
  c28 == 'Totalmente en desacuerdo' | c28 == 'En desacuerdo' ~ 1,
  c28 == 'Ni de acuerdo ni en desacuerdo' ~ 2,
  c28 == 'De acuerdo' | c28 == 'Totalmente de acuerdo' ~ 3)),
  labels = c('En desacuerdo o totalmente en desacuerdo', 'Ni de acuerdo ni en desacuerdo', 'De acuerdo o totalmente de acuerdo'))

# Participacion en procesos constituyentes:

table(elsoc_long$ola, elsoc_long$c31)

elsoc_long$participacion_2016 <- factor(with(elsoc_long, case_when(
  c31 == 'Si' & ola == 2016 ~ 'Si', 
  c31 == 'No' & ola == 2016 ~ 'No'),
  labels = c('Si', 'No')))

elsoc_long$participacion_2019 <- factor(with(elsoc_long, case_when(
  c31 == 'Si' & ola == 2019 ~ 'Si', 
  c31 == 'No' & ola == 2019 ~ 'No'),
  labels = c('Si', 'No')))

elsoc_long$just_violencia_cambio_prom <- with(elsoc_long, c(as.numeric(f05_09)+as.numeric(f05_10)+ as.numeric(f05_11))/3)

elsoc_long$just_violencia_cambio <- factor(with(elsoc_long, 
                                                case_when(just_violencia_cambio_prom == 1 ~ 1,
                                                          just_violencia_cambio_prom < 3 ~ 2,
                                                          just_violencia_cambio_prom <= 5 ~ 3)),
                                           labels = c('Nunca se justifica', 'Pocas o algunas veces se justifica', 'Muchas veces o siempre se justifica'))


### Justificación de la violencia para el cambio social ###
elsoc_long$just_violencia_cambio_prom <- with(elsoc_long, c(as.numeric(f05_09)+as.numeric(f05_10)+ as.numeric(f05_11))/3)

elsoc_long$just_violencia_cambio <- factor(with(elsoc_long, 
                                                case_when(just_violencia_cambio_prom == 1 ~ 1,
                                                          just_violencia_cambio_prom < 3 ~ 2,
                                                          just_violencia_cambio_prom <= 5 ~ 3)),
                                           labels = c('Nunca se justifica', 'Pocas o algunas veces se justifica', 'Muchas veces o siempre se justifica'))
elsoc_long$f05_01_rec <- factor(with(elsoc_long, 
                                     case_when(f05_01 == 'Nunca se justifica' ~ 1,
                                               f05_01 == 'Pocas veces se justifica' | f05_01 == 'Algunas veces se justifica' ~ 2,
                                               f05_01 == 'Muchas veces se justifica' | f05_01 == 'Siempre se justifica' ~ 3)),
                                labels = c('Nunca se justifica', 'Pocas o algunas veces se justifica', 'Muchas veces o siempre se justifica'))

elsoc_long$f05_02_rec <- factor(with(elsoc_long, 
                                     case_when(f05_02 == 'Nunca se justifica' ~ 1,
                                               f05_02 == 'Pocas veces se justifica' | f05_02 == 'Algunas veces se justifica' ~ 2,
                                               f05_02 == 'Muchas veces se justifica' | f05_02 == 'Siempre se justifica' ~ 3)),
                                labels = c('Nunca se justifica', 'Pocas o algunas veces se justifica', 'Muchas veces o siempre se justifica'))

elsoc_long$f05_03_rec <- factor(with(elsoc_long, 
                                     case_when(f05_03 == 'Nunca se justifica' ~ 1,
                                               f05_03 == 'Pocas veces se justifica' | f05_03 == 'Algunas veces se justifica' ~ 2,
                                               f05_03 == 'Muchas veces se justifica' | f05_03 == 'Siempre se justifica' ~ 3)),
                                labels = c('Nunca se justifica', 'Pocas o algunas veces se justifica', 'Muchas veces o siempre se justifica'))
elsoc_long$f05_04_rec <- factor(with(elsoc_long, 
                                     case_when(f05_04 == 'Nunca se justifica' ~ 1,
                                               f05_04 == 'Pocas veces se justifica' | f05_04 == 'Algunas veces se justifica' ~ 2,
                                               f05_04 == 'Muchas veces se justifica' | f05_04 == 'Siempre se justifica' ~ 3)),
                                labels = c('Nunca se justifica', 'Pocas o algunas veces se justifica', 'Muchas veces o siempre se justifica'))
elsoc_long$f05_07_rec <- factor(with(elsoc_long, 
                                     case_when(f05_07 == 'Nunca se justifica' ~ 1,
                                               f05_07 == 'Pocas veces se justifica' | f05_07 == 'Algunas veces se justifica' ~ 2,
                                               f05_07 == 'Muchas veces se justifica' | f05_07 == 'Siempre se justifica' ~ 3)),
                                labels = c('Nunca se justifica', 'Pocas o algunas veces se justifica', 'Muchas veces o siempre se justifica'))

elsoc_long$f05_09_rec <- factor(with(elsoc_long, 
                                     case_when(f05_09 == 'Nunca se justifica' ~ 1,
                                               f05_09 == 'Pocas veces se justifica' | f05_09 == 'Algunas veces se justifica' ~ 2,
                                               f05_09 == 'Muchas veces se justifica' | f05_09 == 'Siempre se justifica' ~ 3)),
                                labels = c('Nunca se justifica', 'Pocas o algunas veces se justifica', 'Muchas veces o siempre se justifica'))

elsoc_long$f05_10_rec <- factor(with(elsoc_long, 
                                     case_when(f05_10 == 'Nunca se justifica' ~ 1,
                                               f05_10 == 'Pocas veces se justifica' | f05_10 == 'Algunas veces se justifica' ~ 2,
                                               f05_10 == 'Muchas veces se justifica' | f05_10 == 'Siempre se justifica' ~ 3)),
                                labels = c('Nunca se justifica', 'Pocas o algunas veces se justifica', 'Muchas veces o siempre se justifica'))

elsoc_long$f05_11_rec <- factor(with(elsoc_long, 
                                     case_when(f05_11 == 'Nunca se justifica' ~ 1,
                                               f05_11 == 'Pocas veces se justifica' | f05_11 == 'Algunas veces se justifica' ~ 2,
                                               f05_11 == 'Muchas veces se justifica' | f05_11 == 'Siempre se justifica' ~ 3)),
                                labels = c('Nunca se justifica', 'Pocas o algunas veces se justifica', 'Muchas veces o siempre se justifica'))

elsoc_long$t16_rec <- factor(with(elsoc_long, 
                                  case_when(t16 == 'Nunca se justifica' ~ 1,
                                            t16 == 'Pocas veces se justifica' | t16 == 'Algunas veces se justifica' ~ 2,
                                            t16 == 'Muchas veces se justifica' | t16 == 'Siempre se justifica' ~ 3)),
                             labels = c('Nunca se justifica', 'Pocas o algunas veces se justifica', 'Muchas veces o siempre se justifica'))

elsoc_long$t17_rec <- factor(with(elsoc_long, 
                                  case_when(t17 == 'Nunca se justifica' ~ 1,
                                            t17 == 'Pocas veces se justifica' | t17 == 'Algunas veces se justifica' ~ 2,
                                            t17 == 'Muchas veces se justifica' | t17 == 'Siempre se justifica' ~ 3)),
                             labels = c('Nunca se justifica', 'Pocas o algunas veces se justifica', 'Muchas veces o siempre se justifica'))

### PERCEPCION DE DESIGUALDAD Y MERITO
elsoc_long$merito <- c(as.numeric(elsoc_long$d05_01) + as.numeric(elsoc_long$d05_02) + 
                         as.numeric(elsoc_long$d05_03) + as.numeric(elsoc_long$d05_04))/4
# CONFIANZA EN ISNTITUCIONES
elsoc_long$confi.insti <- c(as.numeric(elsoc_long$c05_01) + as.numeric(elsoc_long$c05_02) + 
                              as.numeric(elsoc_long$c05_03) + as.numeric(elsoc_long$c05_04) +
                              as.numeric(elsoc_long$c05_05) + as.numeric(elsoc_long$c05_06) +
                              as.numeric(elsoc_long$c05_07) + as.numeric(elsoc_long$c05_08)) /8


# RECOMPENSA DEL MERITO (Concepto "Justicia distributiva y meritocracia")
elsoc_long$recompensa <- c(as.numeric(elsoc_long$c18_09) +
                             as.numeric(elsoc_long$c18_10)) /2 

# AUTOEFICACIA POLITICA
elsoc_long$autopolitica <- c(as.numeric(elsoc_long$c10_01) + as.numeric(elsoc_long$c10_02)+
                               as.numeric(elsoc_long$c10_03)) /3

# TOLERANCIA 

elsoc_long$tolera <- c(as.numeric(elsoc_long$d26_01) + as.numeric(elsoc_long$d26_02)+
                         as.numeric(elsoc_long$d26_03)+as.numeric(elsoc_long$d26_04)) /4

#----SOCIABILIDAD BARRIAL----
elsoc_long$sociabili <-  c(as.numeric(elsoc_long$t03_01) + as.numeric(elsoc_long$t03_02) + as.numeric(elsoc_long$t03_03) +
                             as.numeric(elsoc_long$t03_04)) / 4


elsoc_long$sociabili.rec <- factor(with(elsoc_long, case_when(sociabili < 3 ~ 3,
                                                              sociabili < 4 ~ 2,
                                                              sociabili <= 5 ~ 1)),
                                   labels = c('Alta', 'Media','Baja' ))

#------Con Pareja sin Pareja
elsoc_long$pareja <- factor(car::recode(elsoc_long$m36, "c(4,5,6,7,8)=0;c(1,2,3)= 1;else=NA", as.factor = T),
                            labels = c("Sin Pareja","Con Pareja"))


#------------MODULO 5------------------
elsoc_long$d07_rec <- factor(car::recode(as.numeric(elsoc_long$d07), 'c(1,2)=1;3=2;c(4,5)= 3; else=NA', as.factor = TRUE),
                             levels = c(1,2,3),
                             labels = c('Bajo Contacto', 'Contacto Medio', 'Alto Contacto'))
#Recodificacion Contacto Clase baja
elsoc_long$d13_rec <- factor(car::recode(as.numeric(elsoc_long$d13), 'c(1,2)=1;3=2;c(4,5)= 3; else=NA', as.factor = TRUE),
                             levels = c(1,2,3),
                             labels = c('Bajo Contacto', 'Contacto Medio', 'Alto Contacto'))


#Recodificacion Contacto  Positivo Clase Alta
elsoc_long$d08_rec <- factor(car::recode(as.numeric(elsoc_long$d08), 'c(1,2)=1;3=2;c(4,5)= 3; else=NA', as.factor = TRUE),
                             levels = c(1,2,3),
                             labels = c('Bajo Contacto Positivo', 'Medio Contacto Positivo', 'Alto Contacto Positivo'))
#Recodificacion Contacto  Positivo Clase baja
elsoc_long$d14_rec <- factor(car::recode(as.numeric(elsoc_long$d14), 'c(1,2)=1;3=2;c(4,5)= 3; else=NA', as.factor = TRUE),
                             levels = c(1,2,3),
                             labels = c('Bajo Contacto Positivo', 'Medio Contacto Positivo', 'Alto Contacto Positivo'))

#Recodificacion Contacto  negativo Clase Alta
elsoc_long$d09_rec <- factor(car::recode(as.numeric(elsoc_long$d09), 'c(1,2)=1;3=2;c(4,5)= 3; else=NA', as.factor = TRUE),
                             levels = c(1,2,3),
                             labels = c('Bajo Contacto Negativo', 'Medio Contacto Negativo', 'Alto Contacto Negativo'))
#Recodificacion Contacto  negativo Clase baja
elsoc_long$d15_rec <- factor(car::recode(as.numeric(elsoc_long$d15), 'c(1,2)=1;3=2;c(4,5)= 3; else=NA', as.factor = TRUE),
                             levels = c(1,2,3),
                             labels = c('Bajo Contacto Negativo', 'Medio Contacto Negativo', 'Alto Contacto Negativo'))

# TRato Justo por grupos (d25)
elsoc_long$trato_justo_cambio_prom <- with(elsoc_long, c(as.numeric(d25_01)+as.numeric(d25_02)+ as.numeric(d25_03) +
                                                           as.numeric(d25_04) + as.numeric(d25_05) + as.numeric(d25_06))/6)

elsoc_long$trato_justo_cambio <- factor(with(elsoc_long, 
                                             case_when(elsoc_long$trato_justo_cambio_prom  <= 3 ~ 1,
                                                       elsoc_long$trato_justo_cambio_prom < 7 ~ 4,
                                                       elsoc_long$trato_justo_cambio_prom <= 10 ~ 8)),
                                        labels = c('Mal Trato', 'Trato Medio', 'Trato Alto'))
# Trato respetuoso a grupos (c35)
elsoc_long$trato_respe_prom <- with(elsoc_long, c(as.numeric(c35_01)+as.numeric(c35_02)+ as.numeric(c35_03) +
                                                    as.numeric(c35_04)) /4)

# Deprivacion relativa
#invertir items
elsoc_long$d27_02_inv <- car::recode(as.numeric(elsoc_long$d27_02), '1=5; 2=4; 3=3; 4=2; 5=1', as.factor = TRUE)
elsoc_long$d27_04_inv <- car::recode(as.numeric(elsoc_long$d27_04), '1=5; 2=4; 3=3; 4=2; 5=1', as.factor = TRUE)

#Índice de Deprivacion Relativa Individual;
elsoc_long$dep.indi <- with(elsoc_long, c(as.numeric(d27_01)+as.numeric(d27_02_inv))/2)

elsoc_long$dep.indi.rec <- factor(with(elsoc_long, case_when(dep.indi < 3 ~ 1,
                                                             dep.indi < 4 ~ 2,
                                                             dep.indi <= 5 ~ 3)),
                                  labels = c('Baja', 'Media', 'Alta'))

#Índice de Deprivacion Relativa grupal;
elsoc_long$dep.grup <- with(elsoc_long, c(as.numeric(d27_03)+as.numeric(d27_04_inv)+as.numeric(d27_05))/3)

elsoc_long$dep.grup.rec <- factor(with(elsoc_long, case_when(dep.grup < 3 ~ 1,
                                                             dep.grup < 4 ~ 2,
                                                             dep.grup <= 5 ~ 3)),
                                  labels = c('Baja', 'Media', 'Alta'))

elsoc_long$depriva <- c(as.numeric(elsoc_long$d27_01) + as.numeric(elsoc_long$d27_02) +
                          as.numeric(elsoc_long$d27_03) + as.numeric(elsoc_long$d27_04)+as.numeric(elsoc_long$d27_05)) /5


################################# Cambios en base WIDE #####################

#Edad
elsoc_wide$edad_w01 <- factor(car::recode(elsoc_wide$m0_edad_w01, "18:29=1;30:49=2;50:64=3;65:100=4"),
                              labels = c('18-29', '30-49', '50-64', '65 o mas'))
elsoc_wide$edad_w02 <- factor(car::recode(elsoc_wide$m0_edad_w02, "18:29=1;30:49=2;50:64=3;65:100=4"),
                              labels = c('18-29', '30-49', '50-64', '65 o mas'))
elsoc_wide$edad_w03 <- factor(car::recode(elsoc_wide$m0_edad_w03, "18:29=1;30:49=2;50:64=3;65:100=4"),
                              labels = c('18-29', '30-49', '50-64', '65 o mas'))
elsoc_wide$edad_w04 <- factor(car::recode(elsoc_wide$m0_edad_w04, "18:29=1;30:49=2;50:64=3;65:100=4"),
                              labels = c('18-29', '30-49', '50-64', '65 o mas'))
elsoc_wide$edad_w05 <- factor(car::recode(elsoc_wide$m0_edad_w05, "18:29=1;30:49=2;50:64=3;65:100=4"),
                              labels = c('18-29', '30-49', '50-64', '65 o mas'))

#Educacion Entrevistado
elsoc_wide$educ_w01 <- factor(car::recode(elsoc_wide$m01_w01, "c(1,2,3)='Basica';c(4,5)='Media';c(6,7)='Tecnica';c(8,9,10)='Universitaria'"))
elsoc_wide$educ_w02 <- factor(car::recode(elsoc_wide$m01_w02, "c(1,2,3)='Basica';c(4,5)='Media';c(6,7)='Tecnica';c(8,9,10)='Universitaria'"))
elsoc_wide$educ_w03 <- factor(car::recode(elsoc_wide$m01_w03, "c(1,2,3)='Basica';c(4,5)='Media';c(6,7)='Tecnica';c(8,9,10)='Universitaria'"))
elsoc_wide$educ_w04 <- factor(car::recode(elsoc_wide$m01_w04, "c(1,2,3)='Basica';c(4,5)='Media';c(6,7)='Tecnica';c(8,9,10)='Universitaria'"))
elsoc_wide$educ_w05 <- factor(car::recode(elsoc_wide$m01_w05, "c(1,2,3)='Basica';c(4,5)='Media';c(6,7)='Tecnica';c(8,9,10)='Universitaria'"))


elsoc_wide$pos_id_w01  <- factor(car::recode(elsoc_wide$c15_w01,"c(11,12)='No se identifica';c(0,1,2,3,4)='Izquierda';  
                                      c(5)='Centro';c(6,7,8,9,10)='Derecha'"),
                                 levels = c('Izquierda', 'Centro', 'Derecha', 'No se identifica'))

elsoc_wide$pos_id_w02  <- factor(car::recode(elsoc_wide$c15_w02,"c(11,12)='No se identifica';c(0,1,2,3,4)='Izquierda';  
                                      c(5)='Centro';c(6,7,8,9,10)='Derecha'"),
                                 levels = c('Izquierda', 'Centro', 'Derecha', 'No se identifica'))

elsoc_wide$pos_id_w03  <- factor(car::recode(elsoc_wide$c15_w03,"c(11,12)='No se identifica';c(0,1,2,3,4)='Izquierda';  
                                      c(5)='Centro';c(6,7,8,9,10)='Derecha'"),
                                 levels = c('Izquierda', 'Centro', 'Derecha', 'No se identifica'))

elsoc_wide$pos_id_w04  <- factor(car::recode(elsoc_wide$c15_w04,"c(11,12)='No se identifica';c(0,1,2,3,4)='Izquierda';  
                                      c(5)='Centro';c(6,7,8,9,10)='Derecha'"),
                                 levels = c('Izquierda', 'Centro', 'Derecha', 'No se identifica'))

elsoc_wide$pos_id_w05  <- factor(car::recode(elsoc_wide$c15_w05,"c(11,12)='No se identifica';c(0,1,2,3,4)='Izquierda';  
                                      c(5)='Centro';c(6,7,8,9,10)='Derecha'"),
                                 levels = c('Izquierda', 'Centro', 'Derecha', 'No se identifica'))
# Recodificar variables de conformidad y acuerdo de cambio constitucion
elsoc_wide$c28_w03_rec <- factor(with(elsoc_wide, case_when(
  c28_w03 == 1 | c28_w03 == 2 ~ 1,
  c28_w03 == 3 ~ 2,
  c28_w03 == 4 | c28_w03 == 5 ~ 3)))

elsoc_wide$c28_w04_rec <- factor(with(elsoc_wide, case_when(
  c28_w04 == 1 | c28_w04 == 2 ~ 1,
  c28_w04 == 3 ~ 2,
  c28_w04 == 4 | c28_w04 == 5 ~ 3)))

elsoc_wide$acuerdo_cambio_constitucion <- factor(with(elsoc_wide, case_when(
  c28_w03_rec == 1 & c28_w04_rec == 1 ~ 2,
  c28_w03_rec == 2 & c28_w04_rec == 2 ~ 2,
  c28_w03_rec == 3 & c28_w04_rec == 3 ~ 3,
  c28_w03_rec != 1 & !is.na(c28_w03_rec) & c28_w04_rec == 1 ~ 1,
  c28_w03_rec != 2 & !is.na(c28_w03_rec) & c28_w04_rec == 2 ~ 1,
  c28_w03_rec != 3 & !is.na(c28_w03_rec) & c28_w04_rec == 3 ~ 4)),
  labels = c('Pasa a estar en contra o indiferente a cambio constitucional',
             'Se mantiene en contra o indiferente a cambio constitucional',
             'Mantiene apoyo a cambio constitucional',
             'Pasa a apoyar cambio constitucional'))

# Identificacion con algun partido, por ola
elsoc_wide$idpart_w01 <- factor(with(elsoc_wide, case_when(
  c16_w01 == 15 ~ 0,
  is.na(c16_w01) ~ -999,
  TRUE ~ 1)), exclude = -999)
elsoc_wide$idpart_w02 <- factor(with(elsoc_wide, case_when(
  c16_w02 == 15 ~ 0,
  is.na(c16_w02) ~ -999,
  TRUE ~ 1)), exclude = -999)
elsoc_wide$idpart_w03 <- factor(with(elsoc_wide, case_when(
  c16_w03 == 15 ~ 0,
  is.na(c16_w03) ~ -999,
  TRUE ~ 1)), exclude = -999)
elsoc_wide$idpart_w04 <- factor(with(elsoc_wide, case_when(
  c16_w04 == 15 ~ 0,
  is.na(c16_w04) ~ -999,
  TRUE ~ 1)), exclude = -999)
elsoc_wide$idpart_w05 <- factor(with(elsoc_wide, case_when(
  c16_w05 == 15 ~ 0,
  is.na(c16_w05) ~ -999,
  TRUE ~ 1)), exclude = -999)

# Identificacion con alguna coalicion, por ola
elsoc_wide$idcoal_w01 <- factor(with(elsoc_wide, case_when(
  c17_w01 == 5 ~ 0,
  is.na(c17_w01) ~ -999,
  TRUE ~ 1)), exclude = -999)
elsoc_wide$idcoal_w02 <- factor(with(elsoc_wide, case_when(
  c17_w02 == 5 ~ 0,
  is.na(c17_w02) ~ -999,
  TRUE ~ 1)), exclude = -999)
elsoc_wide$idcoal_w03 <- factor(with(elsoc_wide, case_when(
  c17_w03 == 5 ~ 0,
  is.na(c17_w03) ~ -999,
  TRUE ~ 1)), exclude = -999)
elsoc_wide$idcoal_w04 <- factor(with(elsoc_wide, case_when(
  c17_w04 == 5 ~ 0,
  is.na(c17_w04) ~ -999,
  TRUE ~ 1)), exclude = -999)
elsoc_wide$idcoal_w05 <- factor(with(elsoc_wide, case_when(
  c17_w05 == 5 ~ 0,
  is.na(c17_w05) ~ -999,
  TRUE ~ 1)), exclude = -999)

elsoc_wide$id_sin <- factor(with(elsoc_wide, case_when(
  (idpart_w01 == 1 | idcoal_w01 == 1) & (idpart_w04 == 1 | idcoal_w04 == 1) ~ 1 ,
  (idpart_w01 == 0 & idcoal_w01 == 0) & (idpart_w04 == 1 | idcoal_w04 == 1) ~ 2 ,
  (idpart_w01 == 1 | idcoal_w01 == 1) & (idpart_w04 == 0 & idcoal_w04 == 0) ~ 3 ,
  (idpart_w01 == 0 & idcoal_w01 == 0) & (idpart_w04 == 0 & idcoal_w04 == 0) ~ 4 ,
  (idpart_w01 == NA | idcoal_w01 == NA) | (idpart_w04 == NA |idcoal_w04 == NA) ~ -999 ,
  TRUE ~ 2)),
  labels = c('Se mantiene identificándose',
             'Cambia a identificarse',
             'Cambia a No identificarse',
             'Se mantiene sin identificarse'))

elsoc_wide$interes_politica_w01 <- factor(with(elsoc_wide, 
                                               case_when(c13_w01 == 1 ~ 1,
                                                         c13_w01 == 2 | c13_w01 == 3 ~ 2,
                                                         c13_w01 == 4 | c13_w01 == 5 ~ 3)),
                                          labels = c('Nada interesado',
                                                     'Poco o algo interesado',
                                                     'Bastante o muy interesado'))

#-----------------------------------
#Poner la clase S3 sitcky para que no se pierdan los atributos con subsets
elsoc_long <- lapply(elsoc_long, sticky::sticky) %>% data.frame()

#Separar las muestras
elsoc_panel <- elsoc_long %>% filter(tipo_atricion == 1 | tipo_atricion == 17)
elsoc_panel_m1 <- dplyr::filter(elsoc_long, muestra == 1 & tipo_atricion == 1)
elsoc_panel_m2 <- dplyr::filter(elsoc_long, muestra == 2 & tipo_atricion == 17)

# Bases wide por submuestra
elsoc_wide_m1 <- elsoc_wide %>% dplyr::filter(tipo_atricion == 1| tipo_atricion == 17)

save(elsoc_long, elsoc_panel, elsoc_panel_m1, elsoc_panel_m2, elsoc_wide, elsoc_wide_m1,
     file = '1_input/datos_elsoc.RData')

# rm(list = ls())