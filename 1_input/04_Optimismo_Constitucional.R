library(panelr)
library(elsoc)
library(ggplot2)
library(tidyr)
library(dplyr)

rm(list=ls())
#cargar base de datos
load("C:/Users/Fondecyt R. Sociales/Dropbox/ELSOC/2_Bases_de_Datos/10_Combinacion_de_Olas_ELSOC/0E_Bases_de_Datos_Resultantes_2016_2021/ELSOC_Long_2016_2021_v1.00_R.RData")

frq(elsoc_long_2016_2021$c46r_01)
frq(elsoc_long_2016_2021$c46_02)
frq(elsoc_long_2016_2021$c46_03)
frq(elsoc_long_2016_2021$c46_04)


# Recodificación de variables ---------------------------------------------
#posición política
elsoc_long_2016_2021$pos_id  <- factor(car::recode(elsoc_long_2016_2021$c15,"c(11,12, -999,-888)='No se identifica';c(0,1,2,3,4)='Izquierda';  
                                      c(5)='Centro';c(6,7,8,9,10)='Derecha'"),
                                       levels = c('Izquierda', 'Centro', 'Derecha', 'No se identifica'))
#Educación
elsoc_long_2016_2021$educ <- car::recode(elsoc_long_2016_2021$m01,"c(1,2,3)=1;c(4,5)=2;c(6,7)=3;c(8,9,10)=4;c(-999,-888)=NA")
elsoc_long_2016_2021$educ <- factor(elsoc_long_2016_2021$educ,labels = c("Basica","Media","Tecnica","Universitaria"))
#Optimismo Constitucional
elsoc_long_2016_2021$c46r_01<-car::recode(elsoc_long_2016_2021$c46_01,"c(-999,-888)=NA")
elsoc_long_2016_2021$c46r_02<-car::recode(elsoc_long_2016_2021$c46_02,"c(-999,-888)=NA")
elsoc_long_2016_2021$c46r_03<-car::recode(elsoc_long_2016_2021$c46_03,"c(-999,-888)=NA")
elsoc_long_2016_2021$c46r_04<-car::recode(elsoc_long_2016_2021$c46_04,"c(-999,-888)=NA")

#interes en política
elsoc_long_2016_2021$interes_pol<- car::recode(elsoc_long_2016_2021$c13, "c(-999,-888)=NA;1='Nada interesado';2='Poco interesado';
                                                      3='Algo interesado';4='Bastante Interesado';5='Muy Interesado'",as.factor=T)
elsoc_long_2016_2021$interes_pol<-factor(elsoc_long_2016_2021$interes_pol,
                                         labels = c('Nada \ninteresado','Poco \ninteresado','Algo \ninteresado',
                                                    'Bastante \ninteresado','Muy \ninteresado'))

#justificación de la violencia
elsoc_long_2016_2021$justifica_viol1<- car::recode(elsoc_long_2016_2021$f05_06, "c(-999,-888)=NA;1='Nunca';2='Pocas veces';
                                                      3='Algunas veces';4='Muchas veces';5='Siempre'",as.factor=T)
elsoc_long_2016_2021$justifica_viol1<-factor(elsoc_long_2016_2021$justifica_viol1,
                                         labels = c('Nunca','Pocas veces','Algunas veces',
                                                    'Muchas veces','Siempre'))
elsoc_long_2016_2021$justifica_viol2<- car::recode(elsoc_long_2016_2021$f05_07, "c(-999,-888)=NA;1='Nunca';2='Pocas veces';
                                                      3='Algunas veces';4='Muchas veces';5='Siempre'",as.factor=T)
elsoc_long_2016_2021$justifica_viol2<-factor(elsoc_long_2016_2021$justifica_viol2,
                                             labels = c('Nunca','Pocas veces','Algunas veces',
                                                        'Muchas veces','Siempre'))

# Grafico 1: Estadística Descriptiva --------------------------------------



elsoc_long_2016_2021 %>% 
  subset(tipo_caso != 2 & tipo_atricion == 1) %>% 
  select(starts_with('c46_'), segmento_disenno, estrato_disenno, ponderador02) %>% 
  pivot_longer(cols = starts_with('c46_')) %>% 
  subset(!is.na(value)) %>% 
  prop(value %in% c(4,5), by = name) %>% 
  mutate(name=factor(name,
                     levels = c('c46_01','c46_02','c46_03','c46_04'),
                     labels = c('Reducirá desigualdades\n en salud y educación','Empeorará condiciones\n económicas',
                                'Reducirá la Corrupción','Poco impacto en la\n Calidad de vida'))) %>% 
ggplot(aes(x = name, y = prop,
           label = as.character(scales::percent(ifelse(prop>.01, prop, NA), accuracy = .1)))) +
  theme_bw() +
  geom_col()+
  theme_bw()+
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  ylab(label = NULL) +
  xlab(label = NULL) +
  scale_fill_viridis_d(begin = 0, end = .85, direction = -1, option = 'viridis')+
  ggtitle('Porcentaje "De acuerdo" + "Totalmente de acuerdo", Optimismo Constitucional')
  



# Grafico 2: Optimismo Constitucional por educación -----------------------


elsoc_long_2016_2021 %>% 
  subset(tipo_caso != 2 & tipo_atricion == 1) %>% 
  select(starts_with('c46r_'),educ, segmento_disenno, estrato_disenno, ponderador02) %>% 
  pivot_longer(cols = starts_with('c46r_')) %>% 
  subset(!is.na(value)) %>%
  prop(value %in% c(4,5), by = c(educ,name)) %>% 
  mutate(name=factor(name,
                     levels = c('c46r_01','c46r_02','c46r_03','c46r_04'),
                     labels = c('Reducirá desigualdades\n en salud y educación','Empeorará condiciones\n económicas',
                                'Reducirá la Corrupción','Poco impacto en la\n Calidad de vida'))) %>% 
  ggplot(aes(x = educ, y = prop,
             label = as.character(scales::percent(ifelse(prop>.01, prop, NA), accuracy = .1)))) +
  geom_col(position = 'stack')+
  facet_grid(.~name)+
theme_bw()+
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  ylab(label = NULL) +
  xlab(label = NULL) +
  scale_fill_viridis_d(begin = 0, end = .85, direction = -1, option = 'viridis')+
  ggtitle('Porcentaje "De acuerdo" + "Totalmente de acuerdo" por nivel educacional')
  

# Grafico 3: Optimismo Constitucional por posición política ---------------

elsoc_long_2016_2021 %>% 
  subset(tipo_caso != 2 & tipo_atricion == 1) %>% 
  select(starts_with('c46r_'),pos_id, segmento_disenno, estrato_disenno, ponderador02) %>% 
  pivot_longer(cols = starts_with('c46r_')) %>% 
  subset(!is.na(value)) %>%
  prop(value %in% c(4,5), by = c(pos_id,name)) %>% 
  mutate(name=factor(name,
                     levels = c('c46r_01','c46r_02','c46r_03','c46r_04'),
                     labels = c('Reducirá desigualdades\n en salud y educación','Empeorará condiciones\n económicas',
                                'Reducirá la Corrupción','Poco impacto en la\n Calidad de vida'))) %>% 
  ggplot(aes(x = pos_id, y = prop,
             label = as.character(scales::percent(ifelse(prop>.01, prop, NA), accuracy = .1)))) +
  geom_col(position = 'stack')+
  facet_grid(.~name)+
  theme_bw()+
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  ylab(label = NULL) +
  xlab(label = NULL) +
  scale_fill_viridis_d(begin = 0, end = .85, direction = -1, option = 'viridis')+
  ggtitle('Porcentaje "De acuerdo" + "Totalmente de acuerdo" según posición política')



# Grafico 4: Optimismo Constitucional por interés en política -------------

elsoc_long_2016_2021 %>% 
  subset(tipo_caso != 2 & tipo_atricion == 1) %>% 
  select(starts_with('c46r_'),interes_pol, segmento_disenno, estrato_disenno, ponderador02) %>% 
  pivot_longer(cols = starts_with('c46r_')) %>% 
  subset(!is.na(value)) %>%
  prop(value %in% c(4,5), by = c(interes_pol,name)) %>% 
  mutate(name=factor(name,
                     levels = c('c46r_01','c46r_02','c46r_03','c46r_04'),
                     labels = c('Reducirá desigualdades\n en salud y educación','Empeorará condiciones\n económicas',
                                'Reducirá la Corrupción','Poco impacto en la\n Calidad de vida'))) %>% 
  ggplot(aes(x = interes_pol, y = prop,
             label = as.character(scales::percent(ifelse(prop>.01, prop, NA), accuracy = .1)))) +
  geom_col(position = 'stack')+
  facet_grid(.~name)



# Grafico 5: Justificación de la violencia para el cambio social Trabajadores bloquee las calles --------

elsoc_long_2016_2021 %>% 
  subset(tipo_caso != 2 & tipo_atricion == 1) %>% 
  select(starts_with('c46r_'),justifica_viol1, segmento_disenno, estrato_disenno, ponderador02) %>% 
  pivot_longer(cols = starts_with('c46r_')) %>% 
  subset(!is.na(value)) %>%
  prop(value %in% c(4,5), by = c(justifica_viol1,name)) %>%
  subset(!is.na(justifica_viol1)) %>% #SE GENERAN NA ACÁ,REVISAR
  mutate(name=factor(name,
                     levels = c('c46r_01','c46r_02','c46r_03','c46r_04'),
                     labels = c('Reducirá desigualdades\n en salud y educación','Empeorará condiciones\n económicas',
                                'Reducirá la Corrupción','Poco impacto en la\n Calidad de vida'))) %>% 
  ggplot(aes(x = justifica_viol1, y = prop,
             label = as.character(scales::percent(ifelse(prop>.01, prop, NA), accuracy = .1)))) +
  geom_col(position = 'stack')+
  facet_grid(.~name)


# Grafico 6: Justificación de la violencia para el cambio social Estudiantes tiren piedras a carabineross --------

elsoc_long_2016_2021 %>% 
  subset(tipo_caso != 2 & tipo_atricion == 1) %>% 
  select(starts_with('c46r_'),justifica_viol2, segmento_disenno, estrato_disenno, ponderador02) %>% 
  pivot_longer(cols = starts_with('c46r_')) %>% 
  subset(!is.na(value)) %>%
  prop(value %in% c(4,5), by = c(justifica_viol2,name)) %>% 
  subset(!is.na(justifica_viol2)) %>% #SE GENERAN NA ACÁ,REVISAR
  mutate(name=factor(name,
                     levels = c('c46r_01','c46r_02','c46r_03','c46r_04'),
                     labels = c('Reducirá desigualdades\n en salud y educación','Empeorará condiciones\n económicas',
                                'Reducirá la Corrupción','Poco impacto en la\n Calidad de vida'))) %>% 
  ggplot(aes(x = justifica_viol2, y = prop,
             label = as.character(scales::percent(ifelse(prop>.01, prop, NA), accuracy = .1)))) +
  geom_col(position = 'stack')+
  facet_grid(.~name)



