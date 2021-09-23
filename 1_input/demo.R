devtools::install_github('edgardo-cerda/elsoc', force = TRUE)
library(elsoc)
library(tidyverse)
library(sjlabelled)

load(file.path('1_input', 'ELSOC_Long_2016_2021_v1.00_R.RData'))
load(file.path('1_input', 'ELSOC_Wide_2016_2021_v2.00_R.RData'))

elsoc_wide_2016_2021 %>%
  filter(tipo_atricion == 1) %>% 
  survey_design_elsoc(weights = 'ponderador02_w05') %>% 
  prop(c01_w05)

### Ejemplo 1 ###
datos.1.1 <- elsoc_long_2016_2021 %>%
  filter(tipo_atricion == 1 & ola %in% c(4,5) & !c20 %in% c(-888, -999)) %>%
  prop(x = c20, by = ola, na.rm = TRUE) %>% 
  sjlabelled::as_label(c20, ola)

datos.1.1 %>% 
  ggplot(aes(y = prop, x = c20, fill = ola, 
             label = as.character(scales::percent(prop, accuracy = .1)))) + 
  theme_bw() + 
  geom_col(position = 'dodge2') +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0,0.3)) +
  ylab(label = NULL) +
  xlab(label = NULL) +
  scale_fill_viridis_d(begin = 0, end = .85, direction = -1, option = 'viridis') +
  coord_flip() +
  theme(axis.text.x = element_text(size=rel(.9))) +
  scale_x_discrete(labels = waiver( )) + 
  guides(fill = guide_legend(reverse = TRUE))


### Ejemplo 2 ###
datos.2.1 <- elsoc_long_2016_2021 %>%
  filter(tipo_atricion == 1 & !c15 %in% c(-888, -999)) %>%
  mutate(pos_id = car::recode(c15, recodes = "c(0,1,2,3,4)=1; c(5)=2; c(6,7,8,9,10)=3; c(11,12, -999, -888)=4")) %>% 
  prop(pos_id, ola, na.rm = TRUE, vartype = c('ci', 'se')) %>% 
  sjlabelled::as_label(ola) %>%
  mutate(pos_id = factor(pos_id, levels = c(2, 1, 3, 4),
                         labels = c('Centro', "Izquierda", "Derecha", "No se\nidentifica")))

datos.2.1 %>% 
  ggplot(aes(y = prop, x = pos_id, fill = ola, 
             ymin = prop_low, ymax = prop_upp,
             label = as.character(scales::percent(prop, accuracy = .1)))) +
  theme_bw() + 
  geom_col(position= 'dodge2') +
  geom_errorbar(position = position_dodge(.9), width = .5) + 
  #escala del eje y en porcentajes del 0 al 100%
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  #Nombres de los ejes se eliminan
  ylab(label = NULL) +
  xlab(label = NULL) +
  #colores oficiales por ola: degradé 'viridis'
  scale_fill_viridis_d(begin = 0, end = .85, direction = -1, option = 'viridis') +
  #etiquetas por sobre cada barra
  geom_text(vjust = -0.8,
            position = position_dodge(width = .9),
            size= 2.75) +
  #posicionamiento de leyenda arriba
  theme(legend.position = 'top',
        legend.title = element_blank())


### Ejemplo 3 ###
datos <- elsoc_long_2016_2021 %>% 
  subset(tipo_atricion == 1) %>% 
  select(starts_with('c46_'), m0_sexo, segmento_disenno, estrato_disenno, ponderador02) %>% 
  pivot_longer(cols = starts_with('c46_')) %>% 
  filter(!is.na(value)) %>% 
  prop(value %in% c(4,5), by = c(name, m0_sexo))

datos %>% 
  ggplot(aes(y= prop, x = name, fill = factor(m0_sexo))) + 
  geom_col(position = 'dodge2')

### Ejemplo 4 ###
elsoc_long_2016_2021 %>% 
  filter(!c01 %in% c(-888, -999)) %>% 
  prop(c01, by  = c(ola, m0_sexo), na.rm = TRUE)

elsoc_long_2016_2021 %>% 
  filter(!c01 %in% c(-888, -999)) %>% 
  prop(c01 %in% c(4,5), by  = c(ola, m0_sexo), na.rm = TRUE) %>% 
  sjlabelled::as_label(m0_sexo, ola) %>% 
  ggplot(aes(y = prop, x = ola, fill = m0_sexo)) + 
  geom_col(position = 'dodge2')

### Ejemplo 5 ###
elsoc_long_2016_2021 %>% 
  filter(!d01_01 %in% c(-888, -999)) %>% 
  elsoc::stat(d01_01, by = ola)
