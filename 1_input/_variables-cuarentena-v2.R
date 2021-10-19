library(tidyverse)
library(elsoc)
library(lubridate)
library(plm) 

load_elsoc()

## DATA CUARENTENA ---------
# Url para descarga directa de datos desde el Github del Ministerio de Ciencia
url <- "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output"

## Cuarentenas desde 2020/03 hasta 2020/07/28 
cuarentenas1_bruto <- read_csv(paste(url, "producto29", "Cuarentenas-Totales.csv", sep="/"))

cuarentenas1_acum <- cuarentenas1_bruto %>% 
  janitor::clean_names() %>% 
  mutate(fecha_de_inicio = as_date(fecha_de_inicio),
         fecha_de_termino = as_date(fecha_de_termino),
         # cortar en fecha 2020/07/27 
         fecha_de_termino = as_date(ifelse(fecha_de_termino > '2020/07/27', 
                                           make_date(2020, 07, 27),
                                           fecha_de_termino)),
         csum1 = as.numeric(fecha_de_termino - fecha_de_inicio, units = 'days')) %>% 
  filter(fecha_de_inicio < '2020/07/27') %>% 
  rename(comuna_cod = codigo_cut_comuna) %>%
  group_by(comuna_cod) %>% 
  summarise(csum1 = mean(csum1, na.rm = TRUE)) %>% 
  ungroup()

## Cuarentenas desde 2020/07/28 al último día actualizado
cuarentenas2_bruto <- read_csv(paste(url, 'producto74', "paso_a_paso_std.csv", sep="/"))

cuarentenas2 <- cuarentenas2_bruto %>% 
  janitor::clean_names() %>% 
  mutate(fecha = as_date(fecha),
         dia = weekdays(fecha),
         cuarentena = case_when(paso==1 ~ 1,
                                paso==2 & dia %in% c('Saturday', 'Sunday') ~ 1,
                                TRUE ~ 0)) %>% 
  filter(zona %in% c('Urbana', 'Total'))

dias_cuarentena <- function(fecha_termino, comuna_cod, ventana) {
  cond <- with(cuarentenas2, codigo_comuna == comuna_cod &
                 fecha >= fecha_termino - ventana &
                 fecha <= fecha_termino)
  dias_cuarentena <- sum(cuarentenas2$cuarentena[cond])
  return(dias_cuarentena)
}

# Cuarentena acomulada desde el 2020/07/27 a la fecha de entrevista:
cuarentenas2_acum <- cuarentenas2 %>% 
  group_by(codigo_comuna, zona) %>% 
  mutate(csum2 = cumsum(cuarentena),
         comuna_cod = as.numeric(codigo_comuna)) %>% 
  ungroup() %>% 
  select(fecha, comuna_cod, csum2)

# Unir fechas:
cuarentenas_acum <- full_join(cuarentenas1_acum,
                              cuarentenas2_acum,
                              by = 'comuna_cod') %>% 
  rowwise() %>% 
  mutate(csum = sum(csum1, csum2, na.rm = TRUE))

### Pegar fechas de cuarentenas a datos elsoc:
elsoc_long_covid <- elsoc_long_2016_2021 %>% 
  mutate(fecha = as_date(make_date(year = annio_entr, month = mes_entr, day = dia_entr)),
         cuarentenas30 = purrr::map2_dbl(.x = fecha, .y = comuna_cod, 30, .f = dias_cuarentena),
         cuarentenas60 = purrr::map2_dbl(.x = fecha, .y = comuna_cod, 60, .f = dias_cuarentena),
         cuarentenas90 = purrr::map2_dbl(.x = fecha, .y = comuna_cod, 90, .f = dias_cuarentena))

elsoc_covid <- elsoc_long_covid %>% 
  purrr::map_at(.at = vars(starts_with('s11_0')), 
                .f = function(s) car::recode(s, "1 = 0; 2 = 1; 3 = 2; c(4, 5) = 3; c(-888, -999) = NA")) %>%
  as.data.frame() %>%
  mutate(suma_dep = (s11_01 + s11_02 + s11_03 + s11_04 + s11_05 + s11_06 + s11_07 + s11_08 + s11_09),
         depr = factor(car::recode(suma_dep, "0:4 = 1; 5:9 = 2; 10:14 = 3; 15:27 = 4"), 
                       levels = c(1,2,3,4),
                       labels = c('Sin sintomas o Minima', 'Depresion Media', 'Depresion Moderada', 'Depresion Moderada-Severa\na Severa')),
         cuarentenas60t = factor(car::recode(cuarentenas60, "0 = 1; 1:30 = 2; 31:100 = 3"),
                                 levels = 1:3,
                                 labels = c('0 días', '1-30 días', '31 o más días')),
         trabajo_remoto = (m60 %in% 1:2)) %>% 
  group_by(idencuesta) %>% 
  mutate(cuarentenas60b = max(cuarentenas60),
         cuarentenas60tb = factor(car::recode(cuarentenas60b, "0 = 1; 1:30 = 2; 31:100 = 3"),
                                 levels = 1:3,
                                 labels = c('0 días', '1-30 días', '31 o más días'))) %>% 
  ungroup()
  
elsoc_covid %>% 
  stats(suma_dep, by = c(ola, cuarentenas60tb), na.rm = TRUE, vartype = 'ci', stat = 'mean') %>% 
  sjlabelled::as_label(ola, cuarentenas60tb) %>%
  ggplot(aes(y = stat, x = ola, 
             color = cuarentenas60tb, fill = cuarentenas60tb, group = cuarentenas60tb,
             ymin = stat_low, ymax = stat_upp)) + 
  geom_point() + 
  geom_line() + 
  # geom_ribbon(alpha = .2) + 
  scale_y_continuous(labels = scales::percent)

elsoc_covid %>% 
  prop(suma_dep >= 10, by = c(ola, cuarentenas60tb), na.rm = TRUE, vartype = 'ci') %>% 
  sjlabelled::as_label(ola, cuarentenas60tb) %>%
  ggplot(aes(y = prop, x = ola, 
             color = cuarentenas60tb, fill = cuarentenas60tb, group = cuarentenas60tb,
             ymin = prop_low, ymax = prop_upp)) + 
  geom_point() + 
  geom_line() + 
  # geom_ribbon(alpha = .2) + 
  scale_y_continuous(labels = scales::percent)

elsoc_covid_p <- elsoc_covid %>%
  pdata.frame(index = c("idencuesta", "ola"))

m1 <- plm(suma_dep ~ 1 + cuarentenas60 + ola, data = elsoc_covid_p, 
          model = 'within', weights = ponderador02)
m2 <- plm(suma_dep ~ 1 + cuarentenas60 + ola + trabajo_remoto, data = elsoc_covid_p, 
          model = 'within', weights = ponderador02)
m3 <- plm(suma_dep ~ 1 + cuarentenas60 + ola + trabajo_remoto + cuarentenas60*trabajo_remoto, data = elsoc_covid_p, 
          model = 'within', weights = ponderador02)

texreg::screenreg(l = list(m1, m2, m3))


