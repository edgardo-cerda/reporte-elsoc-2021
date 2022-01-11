library(tidyverse)
library(elsoc)
library(lubridate)

load_elsoc()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## DATA CUARENTENA ---------
# Url para descarga directa de datos desde el Github del Ministerio de Ciencia
url <- "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output"

## Cuarentenas desde 2020/03 hasta 2020/07/28 
cuarentenas1_bruto <- readr::read_csv(paste(url, "producto29", "Cuarentenas-Totales.csv", sep = "/"))

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
         cuarentena = case_when(paso == 1 ~ 1,
                                paso == 2 & dia %in% c('Saturday', 'Sunday') ~ 1,
                                TRUE ~ 0)) %>% 
  filter(zona %in% c('Urbana', 'Total'))

# Cuarentena acumulada desde el 2020/07/27 a la fecha respectiva:
cuarentenas2_acum <- cuarentenas2 %>% 
  group_by(codigo_comuna, zona) %>% 
  mutate(csum2 = cumsum(cuarentena),
         comuna_cod = as.numeric(codigo_comuna)) %>% 
  ungroup() %>% 
  select(fecha, comuna_cod, csum2)

# Cuarentenas acumuladas por comuna y fecha desde inicio de las cuarentenas:
cuarentenas_acum <- full_join(cuarentenas1_acum,
                              cuarentenas2_acum,
                              by = 'comuna_cod') %>% 
  rowwise() %>% 
  mutate(cuarentena_acum = sum(csum1, csum2, na.rm = TRUE)) %>% 
  select(comuna_cod, fecha, cuarentena_acum) %>%
  # Cuarentenas acumuladas en ventanas de tiempo
  arrange(comuna_cod, fecha) %>% 
  group_by(comuna_cod) %>% 
  mutate(cuarentena_acum.14 = cuarentena_acum - dplyr::lag(cuarentena_acum, n = 14),
         cuarentena_acum.30 = cuarentena_acum - dplyr::lag(cuarentena_acum, n = 30),
         cuarentena_acum.60 = cuarentena_acum - dplyr::lag(cuarentena_acum, n = 60),
         cuarentena_acum.90 = cuarentena_acum - dplyr::lag(cuarentena_acum, n = 90)
  )

### Pegar fechas de cuarentenas en distintas ventanas de tiempo a datos elsoc:
elsoc_long_cuarentenas <- elsoc_long_2016_2021 %>% 
  filter(ola == 5) %>% 
  mutate(fecha = make_date(year = annio_entr, month = mes_entr, day = dia_entr)) %>% 
  left_join(cuarentenas_acum, by = c('comuna_cod', 'fecha')) %>% 
  select(idencuesta, ola, starts_with('cuarentena_acum'))

save(elsoc_long_cuarentenas, 
     file = file.path('..', 'cuarentenas_acum.RData'))
