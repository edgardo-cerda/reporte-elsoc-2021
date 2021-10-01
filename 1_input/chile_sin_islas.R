library(chilemapas)
library(tidyverse)
library(sf)

#################################################################
########### Mapas de Chile para analisis geográfico #############
#################################################################

### Mapa de chile en tsmap a partir de 
mapa_chile_comunas <- chilemapas::mapa_comunas %>% 
  dplyr::filter(!codigo_comuna %in% c('05201', '05104')) %>% # Sacar Rapa Nui e Isla Juan Fernández
  st_as_sf(.)

# Borrar islas de la comuna de valparaiso
mapa_chile_comunas[[4]][[76]][[4]] <- NULL
mapa_chile_comunas[[4]][[76]][[3]] <- NULL
mapa_chile_comunas[[4]][[76]][[2]] <- NULL

### Gráfico de santiago

mapa_stgo <- chilemapas::mapa_zonas %>%   
  dplyr::filter(codigo_region == '13' & codigo_provincia == '131') %>% 
  st_as_sf(.) %>% 
  group_by(codigo_comuna) %>% 
  summarise() 

ggplot(mapa_stgo) + 
  geom_sf()
