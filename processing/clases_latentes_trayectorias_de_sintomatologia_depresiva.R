### Clases latentes: trayectorias de sintomatología depresiva ###

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Cargar librerías
library(elsoc)
library(tidyverse)
library(poLCA)

# Cargar base de datos ELSOC
elsoc::load_elsoc('wide')

# Crear variables y filtrar observaciones relevantes
elsoc_depr_lca <- elsoc_wide_2016_2021 %>% 
  dplyr::filter(tipo_atricion == 1) %>% 
  purrr::map_at(.at = vars(starts_with('s11_0')), 
                .f = function(s) car::recode(s, "1 = 0; 2 = 1; 3 = 2; c(4, 5) = 3; c(-888, -999) = NA")) %>%
  as.data.frame() %>%
  mutate(
    phq9_w01 = (s11_01_w01 + s11_02_w01 + s11_03_w01 + s11_04_w01 + s11_05_w01 + s11_06_w01 + s11_07_w01 + s11_08_w01+ s11_09_w01),
    phq9_w02 = (s11_01_w02 + s11_02_w02 + s11_03_w02 + s11_04_w02 + s11_05_w02 + s11_06_w02 + s11_07_w02 + s11_08_w02+ s11_09_w02),
    phq9_w03 = (s11_01_w03 + s11_02_w03 + s11_03_w03 + s11_04_w03 + s11_05_w03 + s11_06_w03 + s11_07_w03 + s11_08_w03+ s11_09_w03),
    phq9_w04 = (s11_01_w04 + s11_02_w04 + s11_03_w04 + s11_04_w04 + s11_05_w04 + s11_06_w04 + s11_07_w04 + s11_08_w04+ s11_09_w04),
    phq9_w05 = (s11_01_w05 + s11_02_w05 + s11_03_w05 + s11_04_w05 + s11_05_w05 + s11_06_w05 + s11_07_w05 + s11_08_w05+ s11_09_w05),
    depr_w01 = car::recode(phq9_w01, "0:4 = 1; 5:9 = 2; 10:14 = 3; 15:27 = 4; else = NA"),
    depr_w02 = car::recode(phq9_w02, "0:4 = 1; 5:9 = 2; 10:14 = 3; 15:27 = 4; else = NA"),
    depr_w03 = car::recode(phq9_w03, "0:4 = 1; 5:9 = 2; 10:14 = 3; 15:27 = 4; else = NA"),
    depr_w04 = car::recode(phq9_w04, "0:4 = 1; 5:9 = 2; 10:14 = 3; 15:27 = 4; else = NA"),
    depr_w05 = car::recode(phq9_w05, "0:4 = 1; 5:9 = 2; 10:14 = 3; 15:27 = 4; else = NA")
  ) %>% 
  drop_na(starts_with('depr_'))

## Estimación de modelo de clases latentes, considerando 3-6 clases:


# Se fija semilla para obtener clases siempre en el mismo orden:
set.seed(1)

lca3 <- poLCA(cbind(depr_w01, depr_w02, depr_w03, depr_w04, depr_w05) ~ 1,
              data = elsoc_depr_lca , nclass = 3, na.rm = TRUE, maxiter = 5000, nrep = 10)
lca4 <- poLCA(cbind(depr_w01, depr_w02, depr_w03, depr_w04, depr_w05) ~ 1,
              data = elsoc_depr_lca , nclass = 4, na.rm = TRUE, maxiter = 5000, nrep = 10)
lca5 <- poLCA(cbind(depr_w01, depr_w02, depr_w03, depr_w04, depr_w05) ~ 1,
              data = elsoc_depr_lca , nclass = 5, na.rm = TRUE, maxiter = 5000, nrep = 10)
lca6 <- poLCA(cbind(depr_w01, depr_w02, depr_w03, depr_w04, depr_w05) ~ 1,
              data = elsoc_depr_lca , nclass = 6, na.rm = TRUE, maxiter = 5000, nrep = 10)

elsoc_depr_lca$class3 <- as.factor(lca3$predclass)
elsoc_depr_lca$class4 <- as.factor(lca4$predclass)
elsoc_depr_lca$class5 <- as.factor(lca5 $predclass)
elsoc_depr_lca$class6 <- as.factor(lca6 $predclass)

## Resultados

# Agregar etiquetas (3 clases)
elsoc_depr_lca$class_depr3 <- factor(elsoc_depr_lca$class3)

elsoc_depr_lca %>%
  survey_design_elsoc(weights = 'ponderador02_w05') %>% 
  prop_list(depr_w01, depr_w02, depr_w03, depr_w04, depr_w05, 
            by = class_depr3, na.rm = TRUE) %>%
  mutate(name = factor(name,
                       levels = glue::glue('depr_w0{1:5}'),
                       labels = c(2016:2019, 2021)),
         sintomas = factor(value,
                           levels = 1:4,
                           labels = c('Sin sintomas o síntomas\nde depresión mínima', 
                                      'Síntomas de\ndepresion media', 
                                      'Síntomas de\ndepresion moderada', 
                                      'Síntomas de depresion\nmoderada-severa o severa')),
         class_depr3 = factor(class_depr3,
                              labels = glue::glue("{get_labels(elsoc_depr_lca$class_depr3)}\n ({scales::percent(elsoc_depr_lca %>% survey_design_elsoc(weights = 'ponderador02_w05') %>%
                                    prop(class_depr3, na.rm = TRUE) %>%
                                    pull(prop), .1)})"))
  ) %>% 
  ggplot(aes(x = prop, y = fct_rev(name), fill = fct_rev(sintomas))) + 
  geom_col(position = 'stack') + 
  facet_grid(~ class_depr3) +
  labs(y = "Ola del estudio", 
       x = "Proporción",
       fill = "") +
  scale_x_continuous(breaks = seq(0, 1, .25),
                     labels = scales::percent) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_bw() + 
  theme(legend.position = "top", 
        plot.title = element_text(size = 14),
        axis.text.x = element_text(size = 8)) + 
  scale_fill_viridis_d(end = .85)



# Agregar etiquetas (4 clases)
elsoc_depr_lca$class_depr4 <- factor(elsoc_depr_lca$class4)

elsoc_depr_lca %>%
  survey_design_elsoc(weights = 'ponderador02_w05') %>% 
  prop_list(depr_w01, depr_w02, depr_w03, depr_w04, depr_w05, 
            by = class_depr4, na.rm = TRUE) %>%
  mutate(name = factor(name,
                       levels = glue::glue('depr_w0{1:5}'),
                       labels = c(2016:2019, 2021)),
         sintomas = factor(value,
                           levels = 1:4,
                           labels = c('Sin sintomas o síntomas\nde depresión mínima', 
                                      'Síntomas de\ndepresion media', 
                                      'Síntomas de\ndepresion moderada', 
                                      'Síntomas de depresion\nmoderada-severa o severa')),
         class_depr4 = factor(class_depr4,
                             labels = glue::glue("{get_labels(elsoc_depr_lca$class_depr4)}\n ({scales::percent(elsoc_depr_lca %>% survey_design_elsoc(weights = 'ponderador02_w05') %>%
                                    prop(class_depr4, na.rm = TRUE) %>%
                                    pull(prop), .1)})"))) %>% 
  ggplot(aes(x = prop, y = fct_rev(name), fill = fct_rev(sintomas))) + 
  geom_col(position = 'stack') + 
  facet_grid(~ class_depr4) +
  labs(y = "Ola del estudio", 
       x = "Proporción",
       fill = "") +
  scale_x_continuous(breaks = seq(0, 1, .25),
                     labels = scales::percent) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_bw() + 
  theme(legend.position = "top", 
        plot.title = element_text(size = 14),
        axis.text.x = element_text(size = 8)) + 
  scale_fill_viridis_d(end = .85)


# Agregar etiquetas (5 clases)
elsoc_depr_lca$class_depr5 <- factor(elsoc_depr_lca$class5)

elsoc_depr_lca %>%
  survey_design_elsoc(weights = 'ponderador02_w05') %>% 
  prop_list(depr_w01, depr_w02, depr_w03, depr_w04, depr_w05, 
            by = class_depr5, na.rm = TRUE) %>%
  mutate(name = factor(name,
                       levels = glue::glue('depr_w0{1:5}'),
                       labels = c(2016:2019, 2021)),
         sintomas = factor(value,
                           levels = 1:4,
                           labels = c('Sin sintomas o síntomas\nde depresión mínima', 
                                      'Síntomas de\ndepresion media', 
                                      'Síntomas de\ndepresion moderada', 
                                      'Síntomas de depresion\nmoderada-severa o severa')),
         class_depr5 = factor(class_depr5,
                              labels = glue::glue("{get_labels(elsoc_depr_lca$class_depr5)}\n ({scales::percent(elsoc_depr_lca %>% survey_design_elsoc(weights = 'ponderador02_w05') %>%
                                    prop(class_depr5, na.rm = TRUE) %>%
                                    pull(prop), .1)})"))
  ) %>% 
  ggplot(aes(x = prop, y = fct_rev(name), fill = fct_rev(sintomas))) + 
  geom_col(position = 'stack') + 
  facet_grid(~ class_depr5) +
  labs(y = "Ola del estudio", 
       x = "Proporción",
       fill = "") +
  scale_x_continuous(breaks = seq(0, 1, .25),
                     labels = scales::percent) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_bw() + 
  theme(legend.position = "top", 
        plot.title = element_text(size = 14),
        axis.text.x = element_text(size = 8)) + 
  scale_fill_viridis_d(end = .85)



# Agregar etiquetas (6 clases)
elsoc_depr_lca$class_depr6 <- factor(elsoc_depr_lca$class6)

elsoc_depr_lca %>%
  survey_design_elsoc(weights = 'ponderador02_w05') %>% 
  prop_list(depr_w01, depr_w02, depr_w03, depr_w04, depr_w05, 
            by = class_depr6, na.rm = TRUE) %>%
  mutate(name = factor(name,
                       levels = glue::glue('depr_w0{1:5}'),
                       labels = c(2016:2019, 2021)),
         sintomas = factor(value,
                           levels = 1:4,
                           labels = c('Sin sintomas o síntomas\nde depresión mínima', 
                                      'Síntomas de\ndepresion media', 
                                      'Síntomas de\ndepresion moderada', 
                                      'Síntomas de depresion\nmoderada-severa o severa')),
         class_depr6 = factor(class_depr6,
                             labels = glue::glue("{get_labels(elsoc_depr_lca$class_depr6)}\n ({scales::percent(elsoc_depr_lca %>% survey_design_elsoc(weights = 'ponderador02_w05') %>%
                                    prop(class_depr6, na.rm = TRUE) %>%
                                    pull(prop), .1)})"))
  ) %>% 
  ggplot(aes(x = prop, y = fct_rev(name), fill = fct_rev(sintomas))) + 
  geom_col(position = 'stack') + 
  facet_grid(~ class_depr6) +
  labs(y = "Ola del estudio", 
       x = "Proporción",
       fill = "") +
  scale_x_continuous(breaks = seq(0, 1, .25),
                     labels = scales::percent) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_bw() + 
  theme(legend.position = "top", 
        plot.title = element_text(size = 14),
        axis.text.x = element_text(size = 8)) + 
  scale_fill_viridis_d(end = .85)


# Analizar indicadores de distintos modelos de clase latente
indicadores_lca <- function(x) {
  purrr::map_df(.x = x, .f = function(k) {
    data.frame(Nclases = length(k$P), 
               AIC = k$aic, 
               BIC = k$bic, 
               Nobs = k$Nobs, 
               Chisq = k$Chisq)
  })
}

indicadores_lca(list(lca3, lca4, lca5, lca6))


# Agregar etiquetas (4 clases)
elsoc_depr_lca$class_depr <- factor(elsoc_depr_lca$class4,
                                    levels = c('1', '3', '4', '2'),
                                    labels = c('Carga baja de\nsíntomas de depresión',
                                               'Carga media-baja de\nsíntomas de depresión',
                                               'Carga media-alta de\nsíntomas de depresión',
                                               'Carga alta de\nsíntomas de depresión'))

# Guardar datos:
save(elsoc_depr_lca, 
     file = file.path('..', 'elsoc_depr_lca.RData'))
