pacman::p_load(dplyr,       # Manipulacion de datos 
               haven,       # importar datos en .dta o .sav
               car,         # recodificar variables
               sjlabelled,  # etiquetado de variables
               sjmisc,      # descriptivos y frecuencias
               sjPlot,      # tablas, plots y descriptivos
               summarytools, # resumen de dataframe
               readxl        # base de datos xlsx
               )

options(scipen = 999)
rm(list = ls())

cep95 <- readRDS("input/cep95.rds")

datos_cep95 <- select(cep95, 
              voto_kast = elec_pres_urna_33, #
              preferencia_democracia = democracia_21,
              preferencia_acuerdos = pp_congreso_61_b,
              preferencia_esfuerzo = pobreza_62,
              polarización_derecha = polarizacion_1_a, #
              polarización_izquierda = polarizacion_1_b, #
              cambio_estado = democracia_12,
              corrupcion_serpublico = ciudadania_16) #

##--- 3.1 Voto Kast ----
datos_cep95 <- datos_cep95 %>%
  mutate(voto_kast = case_when(
    voto_kast == 1 ~ 0,
    voto_kast == 2 ~ 1,
    voto_kast > 2 ~ NA
  ))
##--- 3.2 Preferencia autoritarismo ----

datos_cep95 <- datos_cep95 %>% 
  mutate(preferencia_autoritarismo = case_when(
    preferencia_democracia == 1 & preferencia_acuerdos == 1 ~ 0, #nula ~ 0, #nula
    preferencia_democracia == 1 & preferencia_acuerdos == 2 ~ 1, #baja
    preferencia_democracia == 2 & preferencia_acuerdos == 2 ~ 2, #media = 
    preferencia_democracia == 3 & preferencia_acuerdos == 2 ~ 3, 
    TRUE ~ NA
  ))

##--- 3.3 Meritocracia ----

datos_cep95 <- datos_cep95 %>% 
  mutate(preferencia_esfuerzo = case_when(
    preferencia_esfuerzo %in% c(-8, -9) ~ NA,
    TRUE ~ preferencia_esfuerzo
  ))

##--- 3.4 Desconfianza en serv. públicos ----

datos_cep95 <- mutate(datos_cep95,
                corrupcion_serpublico = corrupcion_serpublico - 1)

datos_cep95 <- datos_cep95 %>% 
  mutate(corrupcion_serpublico = case_when(
    corrupcion_serpublico %in% c(-8, -9) ~ NA,
    TRUE ~ corrupcion_serpublico
  ))

##--- 3.5 Intolerancia política ----
datos_cep95 <- mutate(datos_cep95,
                polarizacion_pol = abs(polarización_derecha - polarización_izquierda))

datos_cep95 <- datos_cep95 %>% 
  mutate(polarizacion_pol = case_when(
    polarizacion_pol %in% c(-8, -9) ~ NA,
    TRUE ~ polarizacion_pol
  ))

##--- 3.6 Reticencia al cambio ----
datos_cep95 <- datos_cep95 %>% 
  mutate(cambio_estado = case_when(
    cambio_estado %in% c(-8, -9) ~ NA,
    TRUE ~ cambio_estado
  ))

datos_cep95 <- mutate(datos_cep95,
                cambio_estado = 3 - cambio_estado)

#Base limpia
datos_cep95_proc <- na.omit(datos_cep95)

#--- 6. Etiquetar variables ----
datos_cep95_proc$voto_kast <- set_labels(datos_cep95_proc$voto_kast,labels = c("Si" = 1,
                                           "No" = 0
                                           ))

datos_cep95_proc <- datos_cep95_proc %>% 
  mutate(preferencia_autoritarismo = case_when(
    preferencia_autoritarismo == 0  ~ "Nula", 
    preferencia_autoritarismo == 1  ~ "Baja",
    preferencia_autoritarismo == 2  ~ "Media",
    preferencia_autoritarismo == 3  ~ "Alta"))

datos_cep95_proc <- datos_cep95_proc %>% 
  mutate(corrupcion_serpublico = case_when(
    corrupcion_serpublico == 0  ~ "Nadie", 
    corrupcion_serpublico == 1  ~ "Poco",
    corrupcion_serpublico == 2  ~ "Moderado",
    corrupcion_serpublico == 3  ~ "Mucha gente",
    corrupcion_serpublico == 4  ~ "Casi todo"))

datos_cep95_proc <- datos_cep95_proc %>% 
  mutate(cambio_estado = case_when(
    cambio_estado == 0  ~ "Reformas importantes", 
    cambio_estado == 1  ~ "Reformas menores",
    cambio_estado == 2  ~ "Ninguna reforma"))

datos_cep95_proc <- select(datos_cep95_proc, 
                voto_kast,
                preferencia_autoritarismo,
                preferencia_esfuerzo, 
                corrupcion_serpublico, 
                polarizacion_pol, 
                cambio_estado)

