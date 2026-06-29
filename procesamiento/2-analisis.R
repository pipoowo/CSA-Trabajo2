pacman::p_load(lme4,reghelper,haven,stargazer,ggplot2,dplyr, patchwork,
               texreg,ggeffects,sjmisc,statar,summarytools,psych,sjPlot,
               plm,lmtest,foreign, gtsummary, labelled, dplyr, gt)

datos_cep95_proc <- readRDS("input/proc/datos_cep95_proc.rds")

datos_cep95_proc <- datos_cep95_proc %>%
  mutate(
    preferencia_autoritarismo = relevel(as.factor(preferencia_autoritarismo), ref = "Nula"),
    corrupcion_serpublico = relevel(as.factor(corrupcion_serpublico), ref = "Poco o nadie"),
    cambio_estado = relevel(as.factor(cambio_estado), ref = "No"))

var_label(datos_cep95_proc) <- list(
  corrupcion_serpublico = "Percepción de corrupción pública",
  cambio_estado = "Reticencia frente al cambio en el Estado",
  preferencia_autoritarismo = "Preferencia por el autoritarismo",
  preferencia_esfuerzo = "Creencia en la meritocracia",
  polarizacion_pol = "Nivel de polarización política"
)

#Análisis descriptivos
frq(datos_cep95_proc$voto_kast)
frq(datos_cep95_proc$preferencia_autoritarismo)
summary(datos_cep95_proc$preferencia_esfuerzo)
frq(datos_cep95_proc$corrupcion_serpublico)
summary(datos_cep95_proc$polarizacion_pol)
frq(datos_cep95_proc$polarizacion_pol)


#M1, M2, M3
log1 <- glm(voto_kast ~ preferencia_esfuerzo + cambio_estado,
            data = datos_cep95_proc, family = "binomial") 

log2 <- glm(voto_kast ~ preferencia_esfuerzo + cambio_estado + corrupcion_serpublico,
            data = datos_cep95_proc, family = "binomial") 

log3 <- glm(voto_kast ~ preferencia_autoritarismo + preferencia_esfuerzo + corrupcion_serpublico + polarizacion_pol + cambio_estado,
            data = datos_cep95_proc, family = "binomial")

glance_custom <- function(x) {
  predicciones <- predict(x, type = "response")
  valores_reales <- x$y
  r2_tjur <- mean(predicciones[valores_reales == 1]) - mean(predicciones[valores_reales == 0])
  tibble::tibble(nobs = stats::nobs(x), pseudo_r2 = r2_tjur)
}

#Tabla2

t1 <- tbl_regression(log1, exponentiate = TRUE, intercept = TRUE) %>% 
  add_significance_stars(hide_p = TRUE, hide_se = TRUE) %>%
  add_glance_table(glance_fun = glance_custom, include = c("nobs", "pseudo_r2"), label = list(nobs ~ "N", pseudo_r2 ~ "Pseudo R²"))

t2 <- tbl_regression(log2, exponentiate = TRUE, intercept = TRUE) %>% 
  add_significance_stars(hide_p = TRUE, hide_se = TRUE) %>%
  add_glance_table(glance_fun = glance_custom, include = c("nobs", "pseudo_r2"), label = list(nobs ~ "N", pseudo_r2 ~ "Pseudo R²"))

t3 <- tbl_regression(log3, exponentiate = TRUE, intercept = TRUE) %>%  
  add_significance_stars(hide_p = TRUE, hide_se = TRUE) %>%
  add_glance_table(glance_fun = glance_custom, include = c("nobs", "pseudo_r2"), label = list(nobs ~ "N", pseudo_r2 ~ "Pseudo R²"))

tabla2 <- tbl_merge(
  tbls = list(t1, t2, t3),
  tab_spanner = c("**M1**", "**M2**", "**M3**"),
  quiet = TRUE
) %>%
  modify_header(label = "**Variables**") %>%
  modify_table_body(~ .x %>% arrange(row_type == "glance_statistic"))

tabla2

tabla2_gt <- as_gt(tabla2)

gtsave(tabla2_gt, filename = "output/tabla_modelo.png")