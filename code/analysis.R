# ---
# Data Analysis Script
# Author: Darwin Del Castillo
# Date: `r Sys.Date()`
# ---

# Load necessary libraries
pacman::p_load(
  lme4,
  lmerTest,
  gtsummary,
  flextable,
  cardx,
  survey
)

# Setting survey design
data_survey <- svydesign(
  id      = ~distrito,
  strata = ~provincia,
  weights = ~w_total,
  data    = data_clean,
  nest    = TRUE
)

# Tabla general con survey design
tabla_1 <- tbl_svysummary(data = data_survey,
    by = "provincia", # grouping by province
    include = c("edad", "estado_civil", "educacion_agrupado", "religion",
                "etnia_identificador", "etnia", "post_aborto_anticoncep",
                "limitacion_economica", "aborto_busco_ayuda", "plan_familiar_ident",
                "pre_aborto_anticoncep", "serv_plani_fam", "atencion_calidad", 
                "establecimiento_tipo", "problema_atencion", "consejeria_aborto"), # variables relevantes
    missing = "no",
    statistic = list(all_continuous() ~ "{mean} ± {sd}",
                     all_categorical() ~ "{n_unweighted} ({p_unweighted}%)"),
    digits = list(all_continuous() ~ c(1, 1),
                  all_categorical() ~ c(0, 1, 1, 1)),
    label = list(edad ~ "Edad",
                 estado_civil ~ "Estado civil",
                 educacion_agrupado ~ "Nivel educativo",
                 religion ~ "Religión",
                 etnia ~ "Etnia",
                 etnia_identificador ~ "Se identifica con una etnia",
                 pre_aborto_anticoncep ~ "Uso de anticonceptivos pre aborto",
                 post_aborto_anticoncep ~ "Uso de anticonceptivos post aborto",
                 limitacion_economica ~ "Limitaciones económicas",
                 aborto_busco_ayuda ~ "Buscó ayuda después del aborto",
                 plan_familiar_ident ~ "Uso de método de planificación familiar",
                 consejeria_aborto ~ "Recibió consejería post-aborto",
                 serv_plani_fam ~ "Servicio de planificación familiar",
                 atencion_calidad ~ "Atención de calidad",
                 establecimiento_tipo ~ "Tipo de establecimiento",
                 problema_atencion ~ "Problemas en la atención")
  ) |>
  add_stat_label(
    label = list(
      all_continuous() ~ "Media (DE)",
      all_categorical() ~ "n (%)"
    )
  ) |>
  add_overall() |>
  add_p(
        list(all_continuous() ~ "svy.kruskal.test",
             all_categorical() ~ "svy.adj.chisq.test")) |>
  modify_header(
    label ~ "**Variable**",
    stat_0 ~ "**Total (n = {n_unweighted})**",
    stat_1 ~ "**Huánuco (n = {n_unweighted})**",
    stat_2 ~ "**Leoncio Prado (n = {n_unweighted})**",
    stat_3 ~ "**Yarowilca (n = {n_unweighted})**"
  ) |>
  modify_footnote(
    c(stat_0, stat_1, stat_2, stat_3) ~ "Porcentajes no ponderados por diseño muestral.",
    p.value ~ "Prueba de Kruskal-Wallis ajustado por diseño muestral para variables continuas; Prueba de chi cuadrado con corrección de segundo orden de Rao y Scott.",
  )


#########################
# Analysis of individual variables associated with post-abortion contraceptive use
########################


# 2. Fit (unweighted) Poisson model with design‐based SEs:
fit_svy_zero <- svyglm(
  as.numeric(post_aborto_anticoncep) ~ 1,
  design = data_survey,
  family = poisson(link="log")
)

exp(coef(fit_svy_zero))
exp(confint.default(fit_svy_zero))

# 2.1. Fit univariate Poisson models for each variable:

# 2.2. Fit multivariate Poisson models:
model_1 <- svyglm(
  as.numeric(post_aborto_anticoncep) ~
    estado_civil,
  design = data_survey,
  family = poisson(link="log")
)

exp(coef(model_1))
exp(confint.default(model_1))

model_2 <- svyglm(
  as.numeric(post_aborto_anticoncep) ~ 
    estado_civil +
    plan_familiar_ident,
  design = data_survey,
  family = poisson(link="log")
)

exp(coef(model_2))
exp(confint.default(model_2))

model_3 <- svyglm(
  as.numeric(post_aborto_anticoncep) ~ 
    estado_civil +
    plan_familiar_ident +
    pre_aborto_anticoncep,
  design = data_survey,
  family = poisson(link="log")
)

exp(coef(model_3))
exp(confint.default(model_3))

model_4 <- svyglm(
  as.numeric(post_aborto_anticoncep) ~ 
    estado_civil +
    plan_familiar_ident +
    pre_aborto_anticoncep +
    etnia_identificador,
  design = data_survey,
  family = poisson(link="log")
)

exp(coef(model_4))
exp(confint.default(model_4))

model_5 <- svyglm(
  as.numeric(post_aborto_anticoncep) ~ 
    estado_civil +
    plan_familiar_ident +
    pre_aborto_anticoncep +
    etnia_identificador +
    educacion_agrupado +
    religion,
  design = data_survey,
  family = poisson(link="log")
)

exp(coef(model_5))
exp(confint.default(model_5))

model_6 <- svyglm(
  as.numeric(post_aborto_anticoncep) ~ 
    estado_civil +
    pre_aborto_anticoncep,
  design = data_survey,
  family = poisson(link="log")
)

exp(coef(model_6))
exp(confint.default(model_6))

# 3. Create a summary table for the models:
# Función personalizada para extraer resultados con CI
tidy_custom <- function(x, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE, ...) {
  # Obtener coeficientes y errores estándar
  res <- broom::tidy(x)
  
  # Calcular CI manualmente
  z_val <- qnorm((1 + conf.level) / 2)
  res$conf.low <- res$estimate - z_val * res$std.error
  res$conf.high <- res$estimate + z_val * res$std.error
  
  # Exponenciar si es necesario
  if (exponentiate) {
    res$estimate <- exp(res$estimate)
    res$conf.low <- exp(res$conf.low)
    res$conf.high <- exp(res$conf.high)
  }
  
  return(res)
}

tabla_modelos_1 <- tbl_regression(
  model_1,
  exponentiate = TRUE,
  label = list(
    estado_civil ~ "Estado civil"
  ),
  tidy_fun = tidy_custom
) |>
modify_column_hide(columns = p.value) |>
modify_header(
  label ~ "**Variable**",
  estimate ~ "**RP**") |>
modify_column_merge(
    pattern = "{conf.low}, {conf.high}",
    rows = !is.na(conf.low)
  ) |>
modify_footnote(
  all_stat_cols() ~ "RP: razón de prevalencia; IC: intervalo de confianza.",
)

tabla_modelos_2 <- tbl_regression(
  model_2,
  exponentiate = TRUE,
  label = list(
    estado_civil ~ "Estado civil",
    plan_familiar_ident ~ "Usa método de planificación familiar"
  ),
  tidy_fun = tidy_custom
) |>
modify_column_hide(columns = p.value) |>
modify_header(
  label ~ "**Variable**",
  estimate ~ "**RP**") |>
modify_column_merge(
    pattern = "{conf.low}, {conf.high}",
    rows = !is.na(conf.low)
  ) |>
modify_footnote(
  all_stat_cols() ~ "RP: razón de prevalencia; IC: intervalo de confianza.",
)

tabla_modelos_3 <- tbl_regression(
  model_3,
  exponentiate = TRUE,
  label = list(
    estado_civil ~ "Estado civil",
    plan_familiar_ident ~ "Usa método de planificación familiar",
    pre_aborto_anticoncep ~ "Uso de anticonceptivos pre aborto"
  ),
  tidy_fun = tidy_custom
) |>
modify_column_hide(columns = p.value) |>
modify_header(
  label ~ "**Variable**",
  estimate ~ "**RP**") |>
modify_column_merge(
    pattern = "{conf.low}, {conf.high}",
    rows = !is.na(conf.low)
  ) |>
modify_footnote(
  all_stat_cols() ~ "RP: razón de prevalencia; IC: intervalo de confianza.",
)

tabla_modelos_4 <- tbl_regression(
  model_4,
  exponentiate = TRUE,
  label = list(
    estado_civil ~ "Estado civil",
    plan_familiar_ident ~ "Usa método de planificación familiar",
    etnia_identificador ~ "Identificación étnica",
    pre_aborto_anticoncep ~ "Uso de anticonceptivos pre aborto"
  ),
  tidy_fun = tidy_custom
) |>
modify_column_hide(columns = p.value) |>
modify_header(
  label ~ "**Variable**",
  estimate ~ "**RP**") |>
modify_column_merge(
    pattern = "{conf.low}, {conf.high}",
    rows = !is.na(conf.low)
  ) |>
modify_footnote(
  all_stat_cols() ~ "RP: razón de prevalencia; IC: intervalo de confianza.",
)

tabla_modelos_5 <- tbl_regression(
  model_5,
  exponentiate = TRUE,
  label = list(
    estado_civil ~ "Estado civil",
    plan_familiar_ident ~ "Usa método de planificación familiar",
    etnia_identificador ~ "Identificación étnica",
    pre_aborto_anticoncep ~ "Uso de anticonceptivos pre aborto",
    educacion_agrupado ~ "Nivel educativo",
    religion ~ "Religión"
  ),
  tidy_fun = tidy_custom
) |>
modify_column_hide(columns = p.value) |>
modify_header(
  label ~ "**Variable**",
  estimate ~ "**RP**") |>
modify_column_merge(
    pattern = "{conf.low}, {conf.high}",
    rows = !is.na(conf.low)
  ) |>
modify_footnote(
  all_stat_cols() ~ "RP: razón de prevalencia; IC: intervalo de confianza.",
)

tabla_modelos_final <- tbl_merge(
  tbls = list(tabla_modelos_1, tabla_modelos_2, tabla_modelos_3, 
  tabla_modelos_4, tabla_modelos_5),
  tab_spanner = c("**Modelo 1**", "**Modelo 2**", "**Modelo 3**", 
  "**Modelo 4**", "**Modelo 5**")
)

####################################
# Analysis of service variables associated with post-abortion contraceptive use
####################################

# 4. Fit univariate Poisson models for service variables:
model_serv_1 <- svyglm(
  as.numeric(post_aborto_anticoncep) ~ aborto_busco_ayuda,
  design = data_survey,
  family = poisson(link="log")
)

model_serv_2 <- svyglm(
  as.numeric(post_aborto_anticoncep) ~ aborto_busco_ayuda +
  consejeria_aborto,
  design = data_survey,
  family = poisson(link="log")
)

model_serv_3 <- svyglm(
  as.numeric(post_aborto_anticoncep) ~ aborto_busco_ayuda +
  consejeria_aborto +
  serv_plani_fam,
  design = data_survey,
  family = poisson(link="log")
)

# 5. Create a summary table for the service models:
tabla_servicios_1 <- tbl_regression(
  model_serv_1,
  exponentiate = TRUE,
  label = list(
    aborto_busco_ayuda ~ "Buscó ayuda luego del aborto"
  ),
  tidy_fun = tidy_custom
) |>
modify_column_hide(columns = p.value) |>
modify_header(
  label ~ "**Variable**",
  estimate ~ "**RP**") |>
modify_column_merge(
    pattern = "{conf.low}, {conf.high}",
    rows = !is.na(conf.low)
  ) |>
modify_footnote(
  all_stat_cols() ~ "RP: razón de prevalencia; IC: intervalo de confianza.",
)

tabla_servicios_2 <- tbl_regression(
  model_serv_2,
  exponentiate = TRUE,
  label = list(
    aborto_busco_ayuda ~ "Buscó ayuda luego del aborto",
    consejeria_aborto ~ "Recibió consejería post-aborto"
  ),
  tidy_fun = tidy_custom
) |>
modify_column_hide(columns = p.value) |> 
modify_header(
  label ~ "**Variable**",
  estimate ~ "**RP**") |>
modify_column_merge(
    pattern = "{conf.low}, {conf.high}",
    rows = !is.na(conf.low)
  ) |>
modify_footnote(
  all_stat_cols() ~ "RP: razón de prevalencia; IC: intervalo de confianza.",
)

tabla_servicios_3 <- tbl_regression(
  model_serv_3,
  exponentiate = TRUE,
  label = list(
    aborto_busco_ayuda ~ "Buscó ayuda luego del aborto",
    consejeria_aborto ~ "Recibió consejería post-aborto",
    serv_plani_fam ~ "Servicio de planificación familiar"  
    ),
  tidy_fun = tidy_custom
) |>
modify_column_hide(columns = p.value) |> 
modify_header(
  label ~ "**Variable**",
  estimate ~ "**RP**") |>
modify_column_merge(
    pattern = "{conf.low}, {conf.high}",
    rows = !is.na(conf.low)
  ) |>
modify_footnote(
  all_stat_cols() ~ "RP: razón de prevalencia; IC: intervalo de confianza.",
)

# 5. Merging tables for service models:
tabla_servicios_final <- tbl_merge(
  tbls = list(tabla_servicios_1, tabla_servicios_2, tabla_servicios_3),
  tab_spanner = c("**Modelo 1**", "**Modelo 2**", "**Modelo 3**")
)
