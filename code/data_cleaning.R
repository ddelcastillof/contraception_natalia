# ---
# Data cleaning
# Author: Darwin Del Castillo
# Date: `r Sys.Date()`
# ---

# Load necessary libraries
pacman::p_load(tidyverse, 
               janitor,
               stringr,
               readxl)
##### Loading data #####
# Load the data
data_raw <- read_xlsx(path = "data/raw/data_final_raw.xlsx",
                               sheet = "BASE 900 CODIFICADA",
                               range = "A1:CX901",
                               col_names = TRUE,
                               na = c("", "NA")) |>
  clean_names() |>
  ## Creating column with number of row as values
  mutate(index = row_number())

data_identifiers <- read_xlsx(path = "data/raw/data_final_raw.xlsx",
                              sheet = "DATA 890",
                              range = "A1:DK901",
                              col_names = TRUE,
                              na = c("", "NA")) |> 
  clean_names() |> select(-index) |>
  mutate(index = row_number()) |>
  select(index, id)

## Joint ids with data_raw keeping ids at the beginning of the dataset
data_mid <- data_raw |>
  left_join(data_identifiers, by = "index") |>
  select(id, everything()) |>
  select(-index)

## Removing intermediate databases
rm(data_identifiers, data_raw)

##### Data cleaning #####
# Changing names of variables to something more readable
data_mid <- data_mid |>
  rename(
    codigo_encuestador = codigo_de_encuestador,
    edad = que_edad_tiene,
    estado_civil = que_estado_civil_tiene,
    educacion = cual_es_su_nivel_educativo,
    ocupacion = a_que_se_dedica,
    educacion_agrupado = nivela_grupado,
    provincia = en_que_provincia_vive,
    distrito = en_que_distrito_vive,
    etnia_identificador = se_identifica_con_alguna_comunidad_nativa_o_poblacion_etnica,
    etnia = si_marca_si_cual_es,
    religion = pertenece_a_alguna_religion,
    religion_otra = otro_mencione_cual_13,
    celular = cuenta_con_un_celular_para_comunicarse,
    celular_wsp = el_celular_cuenta_con_el_aplicativo_por_whats_app,
    internet_identificador = cuenta_con_internet_en_su_celular_las_24_hrs,
    internet_tiempo = en_que_horario_cuenta_con_internet,
    embarazada_identificador = has_estado_alguna_vez_embarazada,
    hijos_numero = cuantos_hijos_tiene,
    ultima_gestacion = como_concluyo_su_ultima_gestacion,
    ult_embarazo_fecha = cuando_fue_su_ultimo_embarazo,
    tiene_pareja = actualmente_tiene_una_pareja,
    tiempo_pareja = cuanto_tiempo_de_relacion_tienes_con_tu_pareja,
    plan_familiar_ident = actualmente_usas_algun_metodo_de_planificacion_familiar,
    plan_familiar_why = cual_es_la_razon_principal_por_la_cual_no_utilizas_un_metodo_anticonceptivo,
    plan_familiar_otro = otro_mencione,
    met_contra = que_metodo_anticonceptivo_utiliza_actualmente,
    met_contra_donde = de_donde_obtiene_su_metodo_anticonceptivo_actualmente,
    met_contra_otro = otro_mencione_2,
    pareja_met_contra = tu_pareja_sabe_que_usas_un_metodo_anticonceptivo,
    pareja_met_contra_a = tu_pareja_esta_de_acuerdo_con_que_utilices_un_metodo_anticonceptivo,
    pareja_met_decide = tu_pareja_toma_la_ultima_decision_para_que_utilices_un_metodo_anticonceptivo,
    
  ) |>
  # Convert date and time to appropriate formats
  mutate(
    fecha = as.Date(fecha, format = "%Y-%m-%d"),
    hora = str_pad(hora, width = 4, pad = "0"),
    hora = str_sub(hora, 1, 2) %>% as.integer()
  ) |>
  # Remove rows with missing values in key columns
  filter(!is.na(id), !is.na(fecha), !is.na(valor)) |>
  # Remove duplicates based on id and fecha
  distinct(id, fecha, .keep_all = TRUE) |>
  # Clean up the 'valor' column to ensure it's numeric
  mutate(valor = as.numeric(valor)) |>
  # Remove rows with non-numeric 'valor'
  filter(!is.na(valor))
                               