# ---
# Probability sampling unit reconstruction
# Author: Darwin Del Castillo
# Date: `r Sys.Date()`
# ---

# 1) Vectors de M_j y m_j:
tot_distritos <- c("Huánuco"       = 13,
                   "Leoncio Prado"  = 10,
                   "Yarowilca"     = 8)

sel_distritos <- c("Huánuco"       = 3,
                   "Leoncio Prado"  = 4,
                   "Yarowilca"     = 3)

# 2) District weigth: w_dist = (11/3)*(M_j / m_j)
w_dist <- (11/3) * (tot_distritos / sel_distritos)
w_dist
# Huanuco       LeoncioPrado       Yarowilca
# 15.888889     9.166667           9.777778

# 3) Assigning each district weight to each province in data_clean:
data_mid$w_distrito <- w_dist[data_mid$provincia]

# 4) Generating the final dataset with individual weights:
#    w_total = w_distrito * (N_i / n_i),
#    where N_i = population 20–49 in district, n_i = muestra final.

poblacion_distritos <- tibble(id = data_mid$id,
distrito = data_mid$distrito,
provincia = data_mid$provincia,
poblacion = NA,
muestra_final = NA) |>
  mutate(poblacion = case_when(
    distrito == "Huánuco"       ~ 19875,
    distrito == "Pumahuasi"  ~ 1222,
    distrito == "Hermilio Valdizan"     ~ 660,
    distrito == "Luyando"          ~ 863,
    distrito == "Pillcomarca"   ~ 9734,
    distrito == "Chavinillo"     ~ 1096,
    distrito == "Rupa Rupa"    ~ 12132,
    distrito == "Amarilis"      ~ 18536,
    distrito == "Cahuac"   ~ 249,
    distrito == "Jacas Chico"     ~ 252
  ),
  muestra_final = case_when(
    distrito == "Huánuco"       ~ 255,
    distrito == "Pumahuasi"  ~ 238,
    distrito == "Hermilio Valdizan"     ~ 50,
    distrito == "Luyando"          ~ 66,
    distrito == "Pillcomarca"   ~ 125,
    distrito == "Chavinillo"     ~ 84,
    distrito == "Rupa Rupa"    ~ 156,
    distrito == "Amarilis"      ~ 238,
    distrito == "Cahuac"   ~ 19,
    distrito == "Jacas Chico"     ~ 19
  ))

data_clean <- data_mid |>
  left_join(poblacion_distritos,
            by = c("id", "distrito", "provincia"),
            relationship = "one-to-one") |>
  mutate(
    w_persona = poblacion / muestra_final,
    w_total   = w_distrito * w_persona
  ) |>
  mutate(
    fpc1 = tot_distritos[provincia],
    fpc2 = poblacion
  )

rm(list = setdiff(ls(), "data_clean"))
