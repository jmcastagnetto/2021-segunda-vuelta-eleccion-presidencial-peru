library(tidyverse)

#gs_url <- "https://docs.google.com/spreadsheets/d/1PevjP88hNI9_ypKuROGa1zzJHENGM0kgjuQYDTcPZ8M/edit#gid=1171231432"

csv_url <- "https://docs.google.com/spreadsheets/d/1PevjP88hNI9_ypKuROGa1zzJHENGM0kgjuQYDTcPZ8M/export?format=csv&gid=1171231432"

download_time <- Sys.time()

actas <- read_csv(csv_url) %>%
  mutate(
    ubigeo = sprintf("%06d", ubigeo)
  ) %>%
  rename(
    votos_perulibre = votos_pl,
    votos_fuerzapopular = votos_fp
  ) %>%
  add_column(
    descargado = download_time
  )

write_csv(
  actas,
  file = "datos/actas/actas_electorales.csv.gz"
)

saveRDS(
  actas,
  file = "datos/actas/actas_electorales.rds"
)
