library(tidyverse)
library(knitr)
library(kableExtra)

actas <- readRDS("datos/actas/actas_electorales.rds")
update <- unique(actas$descargado)

tbl_resumen <- actas %>%
  group_by(observacion) %>%
  tally() %>%
  ungroup() %>%
  mutate(pct = 100*n/sum(n)) %>%
  arrange(desc(n)) %>%
  janitor::adorn_totals() %>%
  mutate(
    n = format(n, big.mark = ","),
    pct = sprintf("%.3f%%", pct)
  )

kable(
  tbl_resumen,
  col.names = c("Tipo de observación", "Cantidad", "Porcentaje del total"),
  align = "lrr",
  caption = "Estados de actas electorales (ONPE, Perú)"
) %>%
  kable_classic(
    full_width = FALSE
  ) %>%
  footnote(
    general = glue::glue("Datos al {update} // @jmcastagnetto, Jesus M. Castagnetto"),
    general_title = ""
  ) %>%
  row_spec(13, color = "white", bold = TRUE, background = "black") %>%
  save_kable(file = "plots/tabla-resumen-actas.png")
