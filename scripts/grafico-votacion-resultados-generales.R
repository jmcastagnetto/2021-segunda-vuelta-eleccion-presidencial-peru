library(tidyverse)
#library(patchwork)
library(ggforce)
library(grid)
library(gridExtra)

resultados_df <- read_csv(
  "datos/generales/votacion-general-resultados.csv",
  col_types = cols(
    C_CODI_AGRUPOL = col_character(),
    AGRUPACION = col_character(),
    TOTAL_VOTOS = col_double(),
    NLISTA = col_double(),
    POR_VALIDOS = col_double(),
    POR_EMITIDOS = col_double(),
    NOMBREe_CANDIDATO = col_character(),
    updated = col_datetime(format = ""),
    avance = col_double()
  )
) %>%
  mutate( # conbertir a la tz de Peru
    updated = lubridate::as_datetime(updated, tz = "America/Lima")
  ) %>%
  mutate(
    AGRUPACION = if_else( # Simplificar para legibilidad en el gráfico
      AGRUPACION == "PARTIDO POLITICO NACIONAL PERU LIBRE",
      "PERU LIBRE",
      AGRUPACION
    )
  )

last_update <- max(resultados_df$updated)

p1 <- ggplot(
  resultados_df,
  aes(x = updated, y = TOTAL_VOTOS, group = AGRUPACION, color = AGRUPACION)
) +
  geom_line(size = 1) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    y = "Total de votos",
    x = "Hora de actualización",
    title = "Total de votos por partido"
  ) +
  theme_bw(18) +
  theme(
    legend.position = "right"
  ) +
  facet_zoom(
    ylim = c(8e6, 9e6),
    xlim = c(lubridate::ymd_hms("2021-06-07 07:00:00", tz = "America/Lima"),
             last_update),
    split = FALSE,
    shrink = TRUE,
    zoom.size = 1.5,
    horizontal = FALSE
  )

p2 <- ggplot(
  resultados_df,
  aes(x = updated, y = POR_VALIDOS, group = AGRUPACION, color = AGRUPACION)
) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "grey") +
  geom_line(size = 1) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(n.breaks = 3) +
  labs(
    y = "Porcentaje de votos válidos",
    x = "Hora de actualización",
    title = "Porcentajes por partido"
  ) +
  theme_bw(18) +
  theme(
    legend.position = "none"
  ) +
  facet_zoom(
    xlim = c(lubridate::ymd_hms("2021-06-07 07:00:00", tz = "America/Lima"),
             last_update),
    ylim = c(49, 51),
    split = FALSE,
    shrink = TRUE,
    zoom.size = 1.5,
    horizontal = FALSE
  )


p3 <- ggplot(
  resultados_df %>% select(updated, avance) %>% distinct(),
  aes(x = updated, y = avance)
) +
  geom_col(fill = "gray70") +
  geom_text(aes(label = sprintf("%.2f%%", avance)),
            hjust = 0, nudge_y = .5, angle = 90, size = 4) +
  scale_y_continuous(limits = c(0, 110)) +
  labs(
    y = "Porcentaje de avance",
    x = "Hora de actualización",
    title = "Porcentaje de avance del conteo"
  ) +
  theme_classic(16) +
  theme(
    plot.caption = element_text(family = "Inconsolata")
  )

layout <- rbind(
  c(1, 1, 1, 1, 2, 2, 2),
  c(1, 1, 1, 1, 2, 2, 2),
  c(1, 1, 1, 1, 2, 2, 2),
  c(1, 1, 1, 1, 2, 2, 2),
  c(1, 1, 1, 1, 2, 2, 2),
  c(1, 1, 1, 1, 2, 2, 2),
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
)

pcomb <- gridExtra::arrangeGrob(
  p1, p2, p3,
  layout_matrix = layout,
  top = textGrob("Perú: Elecciones Presidenciales 2021, Segunda vuelta",
                 gp = gpar(fontsize = 32, fontface = "bold")),
  bottom = textGrob(glue::glue("@jmcastagnetto, Jesus M. Castagnetto // Gráfico actualizado {lubridate::now(tzone = 'America/Lima')} (PET) // Fuente: ONPE"),
                    just = c("left", "center"),
                    gp = gpar(fontsize = 14, fontfamily = "Inconsolata"))
)

# patchwork and facet_zoom do not play well together
# pcomb <- (gridExtra::arrangeGrob(p1, p2) / p3) +
#   plot_layout(heights = c(2, 1)) +
#   plot_annotation(
#     title = "Perú: Elecciones Presidenciales 2021, Segunda vuelta",
#     caption = glue::glue("@jmcastagnetto, Jesus M. Castagnetto // Gráfico actualizado {lubridate::now(tzone = 'America/Lima')} (PET) // Fuente: ONPE")) &
#   theme(
#     plot.title = element_text(size = 28),
#     plot.caption = element_text(family = "Inconsolata", size = 16)
#   )
# pcomb

ggsave(
  plot = pcomb,
  filename = "plots/resultados-generales-votacion-presidencial.png",
  width = 14,
  height = 12

)
