library(tidyverse)
library(patchwork)

df <- readRDS("datos/actas/actas_electorales.rds") %>%
  mutate(
    obs = fct_infreq(observacion) %>% fct_rev()
  ) %>%
  group_by(obs) %>%
  tally() %>%
  ungroup() %>%
  mutate(
    pct_lbl = glue::glue("N = {str_trim(format(n, big.mark = ','))}\n({sprintf('%.3f%%', 100 * n / sum(n))})"),
    grp = case_when(
      obs == "CONTABILIZADAS NORMALES" ~ "Normal",
      obs == "ACTA ELECTORAL PENDIENTE" ~ "Pendiente",
      obs == "EN PROCESO DE DIGITACION" ~ "Pendiente",
      TRUE ~ "Observada"
    )
  )

df1 <- df %>%
  mutate(
    grp = fct_reorder(grp, n)
  ) %>%
  group_by(grp) %>%
  summarise(
    tot = sum(n)
  ) %>%
  ungroup() %>%
  mutate(
    pct_lbl = glue::glue("N = {str_trim(format(tot, big.mark = ','))}\n({sprintf('%.3f%%', 100 * tot / sum(tot))})")
  )

p1 <- ggplot(df1, aes(y = grp, x = tot, fill = grp)) +
  geom_col(show.legend = FALSE, width = .5) +
  geom_text(aes(label = pct_lbl), size = 7,
            hjust = 0, nudge_x = 300) +
  annotate(
    geom = "text",
    x = 1.7e4,
    y = 1.7,
    label = "{",
    size = 120,
    color = "#21908CFF",
    hjust = 0
  ) +
  scale_fill_manual(values = c(
    "Pendiente" = "#FDE725FF",
    "Observada" = "#21908CFF",
    "Normal" = "#440154FF"
  )) +
  scale_x_continuous(limits = c(0, 9e4)) +
  labs(
    fill = "",
    x = "",
    y = "Estado del Acta",
    title = "Perú: Actas de las Elecciones Presidenciales 2021, Segunda vuelta",
    subtitle = "Fuente: ONPE (extraído por @Ogoun)",
    caption = "@jmcastagnetto, Jesus M. Castagnetto, 2021-06-08"
  ) +
  theme_minimal(16) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(size = 28),
    plot.subtitle = element_text(color = "gray40"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 24),
    plot.caption = element_text(family = "Inconsolata")
  )


p2 <- ggplot(df %>% filter(grp == "Observada"),
       aes(y = obs, x = n, fill = grp)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = pct_lbl), hjust = 0, nudge_x = 5, size = 3) +
  scale_fill_manual(values = c(
    "Pendiente" = "#FDE725FF",
    "Observada" = "#21908CFF",
    "Normal" = "#440154FF"
  )) +
  scale_x_continuous(limits = c(0, 550)) +
  labs(
    fill = "",
    x = "",
    y = "",
    title = "Actas observadas"
  ) +
  theme_minimal(10) +
  theme(
    axis.text.x = element_blank()
  )

p12 <- p1 + inset_element(p2, left = .3, bottom = .1, right = .8, top = .6)

ggsave(
  plot = p12,
  filename = "plots/estados-de-actas.png",
  width = 14,
  height = 9
)
