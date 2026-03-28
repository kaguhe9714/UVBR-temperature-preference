rm(list = ls())

# ======================
# Librerías
# ======================

library(dplyr)
library(ggplot2)
library(grid)

# =========================
# CONFIGURACIÓN
# =========================
orientation <- "horizontal"   # "horizontal" o "vertical"
color_mode  <- "bw"           # "bw" o "color"

# =========================
# DATOS
# =========================

datos <- read.csv("data/UVB_Preference_KGH.csv")

datos_exp3 <- datos %>%
  filter(Experimento %in% c("Exp_3A", "Exp_3B")) %>%
  group_by(Experimento) %>%
  mutate(
    UVB_cat = case_when(
      UVB == min(UVB) ~ "Low",
      UVB == max(UVB) ~ "High",
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup() %>%
  filter(!is.na(UVB_cat))

# =========================
# TEMPERATURA (CONSISTENTE)
# =========================
datos_exp3 <- datos_exp3 %>%
  mutate(
    Temp_cat = case_when(
      Experimento == "Exp_3A" & UVB_cat == "Low"  ~ "Low temperature",
      Experimento == "Exp_3A" & UVB_cat == "High" ~ "High temperature",
      Experimento == "Exp_3B" & UVB_cat == "Low"  ~ "High temperature",
      Experimento == "Exp_3B" & UVB_cat == "High" ~ "Low temperature"
    )
  )

# =========================
# RESUMEN
# =========================
resumen_exp3 <- datos_exp3 %>%
  group_by(Experimento, Sp, UVB_cat, Temp_cat) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(porcentaje = (n / 45) * 100)

# =========================
# FACTORES
# =========================
resumen_exp3 <- resumen_exp3 %>%
  mutate(
    Sp = factor(Sp, levels = c(
      "Engystomops pustulosus",
      "Boana platanera",
      "Rhinella horribilis"
    )),
    Temp_cat = factor(Temp_cat,
                      levels = c("Low temperature", "High temperature")),
    Experimento = recode(Experimento,
                         "Exp_3A" = "Experiment 3A",
                         "Exp_3B" = "Experiment 3B")
  )

# =========================
# SIGNIFICANCIA
# =========================
brackets <- resumen_exp3 %>%
  group_by(Experimento, Sp) %>%
  summarise(
    y = max(porcentaje) + 12,
    y_text = y + 5,
    .groups = "drop"
  )

# =========================
# COLORES
# =========================
if (color_mode == "bw") {
  fill_values <- c("white", "black")
} else {
  fill_values <- c("#1b9e77", "#d95f02")
}

# =========================
# BASE PLOT
# =========================
p <- ggplot(resumen_exp3,
            aes(x = Temp_cat, y = porcentaje, fill = UVB_cat))

p <- p +
  geom_bar(stat = "identity", color = "black", width = 0.5) +
  geom_text(aes(label = round(porcentaje, 1)),
            vjust = -0.5, size = 3)

# =========================
# CORCHETES
# =========================
p <- p +
  geom_segment(data = brackets,
               aes(x = 1, xend = 2, y = y, yend = y),
               inherit.aes = FALSE) +
  geom_segment(data = brackets,
               aes(x = 1, xend = 1, y = y, yend = y - 3),
               inherit.aes = FALSE) +
  geom_segment(data = brackets,
               aes(x = 2, xend = 2, y = y, yend = y - 3),
               inherit.aes = FALSE) +
  geom_text(data = brackets,
            aes(x = 1.5, y = y_text, label = "*"),
            inherit.aes = FALSE,
            size = 6)

# =========================
# ORIENTACIÓN
# =========================
if (orientation == "horizontal") {
  p <- p + facet_grid(Experimento ~ Sp)
} else {
  p <- p + facet_grid(Sp ~ Experimento)
}

# =========================
# ESTÉTICA
# =========================
p <- p +
  scale_fill_manual(values = fill_values) +
  scale_y_continuous(
    breaks = seq(0, 100, 20),
    limits = c(0, 110)
  ) +
  labs(
    x = "",
    y = "Tadpole preference (%)"
  ) +
  theme_classic() +
  theme(
    strip.text.x = element_text(face = "italic"),
    strip.text.y = element_text(face = "bold"),
    legend.title = element_blank(),
    legend.position = "top"
  )

# =========================
# MOSTRAR
# =========================
print(p)

# =========================
# EXPORTAR
# =========================
ggsave(
  filename = paste0("results/figure3_", orientation, "_", color_mode, ".png"),
  plot = p,
  width = 8,
  height = 6,
  dpi = 300
)
