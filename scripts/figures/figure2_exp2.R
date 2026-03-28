rm(list = ls())

# ======================
# Librerías
# ======================
library(dplyr)
library(ggplot2)

# =========================
# CONFIGURACIÓN
# =========================
orientation <- "horizontal"  # "horizontal" o "vertical"

# =========================
# DATOS
# =========================
datos <- UVB_Preference_KGH
datos$UVB <- as.numeric(as.character(datos$UVB))

datos_exp2 <- datos %>%
  filter(Experimento %in% c("Exp_2A", "Exp_2B"),
         UVB %in% c(5, 100)) %>%
  mutate(
    UVB_cat = ifelse(UVB == 5, "Low UVBR", "High UVBR"),
    Temp_cat = case_when(
      Experimento == "Exp_2A" ~ "Low constant temperature",
      Experimento == "Exp_2B" ~ "High constant temperature"
    )
  )

# Totales fijos
totales_fijos <- data.frame(
  Sp = rep(c("Engystomops pustulosus",
             "Boana platanera",
             "Rhinella horribilis"), each = 2),
  Experimento = rep(c("Exp_2A", "Exp_2B"), times = 3),
  total_expuestos = 45
)

# =========================
# RESUMEN
# =========================
resumen_exp2 <- datos_exp2 %>%
  group_by(Sp, Experimento, UVB_cat, Temp_cat) %>%
  summarise(n = n(), .groups = "drop") %>%
  left_join(totales_fijos, by = c("Sp", "Experimento")) %>%
  mutate(
    porcentaje = (n / total_expuestos) * 100,
    UVB_cat = factor(UVB_cat, levels = c("Low UVBR", "High UVBR")),
    Temp_cat = factor(Temp_cat,
                      levels = c("Low constant temperature",
                                 "High constant temperature")),
    Sp = factor(Sp, levels = c(
      "Engystomops pustulosus",
      "Boana platanera",
      "Rhinella horribilis"
    ))
  )

# =========================
# SIGNIFICANCIA
# =========================
sig <- resumen_exp2 %>%
  filter(Sp %in% c("Engystomops pustulosus", "Boana platanera")) %>%
  group_by(Sp, Temp_cat) %>%
  summarise(
    xstart = 1,
    xend = 2,
    ymax = max(porcentaje),
    y = ymax + 8,
    y_text = ymax + 12,
    label = "*",
    .groups = "drop"
  )

# =========================
# BASE DEL GRÁFICO
# =========================
p <- ggplot(resumen_exp2, aes(x = UVB_cat, y = porcentaje))

p <- p +
  geom_bar(
    aes(fill = UVB_cat),
    stat = "identity",
    width = 0.6,
    color = "black"
  ) +
  geom_text(
    aes(label = round(porcentaje, 1)),
    vjust = -0.5,
    size = 3
  )

# corchetes
p <- p +
  geom_segment(
    data = sig,
    aes(x = xstart, xend = xend, y = y, yend = y),
    inherit.aes = FALSE
  ) +
  geom_segment(
    data = sig,
    aes(x = xstart, xend = xstart, y = y, yend = y - 3),
    inherit.aes = FALSE
  ) +
  geom_segment(
    data = sig,
    aes(x = xend, xend = xend, y = y, yend = y - 3),
    inherit.aes = FALSE
  ) +
  geom_text(
    data = sig,
    aes(x = 1.5, y = y_text, label = label),
    inherit.aes = FALSE,
    size = 6
  )

# =========================
# ORIENTACIÓN
# =========================
if (orientation == "horizontal") {
  p <- p + facet_grid(Temp_cat ~ Sp)
} else {
  p <- p + facet_grid(Sp ~ Temp_cat)
}

# =========================
# ESTÉTICA
# =========================
p <- p +
  scale_fill_manual(values = c("black", "white")) +
  scale_y_continuous(
    limits = c(0, 110),
    breaks = seq(0, 100, 25)
  ) +
  labs(
    x = "",
    y = "Tadpoles preference (%)"
  ) +
  theme_classic() +
  theme(
    strip.text.x = element_text(face = "italic"),
    strip.text.y = element_text(face = "plain"),
    legend.title = element_blank()
  )

# =========================
# MOSTRAR
# =========================
print(p)

# =========================
# EXPORTAR
# =========================
ggsave(
  filename = paste0("results/figure2_", orientation, ".png"),
  plot = p,
  width = 8,
  height = 6,
  dpi = 300
)
