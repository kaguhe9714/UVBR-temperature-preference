rm(list = ls())

# ======================
# Librerías
# ======================

library(dplyr)
library(ggplot2)

# =========================
# CONFIGURACIÓN
# =========================
color_mode <- "bn"        # "bw" o "color"
orientation <- "horizontal"  # "horizontal" o "vertical"

# =========================
# DATOS
# =========================
datos <- UVB_Preference_KGH

datos_exp1 <- datos %>%
  filter(Experimento %in% c("Exp_1A", "Exp_1B")) %>%
  mutate(
    UVB_cat = ifelse(UVB == "5", "Low", "High")
  )

resumen_exp1 <- datos_exp1 %>%
  group_by(Sp, UVB_cat) %>%
  summarise(
    n = n(),
    media = mean(Temperature),
    sd = sd(Temperature),
    se = sd / sqrt(n),
    .groups = "drop"
  ) %>%
  mutate(
    UVB_cat = factor(UVB_cat, levels = c("Low", "High")),
    Sp = factor(Sp, levels = c(
      "Engystomops pustulosus",
      "Boana platanera",
      "Rhinella horribilis"
    ))
  )

# =========================
# SIGNIFICANCIA
# =========================
sig <- data.frame(
  Sp = factor("Boana platanera", levels = levels(resumen_exp1$Sp)),
  xstart = 1,
  xend = 2,
  y = max(resumen_exp1$media + resumen_exp1$se) + 0.6,
  label = "*"
)

# =========================
# BASE DEL GRÁFICO
# =========================
p <- ggplot(resumen_exp1, aes(x = UVB_cat, y = media))

# puntos
if (color_mode == "color") {
  p <- p + geom_point(aes(color = UVB_cat), size = 3)
} else {
  p <- p + geom_point(size = 3)
}

# barras de error
p <- p +
  geom_errorbar(
    aes(ymin = media - se, ymax = media + se),
    width = 0.1,
    color = "black"
  )

# etiquetas de valores
p <- p +
  geom_text(
    aes(label = round(media, 1)),
    position = position_nudge(x = 0.20),
    vjust = 0.5,
    size = 3
  )

# corchete
p <- p +
  geom_segment(
    data = sig,
    aes(x = xstart, xend = xend, y = y, yend = y),
    inherit.aes = FALSE
  ) +
  geom_segment(
    data = sig,
    aes(x = xstart, xend = xstart, y = y, yend = y - 0.3),
    inherit.aes = FALSE
  ) +
  geom_segment(
    data = sig,
    aes(x = xend, xend = xend, y = y, yend = y - 0.3),
    inherit.aes = FALSE
  ) +
  geom_text(
    data = sig,
    aes(x = 1.5, y = y + 0.15, label = label),
    inherit.aes = FALSE,
    size = 6
  )

# facetas
if (orientation == "vertical") {
  p <- p + facet_wrap(~Sp, ncol = 1)
} else {
  p <- p + facet_wrap(~Sp)
}

# colores
if (color_mode == "color") {
  p <- p + scale_color_manual(values = c("green", "red"))
}

# ejes y tema
p <- p +
  labs(
    x = "Constant UVBR",
    y = "Tadpole preferred temperature (°C)"
  ) +
  scale_y_continuous(
    limits = c(28, 34),
    breaks = seq(28, 34, 1)
  ) +
  theme_classic() +
  theme(
    strip.text = element_text(face = "italic"),
    legend.position = "none"
  )

# =========================
# MOSTRAR
# =========================
print(p)

# =========================
# EXPORTAR (opcional)
# =========================
ggsave(
  filename = paste0("results/figure1_", color_mode, "_", orientation, ".png"),
  plot = p,
  width = 7,
  height = 5,
  dpi = 300
)
