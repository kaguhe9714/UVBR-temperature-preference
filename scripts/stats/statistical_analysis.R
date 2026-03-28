############################################################
# Title: UVB and Temperature Preference Analyses
# Author: Katalina Gutiérrez Hernández
# Description: Statistical analyses for three experiments
#              evaluating thermal preference and UVB exposure
# Software: R (version 4.5.1)
############################################################

# Clear environment
rm(list = ls())

# Load required packages
library(dplyr)

# Load data
datos <- UVB_Preference_KGH

############################################################
# 1. DATA SUBSETTING
############################################################

datos_exp1 <- datos %>% filter(Experimento %in% c("Exp_1A", "Exp_1B"))
datos_exp2 <- datos %>% filter(Experimento %in% c("Exp_2A", "Exp_2B"))
datos_exp3 <- datos %>% filter(Experimento %in% c("Exp_3A", "Exp_3B"))

############################################################
# 2. EXPERIMENT 1: TEMPERATURE PREFERENCE (MANN-WHITNEY)
############################################################

# Descriptive statistics
resumen_exp1 <- datos_exp1 %>%
  group_by(Sp, UVB) %>%
  summarise(
    n = n(),
    mean_temp = mean(Temperature, na.rm = TRUE),
    sd_temp = sd(Temperature, na.rm = TRUE),
    se = sd_temp / sqrt(n),
    CI_lower = mean_temp - qt(0.975, df = n - 1) * se,
    CI_upper = mean_temp + qt(0.975, df = n - 1) * se,
    min_temp = min(Temperature, na.rm = TRUE),
    max_temp = max(Temperature, na.rm = TRUE),
    .groups = "drop"
  )
resumen_exp1

# Normality test (Shapiro-Wilk)
normalidad_exp1 <- datos_exp1 %>%
  group_by(Experimento) %>%
  summarise(
    p_value = shapiro.test(Temperature)$p.value,
    .groups = "drop"
  )
normalidad_exp1

# Mann-Whitney test (Wilcoxon rank-sum)
mw_exp1 <- datos_exp1 %>%
  group_by(Sp) %>%
  summarise(
    test = list(wilcox.test(Temperature ~ UVB)),
    .groups = "drop"
  ) %>%
  mutate(
    W = sapply(test, function(x) x$statistic),
    p_value = sapply(test, function(x) x$p.value)
  ) %>%
  select(Sp, W, p_value)
mw_exp1

############################################################
# 3. EXPERIMENT 2: UVB PREFERENCE (BINOMIAL TEST)
############################################################

# Function to run binomial test
run_binomial <- function(data, exp_name) {
  data %>%
    filter(Experimento == exp_name, UVB %in% c("5", "100")) %>%
    group_by(Sp) %>%
    summarise(
      n_low = sum(UVB == "5"),
      n_high = sum(UVB == "100"),
      n_total = n_low + n_high,
      prop_low = n_low / n_total,
      test = list(binom.test(n_low, n_total, p = 0.5)),
      .groups = "drop"
    ) %>%
    mutate(
      p_value = sapply(test, function(x) x$p.value)
    ) %>%
    select(Sp, n_low, n_high, n_total, prop_low, p_value)
}

binom_exp2A <- run_binomial(datos, "Exp_2A")
binom_exp2B <- run_binomial(datos, "Exp_2B")

binom_exp2A
binom_exp2B

############################################################
# 4. EXPERIMENT 3: COMBINED PREFERENCE (BINOMIAL TEST)
############################################################

run_exp3 <- function(data, exp_name, reverse = FALSE) {
  data %>%
    filter(Experimento == exp_name) %>%
    mutate(
      combinacion = case_when(
        (!reverse & UVB == "5") ~ "low_low",
        (!reverse & UVB == "100") ~ "high_high",
        (reverse & UVB == "100") ~ "low_low",
        (reverse & UVB == "5") ~ "high_high",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(combinacion)) %>%
    group_by(Sp) %>%
    summarise(
      n_low_low = sum(combinacion == "low_low"),
      n_high_high = sum(combinacion == "high_high"),
      n_total = n_low_low + n_high_high,
      prop_low_low = n_low_low / n_total,
      test = list(binom.test(n_low_low, n_total, p = 0.5)),
      .groups = "drop"
    ) %>%
    mutate(
      p_value = sapply(test, function(x) x$p.value)
    ) %>%
    select(Sp, n_low_low, n_high_high, n_total, prop_low_low, p_value)
}

binom_exp3A <- run_exp3(datos, "Exp_3A", reverse = FALSE)
binom_exp3B <- run_exp3(datos, "Exp_3B", reverse = TRUE)

binom_exp3A
binom_exp3B

############################################################
# END OF SCRIPT
############################################################
