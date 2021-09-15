# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(lvmisc)
library(patchwork)
library(ragg)

# Load data ---------------------------------------------------------------

load(here("data", "data.rda"))
load(here("output", "model.rda"))

# Build plot data ---------------------------------------------------------

plot_data <- data |>
  select(alst_measured = alst, fm_measured = fm, bmc_measured = bmc)
predicted <- predict(m) |>
  as_tibble() |>
  select(alst_predicted = alst, fm_predicted = fm, bmc_predicted = bmc)
plot_data <- cbind(plot_data, predicted) |>
  as_tibble() |>
  mutate(
    alst_diff = alst_measured - alst_predicted,
    alst_mean = (alst_measured + alst_predicted) / 2,
    fm_diff = fm_measured - fm_predicted,
    fm_mean = (fm_measured + fm_predicted) / 2,
    bmc_diff = bmc_measured - bmc_predicted,
    bmc_mean = (bmc_measured + bmc_predicted) / 2
  )

# Build plot --------------------------------------------------------------

alst_bias <- bias(plot_data$alst_measured, plot_data$alst_predicted)
alst_loa <- loa(plot_data$alst_measured, plot_data$alst_predicted)
alst_ba <- ggplot(plot_data) +
  geom_point(aes(x = alst_mean, y = alst_diff), alpha = 0.8, size = 1) +
  geom_hline(yintercept = alst_bias) +
  geom_hline(yintercept = alst_loa$lower, linetype = "longdash") +
  geom_hline(yintercept = alst_loa$upper, linetype = "longdash") +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(-5, 5),
    breaks = seq(-5, 5, 1)
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(0, 40),
    breaks = seq(0, 40, 10)
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
  ) +
  labs(
    x = "Mean of measured and predicted ALST (kg)",
    y = "Measured - predicted appendicular ALST (kg)"
  )
 
fm_bias <- bias(plot_data$fm_measured, plot_data$fm_predicted)
fm_loa <- loa(plot_data$fm_measured, plot_data$fm_predicted)
fm_ba <- ggplot(plot_data) +
  geom_point(aes(x = fm_mean, y = fm_diff), alpha = 0.8, size = 1) +
  geom_hline(yintercept = fm_bias) +
  geom_hline(yintercept = fm_loa$lower, linetype = "longdash") +
  geom_hline(yintercept = fm_loa$upper, linetype = "longdash") +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(-7, 7),
    breaks = seq(-7, 7, 1)
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(0, 50),
    breaks = seq(0, 50, 10)
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
  ) +
  labs(
    x = "Mean of measured and predicted FM (kg)",
    y = "Measured - predicted appendicular FM (kg)"
  )
  
bmc_bias <- bias(plot_data$bmc_measured, plot_data$bmc_predicted)
bmc_loa <- loa(plot_data$bmc_measured, plot_data$bmc_predicted)
bmc_ba <- ggplot(plot_data) +
  geom_point(aes(x = bmc_mean, y = bmc_diff), alpha = 0.8, size = 1) +
  geom_hline(yintercept = bmc_bias) +
  geom_hline(yintercept = bmc_loa$lower, linetype = "longdash") +
  geom_hline(yintercept = bmc_loa$upper, linetype = "longdash") +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(-1, 1),
    breaks = seq(-1, 1, 0.2)
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(0, 4),
    breaks = seq(0, 4, 1)
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
  ) +
  labs(
    x = "Mean of measured and predicted BMC (kg)",
    y = "Measured - predicted appendicular BMC (kg)"
  )
  
# Combine plots and save --------------------------------------------------

fig <- alst_ba + fm_ba + bmc_ba

agg_png(
  here("figures", "bland_altman.png"),
  width = 75,
  height = 20,
  units = "cm",
  res = 100,
  scaling = 1.5
)
plot(fig)
dev.off()
