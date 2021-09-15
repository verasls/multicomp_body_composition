# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
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
  as_tibble()

# Build plot --------------------------------------------------------------

alst_plot <- ggplot(plot_data, aes(x = alst_measured, y = alst_predicted)) +
  geom_point(alpha = 0.8) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 35),
    breaks = seq(0, 35, 5)
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(0, 35),
    breaks = seq(0, 35, 5)
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
    x = "Measured ALST (kg)",
    y = "Predicted ALST (kg)"
  )

fm_plot <- ggplot(plot_data, aes(x = fm_measured, y = fm_predicted)) +
  geom_point(alpha = 0.8) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 50),
    breaks = seq(0, 50, 10)
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
    x = "Measured FM (kg)",
    y = "Predicted FM (kg)"
  )

bmc_plot <- ggplot(plot_data, aes(x = bmc_measured, y = bmc_predicted)) +
  geom_point(alpha = 0.8) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 4),
    breaks = seq(0, 4, 0.5)
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(0, 4),
    breaks = seq(0, 4, 0.5)
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
    x = "Measured BMC (kg)",
    y = "Predicted BMC (kg)"
  )

# Combine plots and save --------------------------------------------------

fig <- alst_plot + fm_plot + bmc_plot

agg_png(
  here("figures", "measured_vs_predicted.png"),
  width = 75,
  height = 30,
  units = "cm",
  res = 100,
  scaling = 2
)
plot(fig)
dev.off()
