# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(psych)
library(lvmisc)
library(car)
source(here("code", "funs.R"))

# Load data ---------------------------------------------------------------

load(here("data", "data.rda"))

# Principal component analysis --------------------------------------------

# Build a correlation matrix
corr_matrix <- data |>
  select(where(is.double), - c(alst, fm, bmc)) |>
  cor()
# Compute its determinant
corr_matrix_det <- det(corr_matrix)
# Barlett test
barlett <- cortest.bartlett(corr_matrix, n = nrow(data))
# Kaiser-Meyer-Olkin test
kmo <- kmo(corr_matrix)

# 1st PCA model (extracting the same number of variables as factors)
pc1 <- principal(
  corr_matrix, nfactors = length(corr_matrix[1, ]), rotate = "varimax"
)
# Scree plot
scree_plot_1 <- scree_plot(pc1$values)

# 2nd PCA model (extracting 6 factors, as suggested by the eigenvalues and
# the scree plot)
pc2 <- principal(corr_matrix, nfactors = 6, rotate = "varimax")
# Print factors with a loading > 0.5
print.psych(pc2, cut = 0.5, sort = TRUE)

# Univariate models -------------------------------------------------------

alst_uni <- lm(
  alst ~ sf_axillary + sf_suprailiac + circ_abdominal + sf_subscapular +
    sf_biceps + sf_abdominal + circ_waist + circ_hip + circ_thorax +
    circ_arm + weight + br_biiliac + height + half_armspan + knee_height +
    br_biacromial + br_bimalleolar + br_chest + br_wrist + br_cubital +
    circ_forearm + br_knee + circ_calf + sf_thigh + circ_thigh + sf_triceps +
    age + sf_chest,
  data = data
)
alst_uni_step <- step(alst_uni)
summary(alst_uni_step)

fm_uni <- lm(
  fm ~ sf_axillary + sf_suprailiac + circ_abdominal + sf_subscapular +
    sf_biceps + sf_abdominal + circ_waist + circ_hip + circ_thorax +
    circ_arm + weight + br_biiliac + height + half_armspan + knee_height +
    br_biacromial + br_bimalleolar + br_chest + br_wrist + br_cubital +
    circ_forearm + br_knee + circ_calf + sf_thigh + circ_thigh + sf_triceps +
    age + sf_chest,
  data = data
)
fm_uni_step <- step(fm_uni)
summary(fm_uni_step)

bmc_uni <- lm(
  bmc ~ sf_axillary + sf_suprailiac + circ_abdominal + sf_subscapular +
    sf_biceps + sf_abdominal + circ_waist + circ_hip + circ_thorax +
    circ_arm + weight + br_biiliac + height + half_armspan + knee_height +
    br_biacromial + br_bimalleolar + br_chest + br_wrist + br_cubital +
    circ_forearm + br_knee + circ_calf + sf_thigh + circ_thigh + sf_triceps +
    age + sf_chest,
  data = data
)
bmc_uni_step <- step(bmc_uni)
summary(bmc_uni_step)

# Build final univariate models and compute VIF
alst_lm <- lm(
  alst ~ weight + half_armspan + br_cubital + sf_triceps +
    sf_thigh + sf_chest,
  data = data
)
alst_vif <- lvmisc::vif(alst_lm)

fm_lm <- lm(
  fm ~ weight + half_armspan + br_cubital + sf_triceps +
    sf_thigh + sf_chest,
  data = data
)
fm_vif <- lvmisc::vif(fm_lm)

bmc_lm <- lm(
  bmc ~ weight + half_armspan + br_cubital + sf_triceps +
    sf_thigh + sf_chest,
  data = data
)
bmc_vif <- lvmisc::vif(bmc_lm)

# Multivariate model ------------------------------------------------------

# Set a matrix of the outcomes
y <- cbind(data$alst, data$fm, data$bmc)
colnames(y) <-  c("alst", "fm", "bmc")
# Build the model
m <- lm(
  y ~ weight + half_armspan + sf_triceps + sex,
  data = data
)

# Get model statistics
coeffs <- map(summary(m), "coefficients") |>
  map(~ .x[, 1]) |>
  map(~ t(t(.x))) |>
  map(round, 5)
r2 <- map(summary(m), "r.squared") |> map(round, 2)
adj_r2 <- map(summary(m), "adj.r.squared") |> map(round, 2)
resid_SE <- map(summary(m), "sigma") |> map(round, 2)
# Pillai's trace
Manova(m)

# PRESS cross-validation
press_m <- press(m) |> round(2)
q2_m <- q2(m) |> round(2)
se_m <- se_press(m) |> round(2)

# Save model
save(m, file = here("output", "model.rda"))
