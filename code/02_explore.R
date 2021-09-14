# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(mvnormtest)
library(lvmisc)
library(broman)

# Load data ---------------------------------------------------------------

load(here("data", "data.rda"))

# Normality tests ---------------------------------------------------------

# Dependent variables
mshapiro.test(t(cbind(data$bmc, data$fm, data$alst)))

# Independent variables
ind_vars <- names(data)[-c(1, 3, 33:35)]
map(ind_vars, ~ shapiro.test(data[[.x]])) |>
  set_names(ind_vars)

# Check for differences between sexes -------------------------------------

t.test(bmc ~ sex, data = data)
t.test(fm ~ sex, data = data)
t.test(alst ~ sex, data = data)

# Descriptive statistics --------------------------------------------------

data <- data |>
  mutate(bmi = bmi(weight, height / 100))
descriptives <- data |>
  summarise(
    across(
      where(is.double),
      list(min = min, max = max, mean = mean, sd = sd),
      .names = "{.fn}_{.col}"
    )
  )
l <- list("min_", "max_", "mean_", "sd_")
descriptives <- map(
  l,
  ~ descriptives |>
      select(starts_with(.x)) |>
      t() |>
      as_tibble(rownames = "Variable") |>
      select(V1)
) |>
  map_dfc(cbind) |>
  as_tibble() |>
  select(Min = `V1...1`, Max = `V1...2`, Mean = `V1...3`, SD = `V1...4`) |>
  add_column(
    data |>
    select(where(is.double)) |>
    names() |>
    as.data.frame(),
    .before = 1
  ) |>
  mutate(
    SE = SD / sqrt(nrow(data)),
    lower_ci = Mean - SE * qt(0.975, df = nrow(data) - 1),
    upper_ci = Mean + SE * qt(0.975, df = nrow(data) - 1),
    `Min-Max` = paste0(myround(Min, 1), " - ", myround(Max, 1)),
    `95% CI` = paste0(myround(lower_ci, 1), " to ", myround(upper_ci, 1)),
    Mean = myround(Mean, 1),
    SD = myround(SD, 1)
  ) |>
  select(
    ` ` = `names(select(data, where(is.double)))`,
    `Min-Max`, Mean, SD, `95% CI`
  )

# Reorder variables
row_order <- c(1, 33, 32, 31, 2, 3, 34, 29, 30, 4:28)
descriptives <- descriptives[row_order, ] |>
  add_row(
    ` ` = "DXA measures",
    `Min-Max` = "", Mean = "", SD = "", `95% CI` = "",
    .after = 1
  ) |>
  add_row(
    ` ` = "Anthropometrics",
    `Min-Max` = "", Mean = "", SD = "", `95% CI` = "",
    .after = 5
  )

# Rename variables
descriptives[1, 1] <- "Age (years)"
descriptives[3, 1] <- "Appendicular lean soft tissue (kg)"
descriptives[4, 1] <- "Fat mass (kg)"
descriptives[5, 1] <- "Bone mineral content (kg)"
descriptives[7, 1] <- "Height (cm)"
descriptives[8, 1] <- "Weight (kg)"
descriptives[9, 1] <- "Body mass index (kg/m2)"
descriptives[10, 1] <- "Knee height (cm)"
descriptives[11, 1] <- "Half armspan (cm)"
descriptives[12, 1] <- "Subscapular skinfold (mm)"
descriptives[13, 1] <- "Triceps skinfold (mm)"
descriptives[14, 1] <- "Biceps skinfold (mm)"
descriptives[15, 1] <- "Axillary skinfold (mm)"
descriptives[16, 1] <- "Chest skinfold (mm)"
descriptives[17, 1] <- "Suprailiac skinfold (mm)"
descriptives[18, 1] <- "Abdominal skinfold (mm)"
descriptives[19, 1] <- "Thigh skinfold (mm)"
descriptives[20, 1] <- "Calf skinfold (mm)"
descriptives[21, 1] <- "Thoracic circumference (cm)"
descriptives[22, 1] <- "Arm circumference (cm)"
descriptives[23, 1] <- "Forearm circumference (cm)"
descriptives[24, 1] <- "Waist circumference (cm)"
descriptives[25, 1] <- "Abdominal circumference (cm)"
descriptives[26, 1] <- "Hip circumference (cm)"
descriptives[27, 1] <- "Thigh circumference (cm)"
descriptives[28, 1] <- "Calf circumference (cm)"
descriptives[29, 1] <- "Biacromial breadth (cm)"
descriptives[30, 1] <- "Biiliac breadth (cm)"
descriptives[31, 1] <- "Bitrochanteric breadth (cm)"
descriptives[32, 1] <- "Bimalleolar breadth (cm)"
descriptives[33, 1] <- "Cubital breadth (cm)"
descriptives[34, 1] <- "Wrist breadth (cm)"
descriptives[35, 1] <- "Knee breadth (cm)"
descriptives[36, 1] <- "Chest breadth (cm)"

# Save into a csv file
if (!dir.exists(here("output"))) {
  dir.create(here("output"))
}
write_csv(descriptives, here("output", "descriptives.csv"))
