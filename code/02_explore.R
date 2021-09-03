# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(mvnormtest)

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
  )
