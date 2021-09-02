# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(readxl)
 
# Read data ---------------------------------------------------------------

data <- read_xlsx(here("data", "raw_data.xlsx")) |>
  select(
    # Rename variables
    subj = Id, age = Idade, sex = Sexo,
    height = Estatura_cm, weight = Peso_kg,
    # Rename skinfold variables
    sf_subescapular = DC_Se_mediana,
    sf_triceps = DC_Tr_mediana,
    sf_biceps = DC_Bi_mediana,
    sf_axilar = DC_Ax_mediana,
    sf_chest = DC_Pt_mediana,
    sf_supraliac = DC_Si_mediana,
    sf_abdomnal = DC_AbV_mediana,
    sf_thigh = DC_Cx_mediana,
    sf_calf = DC_Pn_mediana,
    # Rename circumference variables
    circ_thorax = P_Tx_mediana,
    circ_arm = P_Br_dir_mediana,
    circ_forearm = P_AntBr_mediana,
    circ_waist = P_Cin_mediana,
    circ_abdominal = P_Ab_mediana,
    circ_hip = P_Qd_mediana,
    circ_thigh = P_Cx_mediana,
    circ_calf = P_Pn_mediana,
    # Rename breadth variables,
    br_biacromial = DO_Biacromial,
    br_biiliac = DO_Biiliaco,
    br_bitrochanteric = DO_Bitrocanterico,
    br_bimalleolar = DO_Bimaleolar,
    br_cubital = DO_Cubito,
    br_wrist = DO_Pulso,
    br_knee = DO_Joelho,
    br_chest = DO_Peito,
    # Rename other variables
    knee_height = Alt_Joelho_mediana,
    half_armspan = Meia_envergadura_mediana,
    # Rename dependent variables
    bmc = BMC_kg,
    fm = fat_mass_kg,
    alst = ALST_semBMC
  ) |>
  mutate(
    sex = recode_factor(
      sex, `1` = "Male", `2` = "Female"
    )
  )

# Export data as rda ------------------------------------------------------

save(data, file = here("data", "data.rda"))
