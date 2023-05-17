# Script name: 005_get_subj_code.R
# Project: hoarding
# Script purpose: get subj_code and psytoolkit code
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Thu Dec  8 14:19:38 2022
# Last Modified Date: Thu Dec  8 14:19:38 2022
#
# 👉 


library("here")
library("tidyverse")
library("stringi")

options(max.print=999999)

source(here::here("scripts", "functions", "funs_prl.R"))


# CONTROLS
excel_controls <- rio::import(
  here::here("data", "raw", "stop_signal", "controls", "data.xlsx")
)
subj_code_controls_df <- gen_subj_name(excel_controls)

temp_controls <- tibble(
  psychtoolkit_code = subj_code_controls_df$code_psytoolkit,
  subj_name = subj_code_controls_df$subj_id,
  sex = subj_code_controls_df$sex,
  year_of_birth = subj_code_controls_df$`anno:1`
)


# PATIENTS
excel_patients <- rio::import(
  here::here("data", "raw", "stop_signal", "patients", "data.xlsx")
)
subj_code_patients_df <- gen_subj_name(excel_patients)

temp_patients <- tibble(
  psychtoolkit_code = subj_code_patients_df$code_psytoolkit,
  subj_name = subj_code_patients_df$subj_id,
  sex = subj_code_patients_df$sex,
  year_of_birth = subj_code_patients_df$`anno:1`
)

demo_info <- rbind(
  temp_controls, temp_patients
)


saveRDS(
  demo_info,
  here::here(
    "data", "processed", "stop_signal", "demo_info.Rds"
  )
)


# eof -----
