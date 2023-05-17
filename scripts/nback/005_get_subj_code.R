# Script name: 005_get_subj_code.R
# Project: hoarding
# Script purpose: get subj_code and psytoolkit code
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Thu Dec  8 14:19:38 2022
# Last Modified Date: Thu Dec  8 14:19:38 2022
#
# 👉 

options(max.print=999999)

source(here::here("scripts", "functions", "funs_prl.R"))


# CONTROLS
excel_controls <- rio::import(
  here::here("data", "raw", "nback", "controls", "data.xlsx")
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
  here::here("data", "raw", "nback", "patients", "data.xlsx")
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
    "data", "processed", "nback", "dprime", "demo_info.Rds"
  )
)












# 
# # Read dprime data
# controls_dprime <- readRDS(
#   here("data", "processed", "nback", "controls_nback_dprime_20221208.Rds")
# )
# 
# 
# 
# controls_dprime$psychtoolkit_code <- substr(controls_dprime$subj_name, 25, 60)
# 
# 
# patients_dprime <- readRDS(
#   here("data", "processed", "nback", "patients_nback_dprime_20221208.Rds")
# )
# patients_dprime$psychtoolkit_code <- substr(patients_dprime$subj_name, 34, 69)
# # patients_dprime$subj_name[1:3]
# # patients_dprime$psychtoolkit_code[1:3]
# 
# nback_dprime <- rbind(controls_dprime, patients_dprime)
# nback_dprime$subj_name <- NULL
# 
# 
# # Merge the two dfs.
# 
# dprime_df <- left_join(nback_dprime, lookup_tbl, by = "psychtoolkit_code")
# 
# 
# # Correct subj_code.
# dprime_df$subj_name <- dprime_df$subj_name |> 
#   forcats::fct_recode(
#     # NEW = OLD
#     "gi_to_1997_07_30_762_f" = "giulia_toma_1997_07_30_762_f",
#     "ch_gr_1992_09_24_323_f" = "chiara_gravina_1992_09_24_323_f",
#     "ma_te_2001_05_31_333_m" = "ma a_te_2001_05_31_333_m",
#     "as_mi_1999_03_27_854_f" = "asia_milani_1999_03_27_854_f",
#     "gi_po_2001_05_17_867_f" = "giulia_polichetti_2001_05_17_867_f",
#     "ni_pr_1999_10_16_006_f" = "ni_pr_1999_10_16_06_f",
#     "ch_be_1990_12_20_153_f" = "chiara_benazzi_1990_12_20_153_f",
#     "em_or_2003_01_02_101_f" = "emma_orsucci_2003_01_02_101_f",
#     "fe_sa_2002_05_09_008_f" = "fe_sa_2002_05_09_08_f"
#   )
# 
