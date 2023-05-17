# Script name: name
# Project: project
# Script purpose: purpose
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: date_created
# Last Modified Date: date_modified
#
# 👉 


library("here")
library("tidyverse")
library("stringi")
library("brms")
library("sjPlot")
library("tidyLPA")


options(max.print=999999)


params_prl <- readRDS(
  here::here(
    "data", "processed", "_prl", "prl_params_cleaned", 
    "prl_params_and_subj_code_2023_03_15.rds"
  )
)

dprime_df <- readRDS(
  here::here(
    "data", "processed", "nback", "dprime", "nback_dprime.Rds"
  )
) |> 
  dplyr::rename(
    psychtoolkit_code = subj_name
  )

demo_info_df <- readRDS(
  here::here(
    "data", "processed", "nback", "dprime", "demo_info.Rds"
  )
)

# Merge dprime_df and demo_info_df
dpr_df <- left_join(
  dprime_df, demo_info_df, by = "psychtoolkit_code"
) |> 
  dplyr::rename(
    subj_code = subj_name
  )

# Merge dpr and prl params
df <- inner_join(
  dpr_df, params_prl, by = "subj_code"
)

patients_codes <- c(
  "ma_za_1939_12_04_04_f",  "pa_me_1968_07_10_05_f",  "fa_fa_1974_05_14_03_f", 
  "mi_ca_1952_10_28_012_f", "lo_va_1955_11_25_06_f",  "al_ba_1948_06_19_00_f", 
  "gi_ba_1958_08_17_01_m",  "an_ve_1958_08_15_010_f", "mo_ma_1961_04_20_011_f"
)
# sort(patients_codes)
# sort(unique(df$subj_code))

df$is_patient <- ifelse(
  df$subj_code %in% patients_codes, 1, 0
)


# Stop-signal task

# Demographic info and look-up table
demo_df <- readRDS(
  here::here(
    "data", "processed", "stop_signal", "demo_info.Rds"
  )
)

sst_controls <- readRDS(
  here::here(
    "data", "processed", "stop_signal", "controls", "controls_sst.Rds"
  )
)
sst_controls$is_patient <- 0

sst_patients <- readRDS(
  here::here(
    "data", "processed", "stop_signal", "patients", "patients_sst.Rds"
  )
)
sst_patients$is_patient <- 1


pat_contr_df <- rbind(
  sst_controls, sst_patients
) |> 
  dplyr::rename(
    psychtoolkit_code = subj_code
  )

sst_df <- inner_join(
  pat_contr_df, demo_df, by = "psychtoolkit_code"
) |> 
  dplyr::select(-c(psychtoolkit_code, is_patient, sex, year_of_birth)) |> 
  dplyr::rename(
    subj_code = subj_name
  )


mydat <- left_join(df, sst_df, by = "subj_code")


# lmer -------------------------------------------------------------------------

fm1 <- lmer(
  alpha_pos ~ is_patient * (dprime + sst) + stim + (1 | subj_code),
  mydat
)
car::Anova(fm1)

fm2 <- lmer(
  alpha_neg ~ is_patient * (dprime + sst) + stim + (1 | subj_code),
  mydat
)
car::Anova(fm2)

fm3 <- lmer(
  v ~ is_patient * (dprime + sst) + stim + (1 | subj_code),
  mydat
)
car::Anova(fm3)
summary(fm3)

fm4 <- lmer(
  a ~ is_patient * (dprime + sst) + stim + (1 | subj_code),
  mydat
)
car::Anova(fm4)
summary(fm4)

fm5 <- lmer(
  t ~ is_patient * (dprime + sst) + stim + (1 | subj_code),
  mydat
)
car::Anova(fm5)
summary(fm5)




length(unique(df$subj_code))

hist(df$alpha_pos)

bf_alphap <- bf(alpha_pos ~ is_patient * (dprime + sst) + stim + (1 + stim |p| subj_code))
bf_alphan <- bf(alpha_neg ~ is_patient * (dprime + sst) + stim + (1 + stim |p| subj_code))
fit1 <- brm(
  bf_alphap + bf_alphan + set_rescor(TRUE), 
  data = df, chains = 4
)
summary(fit1)
pp_check(fit1, resp="alphapos")
pp_check(fit1, resp="alphaneg")


df$is_patient <- factor(df$is_patient)

fm1 <- brm(
  bf(
    alpha_pos ~ is_patient * dprime + stim + (1 + stim | subj_code)
  ), 
  backend = "cmdstanr",
  df
)
marginal_effects(fm1, "dprime")
summary(fm1)

fm2 <- brm(
  bf(alpha_neg ~ is_patient * dprime + stim + (1 + stim || subj_code),
     sigma ~ is_patient), 
  backend = "cmdstanr",
  df
  )
summary(fm2)
pp_check(fm2)
marginal_effects(fm2, "dprime:is_patient")


fm2 <- lmer(alpha_neg ~ is_patient*dprime + stim + (1 | subj_code), df)
summary(fm2)

plot_model(fm2, type = "pred", terms = c("dprime", "is_patient"))


fm3 <- lmer(t ~ is_patient*dprime + stim + (1 | subj_code), df)
summary(fm3)

fm4 <- lmer(a ~ is_patient*dprime + stim + (1 | subj_code), df)
summary(fm4)

fm5 <- lmer(v ~ is_patient*dprime + stim + (1 | subj_code), df)
summary(fm5)

m1 <- lmer(alpha_neg ~ is_patient*crit + stim + (1 | subj_code), df)
summary(m1)
m2 <- lmer(alpha_pos ~ is_patient*crit + stim + (1 | subj_code), df)
summary(m2)
m3 <- lmer(v ~ is_patient*crit + stim + (1 | subj_code), df)
summary(m3)
m4 <- lmer(a ~ is_patient*crit + stim + (1 | subj_code), df)
summary(m4)



bysubj_df <- mydat |> 
  group_by(subj_code, is_patient) |> 
  summarize(
    dpr = mean(dprime),
    t = mean(t),
    v = mean(v),
    a = mean(a),
    apos = mean(alpha_pos),
    aneg = mean(alpha_neg),
    sst = mean(sst)
  ) |> 
  ungroup()

bysubj_df <- bysubj_df[!is.na(bysubj_df$sst), ]

bysubj_df %>%
  select(dpr, t, a, v, aneg, apos, sst) %>%
  scale() %>%
  estimate_profiles(2) |> 
  plot_profiles()

mm <- bysubj_df %>%
  select(dpr, t, a, v, aneg, apos, sst) %>%
  scale() %>%
  estimate_profiles(2)

out <- get_data(mm)

bysubj_df$class <- out$Class

table(bysubj_df$is_patient, bysubj_df$class)

