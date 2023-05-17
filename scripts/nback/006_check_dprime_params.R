# Script name: 003_check_dprime_params.R
# Project: ED memory
# Script purpose: check relation d' and dDDMrl params.
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Thu Dec  8 14:21:12 2022
# Last Modified Date: Thu Dec  8 14:21:12 2022
#
# 👉 


# Get parameters.

params_df <- readRDS(
  here::here(
    "data", "processed", "prl", "prl_params_and_subj_code_2022_12_08.rds"
  )
)

length(unique(params_df$subj_code))
# [1] 302


# Get n-back dprime.
dprime_df <- readRDS(
  here::here(
    "data", "processed", "nback", "dprime_nback.rds"
  )
) |> 
  dplyr::rename(
    subj_code = subj_name
  )
dprime_df$psychtoolkit_code <- NULL

length(unique(dprime_df$subj_code))

# To be removed.
bad_subjects <- setdiff(unique(dprime_df$subj_code), unique(params_df$subj_code))

dprime_clean_df <- dprime_df[!(dprime_df$subj_code %in% bad_subjects), ]
length(unique(dprime_clean_df$subj_code))


df <- left_join(params_df, dprime_clean_df, by = "subj_code")
df <- df[!is.na(df$dprime), ]

diagnosis <- rio::import(
  here::here(
    "data", "raw", "diagnostic_categories.xlsx"
  )
)

diag_cat <- diagnosis |> 
  dplyr::select(subj_code, diagnosis)

df1 <- left_join(df, diag_cat, by = "subj_code")
df1$diagnosis <- ifelse(is.na(as.character(df1$diagnosis)), "HC", df1$diagnosis)
df1$diagnosis <- factor(df1$diagnosis)
summary(df1$diagnosis)


df1 |> 
  #dplyr::filter(diagnosis == "BN") |> 
  ggplot(aes(x = dprime, y = a)) + 
  geom_point() + 
  facet_wrap( ~ diagnosis)


df1 |> 
  ggplot(aes(x = dprime, y = alpha_pos)) + 
  geom_point() + 
  facet_wrap( ~ diagnosis)

df1 |> 
  ggplot(aes(x = dprime, y = alpha_neg)) + 
  geom_point() + 
  facet_wrap( ~ diagnosis)

plot(df1$alpha_neg, df1$alpha_pos)

# eof ---

