# Script name: 002_get_dprime.R
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

options(max.print=999999)


controls_df <- readRDS(
  here::here(
    "data", "processed", "nback", "controls", "controls_nback_sdt.Rds"
  )
)
controls_df$is_patient <- 0

patients_df <- readRDS(
  here::here(
    "data", "processed", "nback", "patients", "patients_nback_sdt.Rds"
  )
)
patients_df$is_patient <- 1

# Merge dfs
df <- rbind(patients_df, controls_df)

# Wide format for type.
sdt2 <- df %>% 
  group_by(subj_name, type) %>% 
  summarise(count = n()) %>% 
  spread(type, count)  # Format data to one row per person

# data.frame(sdt2$hit, sdt2$fa, sdt2$miss, sdt2$cr)

sdt2 <- replace(sdt2, is.na(sdt2), 0)
sdt2$fa <- ifelse(sdt2$fa == 0, 0.025, sdt2$fa)
sdt2$miss <- ifelse(sdt2$miss == 0, 0.025, sdt2$miss)


sdt2 <- sdt2 %>% 
  mutate(zhr = qnorm(hit / (hit+miss)),
         zfa = qnorm(fa / (fa+cr)),
         dprime = zhr - zfa,
         crit = -zfa)

btsubj_crit = df |> 
  group_by(subj_name) |> 
  summarise(
    avg_say_signal = mean(say_signal)
  )

sdt_tot <- left_join(sdt2, btsubj_crit, by="subj_name")

plot(sdt_tot$avg_say_signal, sdt_tot$crit)
cor(sdt_tot$avg_say_signal, sdt_tot$crit)

# sdt_tot$crit is a measure of the bias towards saying "signal absent".


glmfit <- 
  brm(say_signal ~ is_signal +
        (1 + is_signal | subj_name), 
      family = bernoulli(link = "probit"), 
      data = df,
      backend = "cmdstan", 
      cores = 4
  )

summary(glmfit)
recoef <- ranef(glmfit)
out_glmfit <- summary(glmfit)

dpr_h <- as.numeric(recoef$subj_name[, , 2][, 1]) + out_glmfit$fixed[2, 1]


dpr <- sdt2$dprime
plot(dpr_h, dpr)
abline(0, 1)

dprime_estimates <- data.frame(
  subj_name = row.names(recoef$subj_name[, , 2]),
  dprime = dpr_h
)

crit_df <- data.frame(
  crit=sdt_tot$crit, subj_name=sdt_tot$subj_name
)

dprime_estimates_df <- left_join(
  dprime_estimates, crit_df, by="subj_name"
)

  saveRDS(
    dprime_estimates_df,
    here("data", "processed", "nback", "dprime", "nback_dprime.Rds")
  )

# The two previous files include, for each subject,
# - subj_name, the psychtoolkit code
# - dprime


# Comparison between patients and controls ----


glmfit2 <- 
  brm(say_signal ~ is_signal * is_patient +
        (1 + is_signal | subj_name), 
      family = bernoulli(link = "probit"), 
      data = df,
      backend = "cmdstan", 
      cores = 4
  )

summary(glmfit2)
# Population-Level Effects: 
#                      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept               -1.54      0.08    -1.71    -1.39 1.00     1304     1874
# is_signal                2.23      0.15     1.94     2.53 1.01      935     1689
# is_patient               0.10      0.19    -0.27     0.48 1.00     1322     2011
# is_signal:is_patient    -0.44      0.35    -1.13     0.22 1.00      923     1396



# eof -----


