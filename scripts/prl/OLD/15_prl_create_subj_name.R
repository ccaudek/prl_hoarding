library("here")
library("tidyverse")
library("readxl")
library("missForest")


gen_subj_name <- function(d) {
  
  library("stringi")
  
  d$mese_c <- ifelse(
    d$`mese:1` < 10, stri_join("0", as.character(d$`mese:1`), sep=''), as.character(d$`mese:1`)
  )
  
  d$giorno_c <- ifelse(
    d$`giorno:1` < 10, 
    stri_join("0", as.character(d$`giorno:1`), sep=''), 
    as.character(d$`giorno:1`)
  )
  
  d$cellulare_c <- ifelse(
    d$`cellulare:1` < 100, 
    stri_join("0", as.character(d$`cellulare:1`), sep=''), 
    as.character(d$`cellulare:1`)
  )
  
  d$sex <- ifelse(d$`sesso:1` == 1, "f",
                  ifelse(d$`sesso:1` == 2, "m", NA))
  
  d$subj_id <- tolower(
    stri_join(d$`nome:1`, d$`cognome:1`, d$`anno:1`, 
              d$mese_c, d$giorno_c, d$cellulare_c, d$sex, 
              sep='_')
  )
  
  d$code_psytoolkit <- d$`esperimento:1`
  
  d
  
}


# task 1: generate subj_name

## the following operations must be repeated for both patients and controls.

## food

d_prl <- readxl::read_excel(here("data", "raw", "prl", "patients", "neutral", "shape", "data.xlsx"))
d_prl <- gen_subj_name(d_prl)
# d_prl$stimulus <- "food"

d_clean <- d_prl %>% 
  dplyr::rename("subj_name" = "subj_id") %>% 
  dplyr::select(subj_name, code_psytoolkit)

d_clean2 <- d_clean[!is.na(d_clean$code_psytoolkit), ] 
d_clean2$code_psytoolkit

data_wrong_code <- readRDS(
  here("data", "processed", "prl", "patients", "neutral", "shape", "data_prl_wrong_subj_code.rds")
  ) %>% 
  dplyr::rename("code_psytoolkit" = "subj_idx")

df <- inner_join(
  d_clean2, data_wrong_code, 
  by = "code_psytoolkit"
)
length(unique(df$subj_name))


# new section -------------------------------------------------------------

  prl_data <- df

  prl_data$feedback <- ifelse(
    prl_data$feedback == 2, 0, prl_data$feedback
  )
  prl_data$feedback <- ifelse(prl_data$feedback == 3, NA, prl_data$feedback)
  
  prl_data$feedback <- 
    ifelse(prl_data$rt < 150 | prl_data$rt > 2499, NA, prl_data$feedback)
  
  prl_data$rt1 <- 
    ifelse(prl_data$rt < 150 | prl_data$rt > 2499, NA, prl_data$rt)
  
  prl_data$rt1 <- ifelse(prl_data$trial == 1, NA, prl_data$rt1)

  prl_data$response <- prl_data$is_target_img_chosen
  
  prl_data$is_rich_choice <- case_when(
    prl_data$is_target_rewared_in_present_epoch & prl_data$is_target_img_chosen ~ 1,
    !prl_data$is_target_rewared_in_present_epoch & !prl_data$is_target_img_chosen ~ 1,
    TRUE ~ 0
  )
  
  # Multiple imputation on NAs.
  temp <- data.frame(
    rt1         = prl_data$rt1, 
    trial       = prl_data$trial,
    feedback    = prl_data$feedback, 
    rich_choice = prl_data$is_rich_choice,
    response    = prl_data$response
  )
  
  # Imputes the "best value" according to the linear regression model, also 
  # known as regression imputation.
  imp <- mice::mice(temp, method = "norm.predict", m = 1) 
  temp <- complete(imp)
  
  prl_data$feedback <- ifelse(temp$feedback > 0.5, 1, 0)
  prl_data$rt <- temp$rt1 / 1000
  
  # I want to number each subject in the data.frame so that subjects are 
  # ordered sequentially, according to the order they appear in the data frame. 
  # https://community.rstudio.com/t/why-does-group-indices-use-alphabetical-ordering/5452
  # As suggested in the above link, I wrap group_indices in another function:
  grpid = function(x) match(x, unique(x))
  # then
  d1 <- prl_data %>% 
    mutate(subj_idx = group_indices(., subj_name) %>% grpid)
  # In this manner, the variable subj_idx assigns an integer to each subject;
  # this integer is ordered according to the sequence in which the subjects are 
  # present in the data.frame.
  # table(d3$subj_idx)
  # unique(d3$subj_idx)
  
  d1$stimulus_type <- d1$stimulus_type %>% 
    forcats::fct_recode(
      neutral = "forme"
    )
  
  d1 <- d1 %>% 
    dplyr::rename(
      stim = stimulus_type
    )
  
  df_for_hddm <- data.frame(
    subj_idx   = d1$subj_idx,
    response   = d1$response,
    stim       = d1$stim,
    rt         = d1$rt,
    trial      = d1$trial,
    feedback   = d1$feedback,
    subj_code  = d1$subj_name,
    q_init     = 0.5
  )
  
# save --------------------------------------------------------------------

  
  rio::export(
    df_for_hddm, 
    here("data", "processed", "prl", "patients", "neutral", "shape", "hddm_input.csv")
  )
  
  lut <- data.frame(
    subj_idx = mydat$subj_idx, 
    subj_code = mydat$subj_code
  )
  
  rio::export(
    lut, 
    here("data", "processed", "prl", "data_for_hddm", "hddm_look_up_table.csv")
  )
  

# data_processed ----------------------------------------------------------

neut1 <- rio::import(
  here("data", "processed", "prl", "patients", "neutral", "shape", "hddm_input.csv")
)

  neut2 <- rio::import(
    here("data", "processed", "prl", "patients", "neutral", "hddm_input.csv")
  )

  
neut <- rbind(neut1, neut2)  
  
rio::export(
neut,
  here("data", "processed", "prl", "patients", "neutral", "finale", "input_hddm.csv")
)


neut <- rio::import(
  here("data", "processed", "prl", "patients", "neutral", "finale", "input_hddm.csv")
)

obj <- rio::import(
  here("data", "processed", "prl", "patients", "hoarding", "hddm_input.csv")
)

input <- full_join(obj, neut)

## split-by 
input$split_by <- ifelse(input$stim == "neutral", 0, 1)


grpid = function(x) match(x, unique(x))
# then
input1 <- input %>% 
  mutate(subj_idx = group_indices(., subj_code) %>% grpid)

mydat <- input1 %>% 
  dplyr::arrange(subj_idx, trial, split_by)

rio::export(
  mydat,
  here("data", "processed", "prl", "patients", "hddm_input_tot.csv")
)


library(dplyr)
neut %>% 
  group_by(subj_code) %>% 
  summarise(feedback = sum(feedback))

obj %>% 
  group_by(subj_code) %>% 
  summarise(feedback = sum(feedback))




# ## social
# 
# d_social <- readxl::read_excel(here("data", "raw", "prl", "patients", "other_social", "data.xlsx"))
# d_social <- gen_subj_name(d_social)
# d_social$stimulus <- "social"
# 
# d_social_clean <- d_social %>% 
#   dplyr::rename("subj_name" = "subj_id") %>% 
#   dplyr::select(subj_name, code_psytoolkit, stimulus)
# 
# d_social_clean2 <- d_social_clean[!is.na(d_social_clean$code_psytoolkit), ] 
# d_social_clean2$code_psytoolkit
# 
# data_social_wrong_code <- readRDS(
#   here("data", "processed", "prl", "social", "patients_data_social_wrong_subj_code.rds")
#   ) %>% 
#   dplyr::rename("code_psytoolkit" = "subj_idx")
# 
# df_social <- inner_join(
#   d_social_clean2, data_social_wrong_code, 
#   by = "code_psytoolkit"
# )

# combined_dat <- rbind(df_food, df_social)
# length(unique(combined_dat$subj_name))

# task 2: generate file to be used as input for hDDM

combined_dat <- df

combined_dat$subj_idx <- as.numeric(
  factor(as.character(combined_dat$subj_name))
  ) - 1
sort(unique(combined_dat$subj_idx))

# response: which image has been chosen in each trial
combined_dat$response <- combined_dat$is_target_img_chosen

combined_dat$rt <- combined_dat$rt / 1000

combined_dat$trial <- rep(0:159, nrow(combined_dat) / 160)

combined_dat$feedback <- ifelse(
  combined_dat$feedback == 2, 0, combined_dat$feedback
)

combined_dat$feedback <- ifelse(combined_dat$feedback == 3, NA, combined_dat$feedback)

# imputation

# tobe_imputed <- combined_dat
# tobe_imputed$feedback_f <- as.factor(tobe_imputed$feedback)
# tobe_imputed$feedback <- NULL
# 
# tobe_imputed <- tobe_imputed %>% 
#   dplyr::select(
#     subj_name, stimulus_type, 
#     rt, #is_target_img_rewarded_in_first_epoch,
#     #is_target_rewared_in_present_epoch, resp, trial, 
#     feedback_f
#   )
# 
# 
# tobe_imputed <- as.data.frame(tobe_imputed)
# 
# tobe_imputed$rt <- ifelse(
#   tobe_imputed$rt < 0.1 | tobe_imputed$rt > 2.49 | 
#     is.na(tobe_imputed$feedback_f), NA, tobe_imputed$rt)
# summary(tobe_imputed$rt)
# 
# 
# temp <- tobe_imputed %>% 
#   dplyr::select(rt, stimulus_type, feedback_f)
# 
# 
# temp_imp <- missForest(temp, verbose = TRUE)
# 
# 
# ## Impute missing values providing the complete matrix for
# ## illustration. Use 'verbose' to see what happens between iterations:
# tobe_imputed_imp <- missForest(tobe_imputed, verbose = TRUE)



# for (i in 1:nrow(combined_dat)) {
#   if (combined_dat$feedback[i] == 3) {
#     coin <- runif(1)
#     if (coin < 0.5)
#       combined_dat$feedback[i] <- 1
#     else
#       combined_dat$feedback[i] <- 0
#   }
# }


ddm_dat <- combined_dat %>% 
  dplyr::select(
    subj_idx,	response,	stimulus_type,	rt,	trial,	feedback
  ) %>% 
  dplyr::rename(
    "stim" = "stimulus_type"
  )

ddm_dat$q_init = 0.5

ddm_dat <- ddm_dat %>% 
  arrange(subj_idx)

ddm_dat$rt <- ifelse(ddm_dat$rt < 0.1 | ddm_dat$rt == 2.5, NA, ddm_dat$rt)




imp <- mice::mice(ddm_dat, method = "norm.predict", m = 1) # Impute data
ddm_dat_imp <- complete(imp) # Store data

# TODO: add a `group` column with levels `control` and `patient`.

hist(ddm_dat$rt)


# d$is_patient <- 0
# d$is_patient <- ifelse(d$subj_idx == "ro.gi.1965.08.07.292.f", 1, d$is_patient)
# d$is_patient <- ifelse(d$subj_idx == "vi.va.1999.12.15.158.f", 1, d$is_patient)

ddm_dat_imp$split_by <- as.numeric(
  factor(as.character(ddm_dat_imp$stim))
) - 1


# negative RTs for response == 0
ddm_dat_imp$rt <- ifelse(
  ddm_dat_imp$response == 0, -ddm_dat_imp$rt, ddm_dat_imp$rt
)

# save data in CSV files
rio::export(ddm_dat_imp, here("data", "processed", "reappraisal", "data_for_hddm.csv"))


# Third task

lu_tbl <- combined_dat %>% 
  dplyr::select(subj_idx,	subj_name) 

deduped_tbl <- unique(lu_tbl[, 1:2])


rio::export(deduped_tbl, here("data", "processed", "reappraisal", "look_up_table.csv"))

# e  n  d  ---------------------------------------------------------------------

















temp$subj_idx <- factor(temp$subj_idx)


df <- inner_join(temp, lookup_table, by = "subj_idx")

length(unique(df$subj_name))
















lookup_table <- data.frame(
  subj_idx = d$subj_idx, 
  subj_name = d$subj_id
)

lookup_table$subj_name <- ifelse(
  lookup_table$subj_name == "cristina_cinque_1999_08_21_931_f", 
  "cr_ci_1999_08_21_931_f", as.character(lookup_table$subj_name)
)

temp <- readRDS(here("data", "processed", "prl", "food", "data_wrong_subj_code.rds"))
temp$subj_idx <- factor(temp$subj_idx)


df <- inner_join(temp, lookup_table, by = "subj_idx")

length(unique(df$subj_name))



  
foo <- unique(df$subj_name) 
subj_idx <- as.numeric(factor(as.character(foo))) - 1

correspondence_table <- data.frame(
  subj_name = foo,
  subj_idx = subj_idx
)

saveRDS(correspondence_table, 
        here("data", "processed", "prl", "food", "food_lookup_table_subjidx_subjname.rds"))









