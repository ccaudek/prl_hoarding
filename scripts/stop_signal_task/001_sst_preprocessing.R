# Script name: 001_sst_preprocessing.R
# Project: ED memory
# Script purpose: 
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Thu Dec  8 14:07:39 2022
# Last Modified Date: Thu Dec  8 14:07:39 2022
#
# 👉 

library("here")
library("tidyverse")
library("stringi")

# https://cran.r-project.org/web/packages/splithalfr/vignettes/sst_ssrti.html
# Scoring method by Logan (1981).
fn_score <- function(ds) {
  # Mean SSD
  mean_ssd <- mean(ds[ds$condition == 1, ]$ssd)
  # Proportion of failed nogos
  p_failed_nogo <- 1 - mean(ds[ds$condition == 1, ]$response)
  # Go RTs
  go_rts <- ds[
    ds$condition == 0 &
      ds$rt > 0,
  ]$rt
  # n-th percentile of Go RTs
  rt_quantile <- quantile(go_rts, p_failed_nogo, names = FALSE)
  # SSRTi
  return(rt_quantile - mean_ssd)
}



# Run the script twice: 
# one with FLAG <patients>, and one with FLAG <controls>.
FLAG <- "controls"

if (FLAG == "controls") {
  dir <- here("data", "raw", "stop_signal", "controls")
} else {
  dir <- here("data", "raw", "stop_signal", "patients")
}

file_names <- as.character(list.files(path=dir, pattern = "STOP_SIGNAL_"))
n_files <- length(file_names)
n_files

d_list <- list()
for (i in 1:n_files) {
  
  if (FLAG == "controls") {
    d1  <- read.table(here("data", "raw", "stop_signal", "controls", file_names[i]))
  } else {
    d1  <- read.table(here("data", "raw", "stop_signal", "patients", file_names[i]))
  }
  
  # trial. Trial number
  # ssd. Stop signal delay
  # condition. 0 = go, 1 = stop
  # response. Correct (1) or incorrect (0)
  # rt. Reaction time (milliseconds)
  # participant. Participant ID
  
  d <- d1 %>% 
    dplyr::rename(
      cond = V1, # must be recoded
      arrow_direction = V2,
      ssd = V3, # 0: go, ssd in trials nogo
      rt_before_ss = V4, # reaction times before the stop signal
                         # this is NOT the complete rt!
      response_status_to_arrow_stim = V5,
         # 3: no response; 
         # 1: correct resp to arrow direction; 
         # 2 wrong resp. to arrow direction
      rt_after_ss = V6,
         # it has a value different from 0 when the subject make a keypress
         # after the stop signal
      response_status_in_nogo_trials = V7,
         # 0 in go trials; 1 correct response to arrow direction in nogo trials
         # when the subject was asked not to respond; 3: no response
         # in nogo trials 
      response = V8 # 1: correct in both go and nogo trials, 0: wrong either in
                    # go or nongo trials.
    )
  
  d$trial <- 1:nrow(d)
  d$condition <- ifelse(d$cond == "go", 0, 1)
  
  d$rt <- case_when(
    d$response_status_in_nogo_trials == 3    ~ 0,
    d$rt_before_ss == d$ssd                  ~ d$ssd + d$rt_after_ss,
    d$rt_before_ss < d$ssd                   ~ d$rt_before_ss,
    d$rt_before_ss + d$rt_after_ss == 500    ~ 0,
    d$ssd == 0                               ~ d$rt_before_ss,
    .default = 999999
  )
  
  d$subj_name <- file_names[i]
  
  d$participant <- i
  
  d_list[[i]] <- d
}

# convert list into data.frame
df <- do.call(rbind.data.frame, d_list)


ds_sst <- df |> 
  dplyr::select(
    trial, 
    ssd, 
    condition, 
    response, 
    rt, 
    participant
  )

# remove first trial.
ds_sst <- ds_sst[ds_sst$trial > 1, ]

# fn_score(subset(ds_sst, participant == 1))

scores <- by(
  ds_sst,
  ds_sst$participant,
  fn_score
)

sst_df <- data.frame(
  subj_code = file_names,
  sst = scores
)


if (FLAG == "controls") {
  saveRDS(
    sst_df,
    here("data", "processed", "stop_signal", "controls", "controls_sst.Rds")
  )
} else {
  saveRDS(
    sst_df,
    here("data", "processed", "stop_signal", "patients", "patients_sst.Rds")
  )
}


# eof ----



