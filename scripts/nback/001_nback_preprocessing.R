# Script name: 001_nback_preprocessing.R
# Project: ED memory
# Script purpose: get dprime
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Thu Dec  8 14:07:39 2022
# Last Modified Date: Thu Dec  8 14:07:39 2022
#
# 👉 

library("here")
library("tidyverse")
library("stringi")


# Run the script twice: 
# one with FLAG <patients>, and one with FLAG <controls>.
FLAG <- "controls"

if (FLAG == "controls") {
  dir <- here("data", "raw", "nback", "controls")
} else {
  dir <- here("data", "raw", "nback", "patients")
}

file_names <- as.character(list.files(path=dir, pattern = "N_BACK_"))
n_files <- length(file_names)
n_files

d_list <- list()
for (i in 1:n_files) {
  
  if (FLAG == "controls") {
    d1  <- read.table(here("data", "raw", "nback", "controls", file_names[i]))
  } else {
    d1  <- read.table(here("data", "raw", "nback", "patients", file_names[i]))
  }
  
  d <- d1 %>% 
    dplyr::rename(
      block = V1,
      trial = V2,
      is_signal = V3,
      is_correct_resp = V4,
      # V5 useless
      miss = V6, # Miss
      fa = V7, # False Alarm
      rt = V8,
      # V9 useless
      current_letter = V10,
      # V11 useless
      previous_letter = V12
    )
  
  d$subj_name <- file_names[i]
  
  d_list[[i]] <- d
}

# convert list into data.frame
df <- do.call(rbind.data.frame, d_list)

df$letter_lag2 <- dplyr::lag(df$current_letter, n = 2L)
df$say_signal <- ifelse(df$rt < 3000, 1, 0)

df %>%
  dplyr::filter(rt != 3000) %>% 
  group_by(subj_name) %>%
  summarise(
    avg_rt = mean(rt, trim = 0.1)
  )

sdt <- df %>% 
  mutate(type = "hit",
         type = ifelse(is_signal==1 & say_signal==0, "miss", type),
         type = ifelse(is_signal==0 & say_signal==0, "cr", type),  # Correct rejection
         type = ifelse(is_signal==0 & say_signal==1, "fa", type)) |>   # False alarm
  dplyr::select(-c(V5, V9, V11))

if (FLAG == "controls") {
  saveRDS(
    sdt,
    here("data", "processed", "nback", "controls", "controls_nback_sdt.Rds")
  )
} else {
  saveRDS(
    sdt,
    here("data", "processed", "nback", "patients", "patients_nback_sdt.Rds")
  )
}


# eof ----



