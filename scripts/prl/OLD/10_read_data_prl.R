# Prelims
library("here")
library("tidyverse")
library("stringi")


# is_odd <- function(x) 
#   x %% 2 != 0

dir <- here("data", "raw", "_prl", "patients", "neutral", "shape")

file_names <- as.character(list.files(path=dir, pattern = "PRL_"))
n_files <- length(file_names)
n_files

d_list <- list()

for (i in 1:n_files) {
  
  d  <- read.table(here("data", "raw", "_prl", "patients", "neutral", "shape", file_names[i]))
  
  d$subj_idx <- file_names[i]
  d$epoch <- d$V1
  d$target_position <- d$V2
  d$stimulus_type <- d$V3
  d$stimulus_class <- d$V4
  d$which_image_is_rewarded_more <- d$V5
  d$image_category <- d$V6
  d$keypress <- d$V7 # (1=a, 2=l)
  d$rt <- d$V8
  d$feedback <- d$V9 # 1 = reward, 2 = punishment, 3 = too slow
  d$V10 <- NULL
  # on each trial, a food picture and a neutral image
  # the food/social image is coded as image2
  d$is_target_img_rewarded_in_first_epoch <- ifelse(
    d$V5 == "image1_rewarded7", "no", "yes"
  )
  
  d$epoch <- as.numeric(stri_sub(d$V1, 5, 5))
  # d$V1 <- NULL
  
  d$is_target_rewared_in_present_epoch <- rep(c(0, 1, 0, 1), each = 40)
  
  # V2: position of target image (sad face)
  # where is the target img?
  d$position_target_img <- stri_sub(d$V2, 8, 9)
  
  d$stimulus_type <- d$stimulus_type %>%
    recode(
      "forme" = "shape"
  )
  # V7 key pressed by participant
  # 1 : "a" -> sx
  # 2 : "l" -> dx
    
  # participant has chosen the image to the right or to the left
  d$resp <- ifelse(
    d$V7 == 1, "sx",
    ifelse(d$V7 == 2, "dx", "ERROR")
  )

  d$is_target_img_chosen <- ifelse(
    d$resp == d$position_target_img, 1, 0
  )

  d$is_positive_feedback <- case_when(
    d$V9 == 1 ~ "yes",
    d$V9 == 2 ~ "no",
    d$V9 == 3 ~ "no response"
  )
  
  d$trial <- 1:160
  
  d_list[[i]] <- d
  
}

# convert list into data.frame
df <- do.call(rbind.data.frame, d_list)

# save data in RDS files
saveRDS(df, here("data", "processed", "_prl", "patients", "neutral", "shape", "data_prl_wrong_subj_code.rds"))


# E N D ----




# 
# 
# 
# 
# df %>% 
#   # dplyr::filter(block < 3) %>% 
#   group_by(target_epoch_relation, epoch) %>% 
#   summarise(
#     m = mean(is_target_img_chosen)
#   )
# 
# 
# df$feedback <- ifelse(df$is_positive_feedback == "yes", 1, 0)
# 
# df %>% 
#   group_by(block) %>% 
#   summarise(
#     avg_feedback = mean(feedback, na.rm = TRUE)
#   )
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# df %>% 
#   group_by(epoch) %>% 
#   summarise(
#     m = mean(is_target_img_rewarded == "yes")
#   )
# 
# 
# df %>% 
#   group_by(epoch) %>% 
#   summarise(
#     m = mean(is_positive_feedback == "yes")
#   )
# 
# df$t <- rep(1:40, 12)
# 
# 
# 
# out <- df %>% 
#   group_by(t, epoch) %>% 
#   summarise(
#     m = mean(is_positive_feedback == "yes")
#   )
# 
# plot(1:160, out$m, type = 'l')
# 
# 








