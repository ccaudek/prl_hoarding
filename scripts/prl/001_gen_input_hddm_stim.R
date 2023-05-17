# Script name:001_gen_input_hddm_stim.R
# Project: Hoarding Project, AUSL Bologna 
# Script purpose: functions to be used
# @author: Ilaria Colpizzi
# Date Created: Gen 5 10:48 2023
# Last Modified Date: Gen 5 10:48 2023

# Prelims
suppressPackageStartupMessages({
  library("here")
  library("tidyverse")
  library("stringi")
  library("readxl")
})

# Increase max print
options(max.print = .Machine$integer.max)

# source(here("code", "functions", "funs_gen_data_for_hddm.R"))
source(here::here("scripts", "functions", "funs_prl.R"))


# 1. Merge psytoolkit PRL files  ------------------------------------------

load_psychtoolkit_files()


# 2.  Create data list ----------------------------------------------------

data_list <- write_prl_raw_data_list()


# 3. Corrected subj_name --------------------------------------------------

# For each element of the list, we correct the subject's identifier.

d_list <- correct_subj_names(data_list)
# The returned list has the same structure as data_list. 

# 4. Clean and save single file --------------------------------------------

binding_cleaned_data_frames(d_list)

# 5. Create input for HDDMrl ----------------------------------------------

write_input_for_hddmrl()


# add subject conde to hddm params ----------------------------------------

add_subj_code_to_hddm_params_and_save()

