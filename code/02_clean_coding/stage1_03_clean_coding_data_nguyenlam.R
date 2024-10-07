################################################################################
# stage1_03_clean_coding_data_nguyenlam.R
#
# This script cleans the raw coding data entered by coder nguyenlam.
#
# The script assumes the raw data exists in the current environment with name
# data_entry_<coder name>_raw (imported by stage1_02_import_coding_data.R).
#
# The code takes the raw data, and makes use of a temporary data frame object
# to perform the cleaning required.

# Load required packages.
library(tidyverse)

# Remove the two demo records from the data set.
# Recode coder's name/initials to format for merging.
data_entry_temp <- data_entry_nguyenlam_raw %>% 
  filter(!(article_id %in% c("demo01", "demo02"))) %>% 
  mutate(coder_initials = recode(coder_initials, nathan = "nguyenlam"))



# Cleaning link_work_noother
# Add a comment for a case where the link did not work (copy from notes field).
data_entry_temp <- data_entry_temp %>% 
  mutate(link_work_noother = ifelse(
    article_id == "a000493"
    & link_work == "no"
    & is.na(link_work_noother) == TRUE,
    notes,
    link_work_noother
  ))



# Cleaning is_repository
# Treat Github as a repository, change from "no" to "yes" for one article.
# Move comment in is_repository_other to repository_name.
data_entry_temp <- data_entry_temp %>% 
  mutate(is_repository = ifelse(
    article_id == "a001131"
    & link_number == 1
    & is_repository == "no"
    & is.na(is_repository_other) == FALSE,
    "yes",
    is_repository
  )) %>% 
  mutate(repository_name = ifelse(
    article_id == "a001131"
    & link_number == 1
    & is_repository == "yes"
    & is.na(is_repository_other) == FALSE
    & is.na(repository_name) == TRUE,
    is_repository_other,
    repository_name
  )) %>% 
  mutate(is_repository_other = ifelse(
    article_id == "a001131"
    & link_number == 1
    & is_repository == "yes"
    & is.na(is_repository_other) == FALSE
    & is.na(repository_name) == FALSE,
    NA_character_,
    is_repository_other
  ))



# Cleaning files_exist, files_exist_other, download_success, and 
# download_success_other for article intro08.

data_entry_temp <- data_entry_temp %>% 
  mutate(files_exist_other = ifelse(
    article_id == "intro08"
    & is.na(files_exist) == TRUE
    & is.na(files_exist_other) == TRUE
    & download_success == "other"
    & is.na(download_success_other) == FALSE,
    download_success_other,
    files_exist_other
  )) %>% 
  mutate(download_success_other = ifelse(
    article_id == "intro08"
    & is.na(files_exist) == TRUE
    & is.na(files_exist_other) == FALSE
    & download_success == "other"
    & is.na(download_success_other) == FALSE,
    NA_character_,
    download_success_other
  )) %>% 
  mutate(download_success = ifelse(
    article_id == "intro08"
    & is.na(files_exist) == TRUE
    & is.na(files_exist_other) == FALSE
    & download_success == "other"
    & is.na(download_success_other) == TRUE,
    "no",
    download_success
  )) %>% 
  mutate(files_exist = ifelse(
    article_id == "intro08"
    & is.na(files_exist) == TRUE
    & is.na(files_exist_other) == FALSE
    & download_success == "no"
    & is.na(download_success_other) == TRUE,
    "other",
    files_exist
  ))



# Cleaning download_success_other
# Move comments in field download_success_other to field notes (append).
data_entry_temp <- data_entry_temp %>% 
  mutate(notes = ifelse(
    article_id %in% c("a000840", "a000842")
    & download_success == "yes"
    & is.na(download_success_other) == FALSE,
    paste0(notes, " - ", download_success_other),
    notes
  )) %>% 
  mutate(download_success_other = ifelse(
    article_id %in% c("a000840", "a000842")
    & download_success == "yes"
    & is.na(download_success_other) == FALSE,
    NA_character_,
    download_success_other
  ))

# Set the clean data as a separate object.
data_entry_nguyenlam_clean <- data_entry_temp

# Clean up (to avoid confusion when the code for other coders is run).
rm(list = c("data_entry_temp"))

# Export the cleaned coding data set to file (CSV and RDS).
readr::write_rds(x = data_entry_nguyenlam_clean,
                 path = here::here("data", "output", "stage1",
                                   "after_cleaning",
                                   "data_entry_clean_nguyenlam.rds"))

readr::write_csv(x = data_entry_nguyenlam_clean,
                 path = here::here("data", "output", "stage1",
                                   "after_cleaning",
                                   "data_entry_clean_nguyenlam.csv"))