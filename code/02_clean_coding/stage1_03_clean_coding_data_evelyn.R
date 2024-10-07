################################################################################
# stage1_03_clean_coding_data_evelyn.R
#
# This script cleans the raw coding data entered by coder evelyn.
#
# The script assumes the raw data exists in the current environment with name
# data_entry_<coder name>_raw (imported by stage1_02_import_coding_data.R).
#
# The code takes the raw data, and makes use of a temporary data frame object
# to perform the cleaning required.

# Load required packages.
library(tidyverse)

# Remove the two demo records from the data set.
# Remove the inserted columns from the data set.
# Recode coder's name/initials to format for merging.
data_entry_temp <- data_entry_evelyn_raw %>% 
  filter(!(article_id %in% c("demo01", "demo02"))) %>% 
  select(-c("X2", "X3", "X4", "X5")) %>% 
  mutate(coder_initials = recode(coder_initials, Evelyn = "evelyn"))



# Remove the articles that were not reached by the coder.
data_entry_temp <- data_entry_temp %>% 
  filter(!is.na(has_data_badge))



# Cleaning article_has_links
# Add value of "no" to record which clearly has no links to data (according to
# notes field and lack of data entry).
data_entry_temp <- data_entry_temp %>% 
  mutate(article_has_links = ifelse(
    article_id == "a001420"
    & is.na(article_has_links) == TRUE,
    "no",
    article_has_links
  ))



# Cleaning is_repository_other and repository_name
# For a case where the link is to the journal's website and is not a repo.
data_entry_temp <- data_entry_temp %>% 
  mutate(is_repository_other = ifelse(
    article_id == "a001039"
    & link_number == 2
    & is_repository == "no"
    & is.na(is_repository_other) == FALSE,
    NA_character_,
    is_repository_other
  )) %>% 
  mutate(repository_name = ifelse(
    article_id == "a001039"
    & link_number == 2
    & is_repository == "no"
    & is.na(is_repository_other) == TRUE
    & is.na(repository_name) == FALSE,
    NA_character_,
    repository_name
  ))



# Cleaning files_exist
# Set two records where logins required from files_exist = "no" to "other";
# records already have comments in the files_exist_other field, which makes
# "other" the more appropriate value here.
# Also change their download_success values to "no".
data_entry_temp <- data_entry_temp %>% 
  mutate(download_success = ifelse(
    article_id %in% c("a001285", "a001444")
    & files_exist == "no"
    & is.na(files_exist_other) == FALSE,
    "no",
    download_success
  )) %>% 
  mutate(files_exist = ifelse(
    article_id %in% c("a001285", "a001444")
    & files_exist == "no"
    & is.na(files_exist_other) == FALSE,
    "other",
    files_exist
  ))



# Cleaning download_success
# Add download_success = "no" to three articles for which there was a note
# to the effect that files could not be found in the linked repositories.
data_entry_temp <- data_entry_temp %>% 
  mutate(download_success = ifelse(
    article_id %in% c("a000031", "a000160", "a001296")
    & files_exist == "other"
    & is.na(download_success) == TRUE,
    "no",
    download_success
  ))

# Correct two records whose downloads were more than 100MB in total.
data_entry_temp <- data_entry_temp %>% 
  mutate(download_success_other = ifelse(
    article_id == "a000039"
    & download_success == "other"
    & is.na(download_success_other) == TRUE,
    notes,
    download_success_other
  )) %>% 
  mutate(download_success = ifelse(
    article_id == "a001109"
    & download_success == "yes"
    & is.na(download_success_other) == FALSE,
    "other",
    download_success
  ))

# Correct three records where the data files required a login to access.
data_entry_temp <- data_entry_temp %>% 
  mutate(files_exist = ifelse(
    article_id %in% c("a000848", "a001329", "a000605")
    & files_exist == "yes"
    & is.na(files_exist_other) == TRUE
    & download_success == "no"
    & is.na(download_success_other) == FALSE,
    "other",
    files_exist
  )) %>% 
  mutate(files_exist_other = ifelse(
    article_id %in% c("a000848", "a001329", "a000605")
    & files_exist == "other"
    & is.na(files_exist_other) == TRUE
    & download_success == "no"
    & is.na(download_success_other) == FALSE,
    download_success_other,
    files_exist_other
  )) %>% 
  mutate(download_success_other = ifelse(
    article_id %in% c("a000848", "a001329", "a000605")
    & files_exist == "other"
    & is.na(files_exist_other) == FALSE
    & download_success == "no"
    & is.na(download_success_other) == FALSE,
    NA_character_,
    download_success_other
  ))



# Set the clean data as a separate object.
data_entry_evelyn_clean <- data_entry_temp

# Clean up (to avoid confusion when the code for other coders is run).
rm(list = c("data_entry_temp"))

# Export the cleaned coding data set to file (CSV and RDS).
readr::write_rds(x = data_entry_evelyn_clean,
                 path = here::here("data", "output", "stage1",
                                   "after_cleaning",
                                   "data_entry_clean_evelyn.rds"))

readr::write_csv(x = data_entry_evelyn_clean,
                 path = here::here("data", "output", "stage1",
                                   "after_cleaning",
                                   "data_entry_clean_evelyn.csv"))