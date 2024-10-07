################################################################################
# stage1_03_clean_coding_data_phuong.R
#
# This script cleans the raw coding data entered by coder phuong.
#
# The script assumes the raw data exists in the current environment with name
# data_entry_<coder name>_raw (imported by stage1_02_import_coding_data.R).
#
# The code takes the raw data, and makes use of a temporary data frame object
# to perform the cleaning required.

# Load required packages.
library(tidyverse)

# Remove the two demo records from the data set.
# Remove the rows for the articles that were not checked.
data_entry_temp <- data_entry_phuong_raw %>% 
  filter(!(article_id %in% c("demo01", "demo02"))) %>% 
  filter_at(vars(!"article_id"), any_vars(!is.na(.)))



# Cleaning link_work
# Enter a value of "yes" to field link_work for a record for which the link
# clearly worked from the values of the other fields.
data_entry_temp <- data_entry_temp %>% 
  mutate(link_work = ifelse(
    article_id == "a001028"
    & link_number == 1
    & is.na(link_url) == FALSE
    & is.na(link_work) == TRUE,
    "yes",
    link_work
  ))



# Cleaning resolved_url
# Remove the values in the resolved_url field for links which did not work.
# Remove the values in resolved_url for two cases which apparently didn't
# work (append resolved_url value to link_work_noother value).
data_entry_temp <- data_entry_temp %>% 
  mutate(link_work_noother = ifelse(
    article_id == "a001512"
    & link_work == "other"
    & is.na(resolved_url) == FALSE
    & is.na(is_repository) == TRUE,
    paste0(link_work_noother, " - ", resolved_url),
    link_work_noother
  )) %>% 
  mutate(resolved_url = case_when(
    article_id %in% c("intro10", "a000620", "a001005")
    & link_work == "no"
    & is.na(resolved_url) == FALSE ~ NA_character_,
    article_id == "a001512"
    & link_work == "other"
    & is.na(resolved_url) == FALSE
    & is.na(is_repository) == TRUE ~ NA_character_,
    TRUE ~ resolved_url
  ))



# Cleaning link_work
# Change the value for the link_work for article a001512 from "other" to "no".
# From the comment and lack of other data in fields, it seems the links did not
# work.
data_entry_temp <- data_entry_temp %>% 
  mutate(link_work = ifelse(
    article_id == "a001512"
    & link_work == "other"
    & is.na(resolved_url) == TRUE,
    "no",
    link_work
  ))



# Cleaning download_success_other
# Move comment from this field to notes field.
data_entry_temp <- data_entry_temp %>% 
  mutate(notes = ifelse(
    article_id == "a001257"
    & download_success == "yes"
    & is.na(download_success_other) == FALSE,
    download_success_other,
    notes
  )) %>% 
  mutate(download_success_other = ifelse(
    article_id == "a001257"
    & download_success == "yes"
    & is.na(download_success_other) == FALSE,
    NA_character_,
    download_success_other
  ))



# Set the clean data as a separate object.
data_entry_phuong_clean <- data_entry_temp

# Clean up (to avoid confusion when the code for other coders is run).
rm(list = c("data_entry_temp"))

# Export the cleaned coding data set to file (CSV and RDS).
readr::write_rds(x = data_entry_phuong_clean,
                 path = here::here("data", "output", "stage1",
                                   "after_cleaning",
                                   "data_entry_clean_phuong.rds"))

readr::write_csv(x = data_entry_phuong_clean,
                 path = here::here("data", "output", "stage1",
                                   "after_cleaning",
                                   "data_entry_clean_phuong.csv"))