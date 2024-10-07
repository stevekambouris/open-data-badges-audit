################################################################################
# stage1_03_clean_coding_data_kongmanas.R
#
# This script cleans the raw coding data entered by coder kongmanas.
#
# The script assumes the raw data exists in the current environment with name
# data_entry_<coder name>_raw (imported by stage1_02_import_coding_data.R).
#
# The code takes the raw data, and makes use of a temporary data frame object
# to perform the cleaning required.

# Load required packages.
library(tidyverse)

# Remove the two demo records from the data set.
# Remove batch number indicator rows from the data set.
# Drop the extra column containing a single comment (already dealt with).
data_entry_temp <- data_entry_kongmanas_raw %>% 
  filter(!(article_id %in% c("demo01", "demo02"))) %>% 
  filter(!(substr(article_id, 1, 5) == "batch")) %>% 
  select(-c("X24"))



# Cleaning article_has_links_other
# Remove redundant comment made in article_has_links_other (already in notes
# field).
data_entry_temp <- data_entry_temp %>% 
  mutate(article_has_links_other = ifelse(
    article_id == "a000400"
    & article_has_links == "no"
    & is.na(article_has_links_other) == FALSE,
    NA_character_,
    article_has_links_other
  ))



# Cleaning link_work and link_work_noother
# Article a000808: set link_work to "other" and link_work_noother to the value
# the notes field.
# Article a000440: remove a redundant comment from link_work_noother.
data_entry_temp <- data_entry_temp %>% 
  mutate(link_work = ifelse(
    article_id == "a000808"
    & article_has_links == "other"
    & is.na(link_url) == FALSE
    & is.na(link_work) == TRUE,
    "other",
    link_work
  )) %>% 
  mutate(link_work_noother = case_when(
    article_id == "a000808" & article_has_links == "other"
    & is.na(link_url) == FALSE & link_work == "other" ~ notes,
    article_id == "a000440" & link_work == "yes"
    & is.na(link_work_noother) == FALSE ~ NA_character_,
    TRUE ~ link_work_noother
  ))


# Cleaning files_exist_other, download_success, download_success_other
# Article a000785: add comment to files_exist_other, change download_success
# to "no" and remove comment from download_success_other.
data_entry_temp <- data_entry_temp %>% 
  mutate(files_exist_other = ifelse(
    article_id == "a000785"
    & files_exist == "other"
    & is.na(files_exist_other) == TRUE,
    download_success_other,
    files_exist_other
  )) %>% 
  mutate(download_success = ifelse(
    article_id == "a000785"
    & files_exist == "other"
    & download_success == "other"
    & is.na(download_success_other) == FALSE,
    "no",
    download_success
  )) %>% 
  mutate(download_success_other = ifelse(
    article_id == "a000785"
    & files_exist == "other"
    & download_success == "no"
    & files_exist_other == download_success_other,
    NA_character_,
    download_success_other
  ))



# Cleaning download_success, download_success_other
# Article a000914: set files_exist to "no" and clear redundant comment from
# files_exist_other.
# Article a001116: set download_success to "no".
# Article a001430: append comment in download_success_other to notes field.
data_entry_temp <- data_entry_temp %>% 
  mutate(files_exist = ifelse(
    article_id == "a000914"
    & files_exist == "other"
    & is.na(download_success) == TRUE,
    "no",
    files_exist
  )) %>% 
  mutate(files_exist_other = ifelse(
    article_id == "a000914"
    & files_exist == "no"
    & is.na(files_exist_other) == FALSE,
    NA_character_,
    files_exist_other
  )) %>% 
  mutate(download_success = ifelse(
    article_id == "a001116"
    & is_repository == "other"
    & files_exist == "other"
    & is.na(download_success) == TRUE,
    "no",
    download_success
  )) %>% 
  mutate(notes = ifelse(
    article_id == "a001430"
    & download_success == "no"
    & is.na(download_success_other) == FALSE,
    paste0(download_success_other, ". ", notes),
    notes
  )) %>% 
  mutate(download_success_other = ifelse(
    article_id == "a001430"
    & download_success == "no"
    & is.na(download_success_other) == FALSE,
    NA_character_,
    download_success_other
  ))



# Cleaning responses for article intro08 link 2 of 2.
data_entry_temp <- data_entry_temp %>% 
  mutate(download_success = ifelse(
    article_id == "intro08"
    & link_number == 2
    & link_work == "other"
    & is.na(download_success) == TRUE,
    "other",
    download_success
  )) %>% 
  mutate(download_success_other = ifelse(
    article_id == "intro08"
    & link_number == 2
    & link_work == "other",
    "(not completed by coder)",
    download_success_other
  )) %>% 
  mutate(files_exist = ifelse(
    article_id == "intro08"
    & link_number == 2
    & link_work == "other"
    & is.na(files_exist) == TRUE,
    "other",
    files_exist
  )) %>% 
  mutate(files_exist_other = ifelse(
    article_id == "intro08"
    & link_number == 2
    & link_work == "other"
    & files_exist == "other",
    "(not completed by coder)",
    files_exist_other
  )) %>% 
  mutate(is_repository = ifelse(
    article_id == "intro08"
    & link_number == 2
    & link_work == "other"
    & is.na(is_repository) == TRUE,
    "other",
    is_repository
  )) %>% 
  mutate(is_repository_other = ifelse(
    article_id == "intro08"
    & link_number == 2
    & link_work == "other",
    "(not completed by coder)",
    is_repository_other
  )) %>% 
  mutate(resolved_url = ifelse(
    article_id == "intro08"
    & link_number == 2
    & link_work == "other",
    "(not completed by coder)",
    resolved_url
  ))



# Set the clean data as a separate object.
data_entry_kongmanas_clean <- data_entry_temp

# Clean up (to avoid confusion when the code for other coders is run).
rm(list = c("data_entry_temp"))

# Export the cleaned coding data set to file (CSV and RDS).
readr::write_rds(x = data_entry_kongmanas_clean,
                 path = here::here("data", "output", "stage1",
                                   "after_cleaning",
                                   "data_entry_clean_kongmanas.rds"))

readr::write_csv(x = data_entry_kongmanas_clean,
                 path = here::here("data", "output", "stage1",
                                   "after_cleaning",
                                   "data_entry_clean_kongmanas.csv"))