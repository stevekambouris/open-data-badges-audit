################################################################################
# stage1_03_clean_coding_data_sophia.R
#
# This script cleans the raw coding data entered by coder sophia.
#
# The script assumes the raw data exists in the current environment with name
# data_entry_<coder name>_raw (imported by stage1_02_import_coding_data.R).
#
# The code takes the raw data, and makes use of a temporary data frame object
# to perform the cleaning required.

# Load required packages.
library(tidyverse)

# Cleaning article_id
# Cleaning coder_initials
# Remove the two demo records from the data set.
# Recode coder's name/initials to format for merging.
data_entry_temp <- data_entry_sophia_raw %>% 
  filter(!(article_id %in% c("demo01", "demo02"))) %>% 
  mutate(coder_initials = recode(coder_initials, Sophia = "sophia"))



# Cleaning has_badge
# Remove comment from has_data_badge_other since has_data_badge = "no".
data_entry_temp <- data_entry_temp %>% 
  mutate(has_data_badge_other = ifelse(
    article_id == "a000787"
    & has_data_badge == "no",
    NA,
    has_data_badge_other
  ))



# Cleaning data_availability_statement
# Remove comment from data_availability_statement_other where 
# data_availability_statement is "no" and move comment to the notes field.
data_entry_temp <- data_entry_temp %>% 
  mutate(notes = ifelse(
    article_id == "a000980"
    & data_availability_statement == "no",
    data_availability_statement_other,
    notes
  )) %>% 
  mutate(data_availability_statement_other = ifelse(
    article_id == "a000980"
    & data_availability_statement == "no",
    NA,
    data_availability_statement_other
  ))



# Cleaning has_links
# Remove one redundant comment from article_has_links_other.
# Move two comments from article_has_links_other to notes.
# Fill in five cases of missing values which are implied to be "no" from the
# lack of additional information.
data_entry_temp <- data_entry_temp %>% 
  mutate(notes = ifelse(
    article_id %in% c("a001268", "a000573")
    & article_has_links == "no",
    article_has_links_other,
    notes
  )) %>% 
  mutate(article_has_links_other = ifelse(
    article_id %in% c("a000371", "a001268", "a000573")
    & article_has_links == "no",
    NA,
    article_has_links_other
  )) %>% 
  mutate(article_has_links = ifelse(
    article_id %in% c("a000980", "a000678", "a000026", "a000540", "a000597")
    & data_availability_statement == "no"
    & is.na(article_has_links) == TRUE,
    "no",
    article_has_links
  ))



# Cleaning resolved_url
# Remove two values of resolved_url for links which did not work.
# Remove superfluous values from later variables for a link which did not work.
data_entry_temp <- data_entry_temp %>% 
  mutate(resolved_url = ifelse(
    article_id %in% c("intro10", "a000367")
    & link_number == 1
    & link_work == "no"
    & is.na(resolved_url) == FALSE,
    NA,
    resolved_url
  )) %>% 
  mutate(is_repository = ifelse(
    article_id == "intro10"
    & link_number == 1
    & link_work == "no"
    & is_repository == "no",
    NA,
    is_repository
  )) %>% 
  mutate(files_exist = ifelse(
    article_id == "intro10"
    & link_number == 1
    & link_work == "no"
    & files_exist == "no",
    NA,
    files_exist
  )) %>% 
  mutate(download_success = ifelse(
    article_id == "intro10"
    & link_number == 1
    & link_work == "no"
    & download_success == "no",
    NA,
    download_success
  ))



# Cleaning files_exist
# Put comments in files_exist_other into the notes field (five cases).
data_entry_temp <- data_entry_temp %>% 
  mutate(notes = ifelse(
    article_id %in% c("a001125", "a001476", "a001215", "a000295", "a000334")
    & files_exist == "no"
    & is.na(files_exist_other) == FALSE,
    files_exist_other,
    notes
  )) %>% 
  mutate(files_exist_other = ifelse(
    article_id %in% c("a001125", "a001476", "a001215", "a000295", "a000334")
    & files_exist == "no"
    & is.na(files_exist_other) == FALSE,
    NA,
    files_exist_other
  ))


# Cleaning download_success
# Remove values of download_success for four cases where files_exist = no.
# Fill in missing values of download_success where the answer should be no
# from context.
# Change some values of download_success from "no" to "other".
data_entry_temp <- data_entry_temp %>% 
  mutate(download_success = case_when(
    article_id %in% c("a000001", "a000569", "a001051", "a001125")
     & files_exist == "no"
     & download_success == "no" ~ NA_character_,
    article_id %in% c("a000965", "a000031", "a000845")
     & files_exist == "other"
     & (is.na(download_success)  == TRUE) ~ "no",
    article_id %in% c("intro02", "a000904", "a000311", "a000971", "a001326",
                       "a001100")
     & files_exist == "yes"
     & download_success == "no"
     & (is.na(download_success_other) == FALSE) ~ "other",
    TRUE ~ download_success
  )) %>%
  mutate(download_success_other = ifelse(
    article_id == "a000429"
    & files_exist == "yes"
    & download_success == "no"
    & is.na(download_success_other) == FALSE,
    NA,
    download_success_other
  ))

# Set the clean data as a separate object.
data_entry_sophia_clean <- data_entry_temp

# Clean up (to avoid confusion when the code for other coders is run).
rm(list = c("data_entry_temp"))

# Export the cleaned coding data set to file (CSV and RDS).
readr::write_rds(x = data_entry_sophia_clean,
                 path = here::here("data", "output", "stage1",
                                   "after_cleaning",
                                   "data_entry_clean_sophia.rds"))

readr::write_csv(x = data_entry_sophia_clean,
                 path = here::here("data", "output", "stage1",
                                   "after_cleaning",
                                   "data_entry_clean_sophia.csv"))