################################################################################
# stage1_03_clean_coding_data_aniruddah.R
#
# This script cleans the raw coding data entered by coder aniruddah.
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
data_entry_temp <- data_entry_aniruddah_raw %>% 
  filter(!(article_id %in% c("demo01", "demo02"))) %>% 
  mutate(coder_initials = recode(coder_initials, Aniruddah = "aniruddah"))

# Cleaning has_links
# Article has missing comment when "other" selected for article_has_links.
# Relevant comment is in notes field.
data_entry_temp <- data_entry_temp %>% 
  mutate(article_has_links_other = ifelse(
    article_id == "a000532"
    & link_number == 1
    & article_has_links == "other"
    & is.na(article_has_links_other) == TRUE,
    notes,
    article_has_links_other
    ))

# Cleaning link_number
# Fill in missing link_number value for paper with only one link.
data_entry_temp <- data_entry_temp %>% 
  mutate(link_number = ifelse(
    article_id == "intro05"
    & number_of_links == 1
    & is.na(link_number) == TRUE,
    1,
    link_number
  ))

# Cleaning link_work
# Fill in missing link_work value for a record with values, but which clearly
# did work.
data_entry_temp <- data_entry_temp %>% 
  mutate(link_work = ifelse(
    article_id == "a001449"
    & link_url == "https://osf.io/34cd8/"
    & is.na(link_work) == TRUE,
    "yes",
    link_work
  ))

# Cleaning link_work_no
# Remove a value for resolved_url for a record where the link did not work.
data_entry_temp <- data_entry_temp %>% 
  mutate(resolved_url = ifelse(
    article_id == "intro10"
    & link_number == 1
    & resolved_url == "http://pisa2006.acer.edu.au/downloads.php",
    NA,
    resolved_url
  ))

# Cleaning download_success
# Put comments in download_success_other for download_success = "other" and
# "partial".
data_entry_temp <- data_entry_temp %>% 
  mutate(download_success_other = ifelse(
    article_id %in% c("a000399", "a000446", "a000452", "a000638", "a001361")
    & download_success == "other"
    & is.na(download_success_other) == TRUE,
    "Download size exceeded 100MB",
    download_success_other
  )) %>% 
  mutate(download_success_other = ifelse(
    article_id == "a001289"
    & download_success == "partial"
    & is.na(download_success_other) == TRUE,
    notes,
    download_success_other
  ))

# Set the clean data as a separate object.
data_entry_aniruddah_clean <- data_entry_temp

# Clean up (to avoid confusion when the code for other coders is run).
rm(list = c("data_entry_temp"))

# Export the cleaned coding data set to file (CSV and RDS).
readr::write_rds(x = data_entry_aniruddah_clean,
                 path = here::here("data", "output", "stage1",
                                   "after_cleaning",
                                   "data_entry_clean_aniruddah.rds"))

readr::write_csv(x = data_entry_aniruddah_clean,
                 path = here::here("data", "output", "stage1",
                                   "after_cleaning",
                                   "data_entry_clean_aniruddah.csv"))