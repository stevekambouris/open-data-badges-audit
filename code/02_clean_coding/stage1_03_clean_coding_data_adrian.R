################################################################################
# stage1_03_clean_coding_data_adrian.R
#
# This script cleans the raw coding data entered by coder adrian.
#
# The script assumes the raw data exists in the current environment with name
# data_entry_<coder name>_raw (imported by stage1_02_import_coding_data.R).
#
# The code takes the raw data, and makes use of a temporary data frame object
# to perform the cleaning required.

# Load required packages.
library(tidyverse)

# Remove the two demo records from the data set.
data_entry_temp <- data_entry_adrian_raw %>% 
  filter(!(article_id %in% c("demo01", "demo02")))

# Remove comment from data_availability_statement_other where
# data_availability_statement = no and issue covered in notes (a000244).
data_entry_temp <- data_entry_temp %>% 
  mutate(data_availability_statement_other = ifelse(
    article_id == "a000244"
    & data_availability_statement == "no"
    & is.na(data_availability_statement_other) == FALSE,
    NA,
    data_availability_statement_other
  ))

# Set value of download_success to "no" in cases where files_exist = "other"
# due to account logins being required.
data_entry_temp <- data_entry_temp %>% 
  mutate(download_success = ifelse(
    article_id %in% c("a001186", "a000581", "a000581", "a000655")
    & files_exist == "other"
    & is.na(download_success) == TRUE,
    "no",
    download_success
  ))

# Set value of download_success to "yes" in case where notes comment indicates
# that everything was OK, but download_success is "other" and there is no
# comment in download_success_other.
data_entry_temp <- data_entry_temp %>% 
  mutate(download_success = ifelse(
    article_id == "a000934"
    & download_success == "other"
    & is.na(download_success_other) == TRUE,
    "yes",
    download_success
  ))

# Change the number of links for article intro08 from 2 to 1. This is explained
# in the notes field for this article.
data_entry_temp <- data_entry_temp %>% 
  mutate(number_of_links = ifelse(
    article_id == "intro08"
    & number_of_links == 2
    & link_number == 1,
    1,
    number_of_links
  ))

# Set the clean data as a separate object.
data_entry_adrian_clean <- data_entry_temp

# Clean up (to avoid confusion when the code for other coders is run).
rm(list = c("data_entry_temp"))

# Export the cleaned coding data set to file (CSV and RDS).
readr::write_rds(x = data_entry_adrian_clean,
                 path = here::here("data", "output", "stage1",
                                   "after_cleaning",
                                   "data_entry_clean_adrian.rds"))

readr::write_csv(x = data_entry_adrian_clean,
                 path = here::here("data", "output", "stage1",
                                   "after_cleaning",
                                   "data_entry_clean_adrian.csv"))