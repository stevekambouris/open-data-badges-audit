################################################################################
# stage1_03_clean_coding_data_matthew.R
#
# This script cleans the raw coding data entered by coder matthew.
#
# The script assumes the raw data exists in the current environment with name
# data_entry_<coder name>_raw (imported by stage1_02_import_coding_data.R).
#
# The code takes the raw data, and makes use of a temporary data frame object
# to perform the cleaning required.

# Load required packages.
library(tidyverse)

# Remove the two demo records from the data set.
data_entry_temp <- data_entry_matthew_raw %>% 
  filter(!(article_id %in% c("demo01", "demo02")))



# Cleaning link count mismatches.
# Article a001628 noted as having two links, but comments indicate that the
# two links led to the same location, so one was ignored/not coded separately.
# Set number_links to 1, but retain the comment about the links in the notes
# field.
data_entry_temp <- data_entry_temp %>% 
  mutate(notes = ifelse(
    article_id == "a001628"
    & number_of_links == 2
    & link_number == 1
    & link_work == "yes"
    & is.na(link_work_noother) == FALSE,
    paste0(link_work_noother, " ", notes),
    notes
  )) %>% 
  mutate(link_work_noother = ifelse(
    article_id == "a001628"
    & number_of_links == 2
    & link_number == 1
    & link_work == "yes"
    & is.na(link_work_noother) == FALSE,
    NA_character_,
    link_work_noother
  )) %>% 
  mutate(number_of_links = ifelse(
    article_id == "a001628"
    & number_of_links == 2
    & link_number == 1
    & link_work == "yes"
    & is.na(link_work_noother) == TRUE,
    1,
    number_of_links
  ))

# Article a000165 coded as having two links, but comments indicate that
# "There were two links available, but both were the same link." So, only one
# link was coded.
data_entry_temp <- data_entry_temp %>% 
  mutate(number_of_links = ifelse(
    article_id == "a000165"
    & number_of_links == 2
    & link_number == 1,
    1,
    number_of_links
  ))



# Cleaning data_availability_statement_other
# Remove comments from data_availability_statement_other that are repeated in
# the notes field.
data_entry_temp <- data_entry_temp %>% 
  mutate(data_availability_statement_other = ifelse(
    article_id %in% c("a001122", "a001475")
    & data_availability_statement == "yes"
    & is.na(data_availability_statement_other) == FALSE,
    NA_character_,
    data_availability_statement_other
  ))



# Cleaning article_has_links
# Add a value of "yes" to a record for which there clearly was a link in the 
# article.
data_entry_temp <- data_entry_temp %>% 
  mutate(article_has_links = ifelse(
    article_id == "intro08"
    & number_of_links == 2
    & is.na(article_has_links) == TRUE,
    "yes",
    article_has_links
  ))



# Cleaning article_has_links, number_of_links, link_number
# Fix a case where article_has_links is "yes", but the notes field reveals
# that there was in fact no link.
data_entry_temp <- data_entry_temp %>% 
  mutate(article_has_links = ifelse(
    article_id == "a000678"
    & article_has_links == "yes"
    & number_of_links == 1
    & link_number == 1
    & is.na(link_url) == TRUE,
    "no",
    article_has_links
  )) %>% 
  mutate(number_of_links = ifelse(
    article_id == "a000678"
    & article_has_links == "no"
    & number_of_links == 1
    & link_number == 1
    & is.na(link_url) == TRUE,
    NA_integer_,
    number_of_links
  )) %>% 
  mutate(link_number = ifelse(
    article_id == "a000678"
    & article_has_links == "no"
    & is.na(number_of_links) == TRUE
    & link_number == 1
    & is.na(link_url) == TRUE,
    NA_integer_,
    link_number
  ))



# Cleaning link_work_noother
# Copy comments from notes field to the link_work_noother field.
# Append the comment in link_work_noother to the notes field for a case
# where link_work = "yes".
data_entry_temp <- data_entry_temp %>%
  mutate(link_work_noother = ifelse(
    article_id %in% c("intro10", "a000588", "a000136", "a000127", "a001280")
    & link_work == "no"
    & is.na(link_work_noother) == TRUE,
    notes,
    link_work_noother
  ))



# Cleaning non-working links
# Article a000588 link did not work, but redundantly fills in fields after
# that; remove those codes.
data_entry_temp <- data_entry_temp %>% 
  mutate(resolved_url = ifelse(
    article_id == "a000588"
    & link_work == "no"
    & is.na(resolved_url) == FALSE
    & is.na(is_repository) == FALSE
    & is.na(files_exist) == FALSE
    & is.na(download_success) == FALSE,
    NA_character_,
    resolved_url
  )) %>% 
  mutate(is_repository = ifelse(
    article_id == "a000588"
    & link_work == "no"
    & is.na(resolved_url) == TRUE
    & is.na(is_repository) == FALSE
    & is.na(files_exist) == FALSE
    & is.na(download_success) == FALSE,
    NA_character_,
    is_repository
  )) %>% 
  mutate(files_exist = ifelse(
    article_id == "a000588"
    & link_work == "no"
    & is.na(resolved_url) == TRUE
    & is.na(is_repository) == TRUE
    & is.na(files_exist) == FALSE
    & is.na(download_success) == FALSE,
    NA_character_,
    files_exist
  )) %>% 
  mutate(download_success = ifelse(
    article_id == "a000588"
    & link_work == "no"
    & is.na(resolved_url) == TRUE
    & is.na(is_repository) == TRUE
    & is.na(files_exist) == TRUE
    & is.na(download_success) == FALSE,
    NA_character_,
    download_success
  ))

# Article a000127 had two links, but apparently they didn't work - possibly
# a scrambling of the URL? Move resolved URL to notes field, but remove all
# coding after link_work and link_work_noother.
data_entry_temp <- data_entry_temp %>% 
  mutate(notes = ifelse(
    article_id == "a000127"
    & link_work == "no"
    & is.na(resolved_url) == FALSE
    & is_repository == "yes"
    & repository_name == "OSF"
    & files_exist == "no",
    paste0(notes, " - apparently resolved to ", resolved_url),
    notes
  )) %>% 
  mutate(resolved_url = ifelse(
    article_id == "a000127"
    & link_work == "no"
    & is.na(resolved_url) == FALSE
    & is_repository == "yes"
    & repository_name == "OSF"
    & files_exist == "no",
    NA_character_,
    resolved_url
  )) %>% 
  mutate(is_repository = ifelse(
    article_id == "a000127"
    & link_work == "no"
    & is.na(resolved_url) == TRUE
    & is_repository == "yes"
    & repository_name == "OSF"
    & files_exist == "no",
    NA_character_,
    is_repository
  )) %>% 
  mutate(repository_name = ifelse(
    article_id == "a000127"
    & link_work == "no"
    & is.na(resolved_url) == TRUE
    & is.na(is_repository) == TRUE
    & repository_name == "OSF"
    & files_exist == "no",
    NA_character_,
    repository_name
  )) %>% 
  mutate(files_exist = ifelse(
    article_id == "a000127"
    & link_work == "no"
    & is.na(resolved_url) == TRUE
    & is.na(is_repository) == TRUE
    & is.na(repository_name) == TRUE
    & files_exist == "no",
    NA_character_,
    files_exist
  ))

# Article a001302 says link_work = "no", but review of entire coding record
# indicates that the value should be "other" - only a DOI was provided, which
# is not a _link_, but is a legitimate pointer to a data set.
data_entry_temp <- data_entry_temp %>% 
  mutate(link_work = ifelse(
    article_id == "a001302"
    & link_work == "no"
    & download_success == "yes",
    "other",
    link_work
  ))



# Cleaning is_repository_other
# Remove the is_repository_other comment for a record where the linked site was
# not identified as a repository.
data_entry_temp <- data_entry_temp %>% 
  mutate(is_repository_other = ifelse(
    article_id == "intro08"
    & is_repository == "no"
    & is.na(is_repository_other) == FALSE,
    NA_character_,
    is_repository_other
  ))



# Cleaning files_exist
# Set the value of files_exist to "other", for cases where the link did not
# lead to the specific data files, but to the general repository site where
# the files could be searched for and (potentially) found. In one case, the
# relevant files were found, and in the other, they were not found.
data_entry_temp <- data_entry_temp %>% 
  mutate(files_exist = ifelse(
    article_id %in% c("a001296", "a000160")
    & is.na(files_exist_other) == FALSE
    & files_exist %in% c("yes", "no"),
    "other",
    files_exist
  ))



# Cleaning files_exist_other
# Append comment in files_exist_other to notes field.
data_entry_temp <- data_entry_temp %>% 
  mutate(notes = ifelse(
    article_id == "a000165"
    & files_exist == "yes"
    & is.na(files_exist_other) == FALSE,
    paste0(notes, " ", files_exist_other),
    notes
  )) %>% 
  mutate(files_exist_other = ifelse(
    article_id == "a000165"
    & files_exist == "yes"
    & is.na(files_exist_other) == FALSE,
    NA_character_,
    files_exist_other
  ))



# Cleaning download_success
# Add the value "yes" to one record where the full record makes clear that the
# files were successfully downloaded.
# Change the value of download_success from "no" to "other" for cases where
# the files were too big to download/took too long.
data_entry_temp <- data_entry_temp %>% 
  mutate(download_success = case_when(
    article_id == "intro09" & files_exist == "yes"
    & is.na(download_success) == TRUE ~ "yes",
    article_id %in% c("a001577", "a000083") & download_success == "no"
    & is.na(download_success_other) == FALSE ~ "other",
    TRUE ~ download_success
  ))



# Set the clean data as a separate object.
data_entry_matthew_clean <- data_entry_temp

# Clean up (to avoid confusion when the code for other coders is run).
rm(list = c("data_entry_temp"))

# Export the cleaned coding data set to file (CSV and RDS).
readr::write_rds(x = data_entry_matthew_clean,
                 path = here::here("data", "output", "stage1",
                                   "after_cleaning",
                                   "data_entry_clean_matthew.rds"))

readr::write_csv(x = data_entry_matthew_clean,
                 path = here::here("data", "output", "stage1",
                                   "after_cleaning",
                                   "data_entry_clean_matthew.csv"))