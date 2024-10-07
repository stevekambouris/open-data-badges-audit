################################################################################
# stage1_03_clean_coding_data_michelle.R
#
# This script cleans the raw coding data entered by coder michelle.
#
# The script assumes the raw data exists in the current environment with name
# data_entry_<coder name>_raw (imported by stage1_02_import_coding_data.R).
#
# The code takes the raw data, and makes use of a temporary data frame object
# to perform the cleaning required.

# Load required packages.
library(tidyverse)

# Incorporate the comment in the "extra" column into the existing notes
# variable.
temp_row <- which(!is.na(data_entry_michelle_raw$X24))
temp_notes1 <- data_entry_michelle_raw[temp_row, "notes"][[1]]
temp_notes2 <- data_entry_michelle_raw[temp_row, "X24"][[1]]
new_notes <- paste0(temp_notes1, "; ", temp_notes2)

data_entry_temp <- data_entry_michelle_raw
data_entry_temp[temp_row, "notes"] <- new_notes

rm(list = c("new_notes", "temp_notes1", "temp_notes2", "temp_row"))


# Add an original row number variable to keep track of row numbers.
# Remove the two demo records from the data set.
# Remove the extra column at the end of the data set.
# Remove the entirely blank rows.
# Remove the row with the stray comment in the notes field only.
# Resolve missing article numbers from rows (visual inspection of the data
# indicates that the tidyr "fill" function can be used here to fill in the
# missing article numbers).
# Recode coder's name/initials to format for merging.
data_entry_temp <- data_entry_temp %>% 
  mutate(orig_row = row_number()) %>% 
  filter(!(article_id %in% c("demo01", "demo02"))) %>% 
  select(-X24) %>% 
  filter_at(vars(!"orig_row"), any_vars(!is.na(.))) %>% 
  filter_at(vars(!"notes" & !"orig_row"), any_vars(!is.na(.))) %>% 
  fill(article_id, coder_initials, date_coded, has_data_badge,
       data_availability_statement, article_has_links, number_of_links,
       .direction = "down") %>% 
  mutate(coder_initials = recode(coder_initials, Michelle = "michelle"))


# These are the row numbers that had missing article numbers, etc. due to
# multiple links as found by a visual examination using Excel.
#check_fromexcel <- c(11, 16, 19, 20, 21, 45, 54, 67, 79, 84, 90, 96, 135, 139,
# 140, 158, 175, 176, 184, 191, 205, 207, 234, 235, 306, 307, 327, 358, 372,
# 387, 388, 393, 394, 412, 413)



# Clean data for record for article a000851. From the information entered for
# this article, it appears that there was not an explicit link to _data_ - no
# link is actually provided.

# Determine the row number for article a000851.
temp_row <- which(data_entry_temp$article_id == "a000851")

# Change data_availability_statement from "no" to "other".
data_entry_temp[temp_row, "data_availability_statement"] <- "other"

# Change article_has_links from "yes" to "no".
data_entry_temp[temp_row, "article_has_links"] <- "no"

# Change number_of_links from 1 to NA.
data_entry_temp[temp_row, "number_of_links"] <- NA

# Clean up.
rm(list = c("temp_row"))



# Cleaning article_has_links_other
# Move the comments from the field article_has_links_other to the field notes.
data_entry_temp <- data_entry_temp %>% 
  mutate(notes = ifelse(
    article_id %in% c("a000831", "a000685")
    & article_has_links == "no"
    & is.na(article_has_links_other) == FALSE,
    article_has_links_other,
    notes
  )) %>% 
  mutate(article_has_links_other = ifelse(
    article_id %in% c("a000831", "a000685")
    & article_has_links == "no"
    & is.na(article_has_links_other) == FALSE,
    NA_character_,
    article_has_links_other
  ))



# Cleaning number_of_links
# Remove the value for number_of_links for cases where no link information is
# provided.
data_entry_temp <- data_entry_temp %>% 
  mutate(number_of_links = ifelse(
    article_id %in% c("a000831", "a000685", "a001051", "a000026")
    & number_of_links == 1
    & is.na(link_url) == TRUE,
    NA,
    number_of_links
  ))



# Clean the responses for article intro10, which was a training exercise and
# coders were still getting a feel for the coding form. All the entries after
# the field link_work_noother can be set to NA, since they are all redundant
# expressions of being unsure/not knowing due to the link not working.

# Determine the row number for article intro10.
temp_row <- which(data_entry_temp$article_id == "intro10")

# Set all the fields after link_work_noother to NA.
data_entry_temp[temp_row, "resolved_url"] <- NA_character_
data_entry_temp[temp_row, "is_repository"] <- NA_character_
data_entry_temp[temp_row, "is_repository_other"] <- NA_character_
data_entry_temp[temp_row, "repository_name"] <- NA_character_
data_entry_temp[temp_row, "files_exist"] <- NA_character_
data_entry_temp[temp_row, "files_exist_other"] <- NA_character_
data_entry_temp[temp_row, "download_success"] <- NA_character_
data_entry_temp[temp_row, "download_success_other"] <- NA_character_

# Clean up.
rm(list = c("temp_row"))



# Cleaning is_repository
data_entry_temp <- data_entry_temp %>% 
  mutate(is_repository = ifelse(
    article_id == "a001147"
    & link_work == "no"
    & is_repository == "yes",
    NA_character_,
    is_repository
  )) %>% 
  mutate(is_repository = ifelse(
    article_id == "a000550"
    & link_number == 1
    & is_repository == "y",
    "yes",
    is_repository
  ))



# Cleaning files_exist_other
# The comment in files_exist_other is reiterated in the notes field.
data_entry_temp <- data_entry_temp %>% 
  mutate(files_exist_other = ifelse(
    article_id == "a000849"
    & files_exist == "no"
    & is.na(files_exist_other) == FALSE,
    NA_character_,
    files_exist_other
  ))



# Cleaning download_success

# Set the value of download_success to "no" for cases where a login was
# required to access files, or specific files could not be found,
# and files_exist is "other".
# For two cases where the second link was a duplicate of the first link, set
# download_success status to "other" and make a note in download_success_other.
data_entry_temp <- data_entry_temp %>% 
  mutate(download_success = case_when(
    article_id %in% c("a000492", "a000535")
    & files_exist == "other" & is.na(download_success) == TRUE ~ "other",
    article_id %in% c("a000260", "a000512", "a001116", "a001490", "a001545",
                      "a000160", "a001493", "a000655", "a001285")
    & files_exist == "other" & is.na(download_success) == TRUE ~ "no",
    TRUE ~ download_success
  )) %>% 
  mutate(download_success_other = ifelse(
    article_id %in% c("a000492", "a000535")
    & download_success == "other"
    & is.na(download_success_other) == TRUE,
    "Link leads to data files already downloaded via previous link so not downloaded",
    download_success_other
  ))



# Cleaning download_success_other

# Move one comment from download_success_other to notes field.
# Remove one comment which is already repeated in the notes field.
data_entry_temp <- data_entry_temp %>% 
  mutate(notes = ifelse(
    article_id == "intro02"
    & link_number == 1
    & download_success == "no"
    & is.na(download_success_other) == FALSE,
    download_success_other,
    notes
  )) %>% 
  mutate(download_success_other = case_when(
    article_id == "intro02" & link_number == 1 & download_success == "no"
    & is.na(download_success_other) == FALSE ~ NA_character_,
    article_id == "a001113" & link_number == 1 & download_success == "no"
    & is.na(download_success_other) == FALSE ~ NA_character_,
    TRUE ~ download_success_other
  ))



# Set the clean data as a separate object.
data_entry_michelle_clean <- data_entry_temp

# Clean up (to avoid confusion when the code for other coders is run).
rm(list = c("data_entry_temp"))

# Export the cleaned coding data set to file (CSV and RDS).
readr::write_rds(x = data_entry_michelle_clean,
                 path = here::here("data", "output", "stage1",
                                   "after_cleaning",
                                   "data_entry_clean_michelle.rds"))

readr::write_csv(x = data_entry_michelle_clean,
                 path = here::here("data", "output", "stage1",
                                   "after_cleaning",
                                   "data_entry_clean_michelle.csv"))