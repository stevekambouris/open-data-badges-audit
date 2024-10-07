# 07_create_special_batches_batch0910.R
#
# This script selects specific articles to be given to coder steve to resolve
# issues with link_urls not being double coded. When completed, this batch of
# coding should result in all urls not already coded by steve being double
# coded.

# Load packages.
library(tidyverse)
library(here)
library(googledrive)
library(googlesheets4)
library(tictoc)



# Authorise googlesheets4.
googlesheets4::gs4_auth()

# Authorise googledrive.
googledrive::drive_auth()



# Constants
batch_reg_url <- "https://docs.google.com/spreadsheets/d/1G16k4ckhunrU3_MU15WfFB1IaatudXxPrH63i8FXyrQ/edit#gid=0"
batch_sum_path <- "~/PhD/assessing_open_data_badges/stage_1/data_collection/admin/batch_summaries/"



tic("Creating batch")

# Get the ID of the folder that contains the CSV summaries.
folder_id_summaries <- googledrive::drive_find(n_max = 100,
                                               q = c("name = 'batch_summaries'",
                                                     "mimeType = 'application/vnd.google-apps.folder'"))

# Get the ID of the folder containing all the coders' folders.
folder_id_coders <- googledrive::drive_find(n_max = 100,
                                            q = c("name = 'coders'",
                                                  "mimeType = 'application/vnd.google-apps.folder'"))



################################################################################
# Part 1: Determine which articles need to be coded.

# Read in the results of the link_url follow-up.
link_url_followup_data <- read_csv(file = here("data", "raw", "stage1_review",
                                               "iter01",
                                               "stage1_step2_link_url_followup_iter01 - link_url_followup.csv"))

# Drop the records that were clearly inappropriate/incorrect/irrelevant.
# Move the correct urls to correct_link_url.
# Drop any articles where steve is already a coder.
# Drop the retracted article a000063 which is not to be included in analysis.
link_url_followup_clean <- link_url_followup_data %>% 
  filter(!(use_code == FALSE & is.na(corrected_link_url) == TRUE)) %>% 
  filter(study_id != "a000063") %>% 
  mutate(corrected_link_url = case_when(
    use_code == TRUE & is.na(corrected_link_url) == TRUE ~ link_url,
    TRUE ~ corrected_link_url
  )) %>% 
  group_by(study_id) %>% 
  filter(all(coder != "steve")) %>% 
  ungroup()

# Check the articles that did have steve as a coder.
check_steve <- link_url_followup_data %>% 
  filter(!(use_code == FALSE & is.na(corrected_link_url) == TRUE)) %>% 
  mutate(corrected_link_url = case_when(
    use_code == TRUE & is.na(corrected_link_url) == TRUE ~ link_url,
    TRUE ~ corrected_link_url
  )) %>% 
  group_by(study_id) %>% 
  filter(any(coder == "steve")) %>% 
  ungroup()

# Determine how many of the corrected urls now have at least two coders.
check_coders_per_link <- link_url_followup_clean %>% 
  group_by(study_id) %>% 
  count(corrected_link_url) %>% 
  filter(any(n < 2)) %>% 
  ungroup()

# Get a set of study_ids for steve to code.
study_ids_for_steve <- check_coders_per_link %>% 
  distinct(study_id)



################################################################################
# Part 2: Construct the batch of articles

# Read in the selection pool dataset.
osb_selection_pool <- readr::read_rds(here::here("data", "output",
                                                 "osb_selection_pool.rds"))

# Merge the study_ids to assign with the pool dataset.
special_selection_pool <- osb_selection_pool %>% 
  inner_join(y = study_ids_for_steve, by = "study_id")

# Set the new batch details.
this_batch_num <- 910
this_batch_id <- paste0("batch", sprintf("%04d", this_batch_num))
this_batch_date <- Sys.Date()


# Create a summary table for the assigned spreadsheet.
batch_summary <- special_selection_pool %>% 
  mutate(batch_id = this_batch_id) %>% 
  mutate(batch_date = this_batch_date) %>% 
  mutate(pdf_filename = "Refer to Zotero library for PDF/link") %>% 
  select(batch_id, batch_date, study_id, Author, `Publication Year`, Title,
         `Publication Title`, pdf_filename) %>% 
  arrange(study_id)

# Write the summary as a CSV to the batches folder.
readr::write_csv(x = batch_summary,
                 path = here::here("data", "raw", "stage1_batches",
                                   paste0(this_batch_id, "_summary.csv")))



################################################################################
# Part 3: Updating the register, and uploading the new summary CSV.



# Upload the summary CSV.
drive_upload(media = here::here("data", "raw", "stage1_batches",
                                paste0(this_batch_id, "_summary.csv")),
             path = as_id(folder_id_summaries),
             name = paste0(this_batch_id, "_summary.csv"),
             type = "csv",
             overwrite = FALSE)

# Update the batch register in Google Sheets.
# coder steve
to_append_to_reg <- data.frame(batch_id = this_batch_id,
                               coder = "steve",
                               date_assigned = this_batch_date)
if (sheets_has_token() == TRUE) {
  googlesheets4::sheet_append(data =to_append_to_reg,
                              ss = batch_reg_url,
                              sheet = "batch_register")
}

# Update the coder's spreadsheet.
# coder steve
folder_id_the_coder <- googledrive::drive_find(n_max = 100,
                                               q = c(paste0("name = 'steve'"),
                                                     paste0("'", as_id(folder_id_coders), "' in parents"),
                                                     "mimeType = 'application/vnd.google-apps.folder'"))

file_id_coder_form <- drive_find(n_max = 100,
                                 q = c(paste0("name = 'stage1_data_collection_steve'"),
                                       paste0("'", as_id(folder_id_the_coder), "' in parents"),
                                       "mimeType = 'application/vnd.google-apps.spreadsheet'"))
if (nrow(file_id_coder_form) == 1) {
  to_append_to_sum <- batch_summary %>% 
    rename(article_id = study_id, authors = Author, year = `Publication Year`,
           title = Title, journal = `Publication Title`)
  googlesheets4::sheet_append(data =to_append_to_sum,
                              ss = as_sheets_id(file_id_coder_form),
                              sheet = "assigned")
} else {
  warning("Couldn't identify coder's data collection form.")
}



################################################################################
# Part 4: Creating folders.

# coder steve
folder_id_the_coder <- googledrive::drive_find(n_max = 100,
                                               q = c(paste0("name = 'steve'"),
                                                     paste0("'", as_id(folder_id_coders), "' in parents"),
                                                     "mimeType = 'application/vnd.google-apps.folder'"))

# Create a folder in the coder's folder.
folder_id_newbatch <- drive_mkdir(name = this_batch_id,
                                  path = as_id(folder_id_the_coder),
                                  overwrite = FALSE)

# Loop through the batch summary file.
for (i in 1:nrow(batch_summary)) {
  current_article_id <- batch_summary[i, "study_id"][[1]]
  
  # Create a folder for the current article within the new batch folder.
  folder_id_current_article <- drive_mkdir(name = current_article_id,
                                           path = as_id(folder_id_newbatch),
                                           overwrite = NA)
  folder_id_current_data <- drive_mkdir(name = "data",
                                        path = as_id(folder_id_current_article),
                                        overwrite = NA)
}

toc()
