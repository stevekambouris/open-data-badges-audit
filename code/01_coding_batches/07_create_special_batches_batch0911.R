# 07_create_special_batches_batch0911.R
#
# This script selects specific articles to be given to coder steve to resolve
# issues arising from checking whether links work (see data log).



################################################################################
# Part 0: Setup.

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

# Set the new batch details.
this_batch_num <- 911
this_batch_id <- paste0("batch", sprintf("%04d", this_batch_num))
this_batch_date <- Sys.Date()

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

# Get a set of study_ids for steve to code.
# These include:
# - articles with link_work disagreements;
# - articles with link_work no/other which are not correct;
# - articles which link to the iris-database.org url;
# - articles from the American Journal of Primatology.
study_ids_raw <- c("a000355", "a000501", "a000694", "a000849", "a000948",
                   "a001057", "a001094", "a001394", "a001429", "a001466",
                   "a001512",
                   "a000367", "a000493", "a000777", "a001147", "a001250",
                   "a000501", "a000688", "a000785", "a000845", "a001285",
                   "a001296",
                   "a000391", "a000493", "a001149")

# Remove duplicates.
study_ids_raw <- unique(study_ids_raw)

# Check: has steve coded any of these articles already?
data_entry_steve_raw <- read_csv(file = here("data", "raw",
                                             "stage1_coding",
                                             "stage1_data_collection_steve - data_entry.csv"))

already_coded <- data_entry_steve_raw %>% 
  filter(article_id %in% study_ids_raw)

study_ids_already_coded <- unique(already_coded$article_id)

study_ids_for_steve <- tibble(study_id = study_ids_raw[!(study_ids_raw %in% study_ids_already_coded)])

# Clean up.
rm(list = c("study_ids_raw", "data_entry_steve_raw", "already_coded",
            "study_ids_already_coded"))



################################################################################
# Part 2: Construct the batch of articles

# Read in the selection pool dataset.
osb_selection_pool <- readr::read_rds(here::here("data", "output",
                                                 "osb_selection_pool.rds"))

# Select the study_ids to assign from the pool dataset.
special_selection_pool <- osb_selection_pool %>% 
  inner_join(study_ids_for_steve, by = "study_id")

# Create a summary table for the assigned spreadsheet.
batch_summary <- special_selection_pool %>% 
  mutate(batch_id = this_batch_id) %>% 
  mutate(batch_date = this_batch_date) %>% 
  mutate(pdf_filename = "Refer to Zotero library for PDF/link") %>% 
  select(batch_id, batch_date, study_id, Author, `Publication Year`, Title,
         `Publication Title`, pdf_filename) %>% 
  arrange(study_id)

# Write the summary as a CSV to the local batches folder.
readr::write_csv(x = batch_summary,
                 path = here::here("data", "raw", "stage1_batches",
                                   paste0(this_batch_id, "_summary.csv")))



################################################################################
# Part 3: Updating the register, and uploading the new summary CSV.

# Upload the summary CSV for the current batch to the Google Drive.
drive_upload(media = here::here("data", "raw", "stage1_batches",
                                paste0(this_batch_id, "_summary.csv")),
             path = as_id(folder_id_summaries),
             name = paste0(this_batch_id, "_summary.csv"),
             type = "csv",
             overwrite = FALSE)

# Update the batch register (reference directly via its url) in Google Sheets.
to_append_to_reg <- data.frame(batch_id = this_batch_id,
                               coder = "steve",
                               date_assigned = this_batch_date)
if (gs4_has_token() == TRUE) {
  googlesheets4::sheet_append(data =to_append_to_reg,
                              ss = batch_reg_url,
                              sheet = "batch_register")
}

# Update the coder's spreadsheet with the batch summary information.

# Find the folder for the coder (steve).
folder_id_the_coder <- googledrive::drive_find(n_max = 100,
                                               q = c(paste0("name = 'steve'"),
                                                     paste0("'", as_id(folder_id_coders), "' in parents"),
                                                     "mimeType = 'application/vnd.google-apps.folder'"))

# Find the coder's Google Sheet used for coding (steve).
file_id_coder_form <- drive_find(n_max = 100,
                                 q = c(paste0("name = 'stage1_data_collection_steve'"),
                                       paste0("'", as_id(folder_id_the_coder), "' in parents"),
                                       "mimeType = 'application/vnd.google-apps.spreadsheet'"))

# If the coder's Google Sheet can be found, update it.
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

# Find the folder for coder steve
# (Not needed a second time, since it has already been found in Part 3.)
# folder_id_the_coder <- googledrive::drive_find(n_max = 100,
#                                                q = c(paste0("name = 'steve'"),
#                                                      paste0("'", as_id(folder_id_coders), "' in parents"),
#                                                      "mimeType = 'application/vnd.google-apps.folder'"))

# Create a folder for the batch in the coder's folder.
folder_id_newbatch <- drive_mkdir(name = this_batch_id,
                                  path = as_id(folder_id_the_coder),
                                  overwrite = FALSE)

# Loop through the batch summary file and create a subfolder for each article
# in the batch.
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

# Finish up.
toc()
