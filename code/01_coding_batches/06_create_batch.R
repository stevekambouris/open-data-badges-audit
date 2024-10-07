# 06_create_batch.R
#
# This script selects some articles to be given to students, and sets up a
# folder structure.

# Simple version for now - does not take into account previously done articles.

# Plan for integrating googledrive package:
# - split this code into two parts: one part for selecting the articles for the
#   batch, and another for actually copying the articles over to the RA's
#   folder.
# - figure out how to construct paths, etc. that will properly identify the
#   article PDFs in the pool folder, and then make copies into the batch folder
#   for the RA.
# - Keep 06_create_batch.R and modify, removing all references to copying PDFs.
# - Create a new file 07_copy_batch.R, which will copy the articles within
#   Google Drive.

# Load packages.
library(tidyverse)
library(here)
library(googledrive)
library(googlesheets4)
library(tictoc)

# Required information:
# - The name/id of the coder
# - The batch number (specified, or work out from the register)
# - The date the batch was assigned.

# Authorise googlesheets4.
googlesheets4::sheets_auth()

# Authorise googledrive.
googledrive::drive_auth()



# Constants
batch_reg_url <- "https://docs.google.com/spreadsheets/d/<--- redacted --->/edit#gid=0"
batch_sum_path <- "~/PhD/assessing_open_data_badges/stage_1/data_collection/admin/batch_summaries/"

# Set the new batch details.
this_batch_num <- 77
this_batch_id <- paste0("batch", sprintf("%04d", this_batch_num))
this_batch_date <- Sys.Date()

this_batch_size <- 40

# Specify the coder.
the_coder <- "evelyn"

tic("Creating batch")

################################################################################
# Part 1: Getting previous batch assignments from Google Drive

# Get the batch register from Google Sheets.
if (sheets_has_token() == TRUE) {
  orig_batch_reg <- googlesheets4::sheets_read(ss = batch_reg_url,
                                               sheet = "batch_register")
}

# Get a list of all batch summary files to try to import.

# Add "summary CSV" to each batch already assigned.
all_batches <- orig_batch_reg %>% 
  mutate(summary_csv = paste0(batch_id, "_summary.csv"))

# Get the unique summary CSVs, according to the register.
all_batch_csvs <- all_batches %>% 
  select(summary_csv) %>% 
  distinct(summary_csv) %>% 
  pull()

# Construct a "path" value for each CSV to compare the Google Drive search
# results against.
check_batch_paths <- paste0(batch_sum_path, all_batch_csvs)

# Download all the CSVs from Google Drive.

# Get the ID of the folder that contains the CSV summaries.
folder_id_summaries <- googledrive::drive_find(n_max = 100,
                                               q = c("name = 'batch_summaries'",
                                                     "mimeType = 'application/vnd.google-apps.folder'"))

folder_id_articles <- googledrive::drive_find(n_max = 100,
                                              q = c("name = 'all_pool_articles'",
                                                    "mimeType = 'application/vnd.google-apps.folder'"))

folder_id_coders <- googledrive::drive_find(n_max = 100,
                                            q = c("name = 'coders'",
                                                  "mimeType = 'application/vnd.google-apps.folder'"))

folder_id_the_coder <- googledrive::drive_find(n_max = 100,
                                               q = c(paste0("name = '", the_coder, "'"),
                                                     paste0("'", as_id(folder_id_coders), "' in parents"),
                                                     "mimeType = 'application/vnd.google-apps.folder'"))

found_csvs <- googledrive::drive_find(n_max = 100,
                                      q = c("name contains 'batch'",
                                            paste0("'", as_id(folder_id_summaries), "' in parents"),
                                            "mimeType = 'text/csv'"))
found_csv_paths <- googledrive::drive_reveal(file = found_csvs, what = "path")

downloaded_csvs <- vector("character", length = nrow(found_csv_paths))

for (i in 1:nrow(found_csv_paths)) {
  this_csv <- found_csv_paths[i, "name"][[1]]
  this_path <- found_csv_paths[i, "path"][[1]]
  this_id <- found_csv_paths[i, "id"][[1]]
  
  if (this_path %in% check_batch_paths) {
    drive_download(file = googledrive::as_id(this_id),
                   path = here::here("data", "temp", this_csv),
                   overwrite = TRUE)
    downloaded_csvs[i] <- here::here("data", "temp", this_csv)
  } else {
    # Two cases here: one, a file with this structure is found elsewhere in
    # Google Drive - that can safely be ignored.
    # Two, a file is in the correct folder, but was not in the batch registry.
    
    # Get the folder path of the found CSV.
    checking_gd_folder <- gsub(pattern = this_csv, replacement = "",
                               x = this_path)
    
    # Print warnings based on whether the path for the CSV found in Google
    # Drive matches the expected path for batch summaries.
    if (checking_gd_folder == batch_sum_path) {
      warning(paste0("File ", this_csv, " was found in the correct Google Drive folder but was not in batch register. Ignored."))
    } else {
      warning(paste0("File ", this_csv, " found in Google Drive was not in the correct folder. Ignored."))
    }
  }
}

# Import all the summary CSVs and append them.
all_csvs_imported <- lapply(X = downloaded_csvs[nchar(downloaded_csvs) > 0],
                            FUN = readr::read_csv)
all_csvs_appended <- dplyr::bind_rows(all_csvs_imported)



################################################################################
# Part 2: Restricting the valid article pool for the selected coder.

# Merge the current batch register with all the batch summaries by batch_id.
# Get a data set of all study_ids already assigned to coders.
all_articles_assigned <- all_batches %>% 
  inner_join(y = all_csvs_appended, by = "batch_id") %>% 
  select(study_id, coder, batch_id, date_assigned) %>% 
  distinct(study_id, coder)

# Get a data set of the number of times each article that has been assigned
# has been checked by different coders.
articles_assigned_count <- all_articles_assigned %>% 
  group_by(study_id) %>% 
  count()

# Read in the selection pool dataset.
osb_selection_pool <- readr::read_rds(here::here("data", "output",
                                                 "osb_selection_pool.rds"))

# Get the set of study_ids already assigned to this coder.
assigned_to_coder <- all_articles_assigned %>% 
  filter(coder == the_coder)

# Merge the assigned coder information and the times coded to the selection
# pool. The final filter removes articles that the coder has already done and
# which already have been assigned twice.
valid_selection_pool <- osb_selection_pool %>% 
  left_join(y = assigned_to_coder, by = "study_id") %>% 
  left_join(y = articles_assigned_count, by = "study_id") %>% 
  mutate(coder = ifelse(is.na(coder), "", coder)) %>% 
  mutate(n = ifelse(is.na(n), 0, n)) %>% 
  filter(allowed_in_pool == TRUE, coder != the_coder, n < 3)

# Special one-off code to construct a particular sample for one coder.
# valid_selection_pool <- osb_selection_pool %>% 
#   left_join(y = assigned_to_coder, by = "study_id") %>% 
#   left_join(y = articles_assigned_count, by = "study_id") %>% 
#   mutate(coder = ifelse(is.na(coder), "", coder)) %>% 
#   mutate(n = ifelse(is.na(n), 0, n)) %>% 
#   filter(allowed_in_pool == TRUE, coder != the_coder,
#          study_id %in% c("a000793", "a000822", "a000863", "a000934", "a000936",
#                          "a000952", "a000971", "a000977", "a000986", "a001020",
#                          "a001040", "a001132", "a001240", "a001287", "a001358",
#                          "a001388", "a001477", "a001481", "a001567", "a001590",
#                          "a001647"))

################################################################################
# Part 3: Taking a sample

# Sample the pool to get a batch of articles.
batch_sample <- valid_selection_pool %>% 
  sample_n(size = this_batch_size, replace = FALSE)

# Create a summary table for the assigned spreadsheet.
batch_summary <- batch_sample %>% 
  mutate(batch_id = this_batch_id) %>% 
  mutate(batch_date = this_batch_date) %>% 
  mutate(pdf_filename = paste0(this_batch_id, "/", study_id, "/", study_id,
                               "_article.pdf")) %>% 
  select(batch_id, batch_date, study_id, Author, `Publication Year`, Title,
         `Publication Title`, pdf_filename) %>% 
  arrange(study_id)

# Write the summary as a CSV to the temp folder.
readr::write_csv(x = batch_summary,
                 path = here::here("data", "temp",
                                   paste0(this_batch_id, "_summary.csv")))



################################################################################
# Part 4: Updating the register, and uploading the new summary CSV.

# Upload the summary CSV.
drive_upload(media = here::here("data", "temp", paste0(this_batch_id, "_summary.csv")),
             path = as_id(folder_id_summaries),
             name = paste0(this_batch_id, "_summary.csv"),
             type = "csv",
             overwrite = FALSE)

# Update the batch register in Google Sheets.
to_append_to_reg <- data.frame(batch_id = this_batch_id,
                               coder = the_coder,
                               date_assigned = this_batch_date)
if (sheets_has_token() == TRUE) {
  googlesheets4::sheets_append(data =to_append_to_reg,
                               ss = batch_reg_url,
                               sheet = "batch_register")
}

# Update the coder's spreadsheet.

file_id_coder_form <- drive_find(n_max = 100,
                                 q = c(paste0("name = 'stage1_data_collection_", the_coder, "'"),
                                       paste0("'", as_id(folder_id_the_coder), "' in parents"),
                                       "mimeType = 'application/vnd.google-apps.spreadsheet'"))
if (nrow(file_id_coder_form) == 1) {
  to_append_to_sum <- batch_summary %>% 
    rename(article_id = study_id, authors = Author, year = `Publication Year`,
           title = Title, journal = `Publication Title`)
  googlesheets4::sheets_append(data =to_append_to_sum,
                               ss = as_sheets_id(file_id_coder_form),
                               sheet = "assigned")
} else {
  warning("Couldn't identify coder's data collection form.")
}


################################################################################
# Part 5: Creating and copying articles across.

# Create a folder in the coder's folder.
folder_id_newbatch <- drive_mkdir(name = this_batch_id,
                                  path = as_id(folder_id_the_coder),
                                  overwrite = FALSE)

# Loop through the batch summary file.
for (i in 1:nrow(batch_summary)) {
  current_article_id <- batch_summary[i, "study_id"][[1]]
  current_article_pdf <- paste0(current_article_id, "_article.pdf")
  
  # Create a folder for the current article within the new batch folder.
  folder_id_current_article <- drive_mkdir(name = current_article_id,
                                           path = as_id(folder_id_newbatch),
                                           overwrite = NA)
  folder_id_current_data <- drive_mkdir(name = "data",
                                           path = as_id(folder_id_current_article),
                                           overwrite = NA)
  file_id_current_pdf <- drive_find(n_max = 100,
                                    q = c(paste0("name = '", current_article_pdf, "'"),
                                          paste0("'", as_id(folder_id_articles), "' in parents"),
                                          "mimeType = 'application/pdf'"))
  if (nrow(file_id_current_pdf) == 1) {
    file_id_copied_pdf <- drive_cp(file = as_id(file_id_current_pdf),
                                   path = as_id(folder_id_current_article),
                                   name = current_article_pdf,
                                   overwrite = NA)
  } else if (nrow(file_id_current_pdf) == 0) {
    warning(paste0(current_article_pdf, " was NOT found in PDF folder."))
  } else {
    warning(paste0(current_article_pdf, " was found more than once in PDF folder."))
  }
}

toc()

# Clean up.

file.remove(dir(path = here::here("data", "temp"),
                pattern = "^batch\\d\\d\\d\\d_summary\\.csv",
                full.names = TRUE))

# Remove all R objects.
rm(list = ls())

################################################################################
# Run this only once: Create a special CSV summary file for the five papers
# that were in the introductory batch (batch0000) that are also in the set. The
# others were from 2020, so they don't count.

# Read in the selection pool dataset.
osb_selection_pool <- readr::read_rds(here::here("data", "output",
                                                 "osb_selection_pool.rds"))

# Create the batch summary for the five articles.
batch0000_summary <- osb_selection_pool %>% 
  filter(study_id %in% c("a000465", "a000473", "a000062", "a001027",
                         "a000880")) %>% 
  mutate(batch_id = "batch0000") %>% 
  mutate(batch_date = as.Date("2020-04-02")) %>% 
  mutate(
    pdf_filename = case_when(
      study_id == "a000465" ~ "batch0000/intro02/intro02_article.pdf",
      study_id == "a000473" ~ "batch0000/intro04/intro04_article.pdf",
      study_id == "a000062" ~ "batch0000/intro05/intro05_article.pdf",
      study_id == "a001027" ~ "batch0000/intro08/intro08_article.pdf",
      study_id == "a000880" ~ "batch0000/intro10/intro10_article.pdf",
      TRUE ~ paste0("batch0000/", study_id, "/", study_id,
                    "_article.pdf")
    )
  ) %>% 
  select(batch_id, batch_date, study_id, Author, `Publication Year`, Title,
         `Publication Title`, pdf_filename) %>% 
  arrange(study_id)

# Write the summary for batch0000 as a CSV.
readr::write_csv(x = batch0000_summary,
                 path = here::here("data", "temp", "batch0000_summary.csv"))
################################################################################
