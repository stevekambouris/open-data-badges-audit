# 07_create_special_batches.R
#
# This script selects specific articles to be given to coders, and sets up a
# folder structure.

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
# Part 2: Restricting the valid article pool.

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

# Merge the assigned coder information and the times coded to the selection
# pool.
special_selection_pool <- osb_selection_pool %>% 
  left_join(y = articles_assigned_count, by = "study_id") %>% 
  mutate(n = ifelse(is.na(n), 0, n)) %>% 
  filter(allowed_in_pool == FALSE | n < 2)



################################################################################
# Part 3: Taking a sample

# Set the new batch details.
this_batch_num <- 901
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
# coder steve
to_append_to_reg <- data.frame(batch_id = this_batch_id,
                               coder = "steve",
                               date_assigned = this_batch_date)
if (sheets_has_token() == TRUE) {
  googlesheets4::sheets_append(data =to_append_to_reg,
                               ss = batch_reg_url,
                               sheet = "batch_register")
}

# coder andy
to_append_to_reg <- data.frame(batch_id = this_batch_id,
                               coder = "andy",
                               date_assigned = this_batch_date)
if (sheets_has_token() == TRUE) {
  googlesheets4::sheets_append(data =to_append_to_reg,
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
  googlesheets4::sheets_append(data =to_append_to_sum,
                               ss = as_sheets_id(file_id_coder_form),
                               sheet = "assigned")
} else {
  warning("Couldn't identify coder's data collection form.")
}

# coder andy
folder_id_the_coder <- googledrive::drive_find(n_max = 100,
                                               q = c(paste0("name = 'andy'"),
                                                     paste0("'", as_id(folder_id_coders), "' in parents"),
                                                     "mimeType = 'application/vnd.google-apps.folder'"))

file_id_coder_form <- drive_find(n_max = 100,
                                 q = c(paste0("name = 'stage1_data_collection_andy'"),
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
# Part 5: Creating folders.

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

# coder andy
folder_id_the_coder <- googledrive::drive_find(n_max = 100,
                                               q = c(paste0("name = 'andy'"),
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

# Clean up.

file.remove(dir(path = here::here("data", "temp"),
                pattern = "^batch\\d\\d\\d\\d_summary\\.csv",
                full.names = TRUE))



################################################################################
#
# Eligible articles that have only been coded once ("remaining articles")
# For steve
#
################################################################################

# Import the remaining articles data set.
remaining_articles <- readr::read_rds(path = here("data", "output", "stage1",
                                                  "remaining_articles.rds"))

# Determine which of the remaining articles can be coded by steve.
# Remove all articles from batch0901 - andy has been assigned these already.
# Remove articles that have already been coded by steve - assign to andy.
remaining_valid <- remaining_articles %>% 
  filter(!batch_id == "batch0901") %>% 
  group_by(study_id) %>%
  filter(all(coder != "steve")) %>%
  ungroup() %>% 
  distinct(study_id)



# Next, run all the code before Part 1 (loading libraries, setting
# constants, updating tokens with google drive) then run the code in Part 1 to
# set the objects: folder_id_summaries, folder_id_articles, folder_id_coders



# Part 2: Restricting the valid article pool.

# Read in the selection pool dataset.
osb_selection_pool <- readr::read_rds(here::here("data", "output",
                                                 "osb_selection_pool.rds"))

# Merge the assigned coder information and the times coded to the selection
# pool.
special_selection_pool <- osb_selection_pool %>% 
  inner_join(y = remaining_valid, by = "study_id")



# Part 3: Taking a sample

# Set the new batch details.
this_batch_num <- 902
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

# Write the summary as a CSV to the temp folder.
readr::write_csv(x = batch_summary,
                 path = here::here("data", "temp",
                                   paste0(this_batch_id, "_summary.csv")))



# Part 4: Updating the register, and uploading the new summary CSV.

# Upload the summary CSV.
drive_upload(media = here::here("data", "temp", paste0(this_batch_id, "_summary.csv")),
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



# Part 5: Creating folders.

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



################################################################################
#
# Eligible articles that have only been coded once ("remaining articles")
# For andy
#
################################################################################

# Import the remaining articles data set.
remaining_articles <- readr::read_rds(path = here("data", "output", "stage1",
                                                  "remaining_articles.rds"))

# Determine which of the remaining articles can be coded by steve.
# Remove all articles from batch0901 - andy has been assigned these already.
# Keep articles that have already been coded by steve - assign to andy.
remaining_valid <- remaining_articles %>% 
  filter(!batch_id == "batch0901") %>% 
  group_by(study_id) %>%
  filter(any(coder == "steve")) %>%
  ungroup() %>% 
  distinct(study_id)



# Next, run all the code before Part 1 (loading libraries, setting
# constants, updating tokens with google drive) then run the code in Part 1 to
# set the objects: folder_id_summaries, folder_id_articles, folder_id_coders



# Part 2: Restricting the valid article pool.

# Read in the selection pool dataset.
osb_selection_pool <- readr::read_rds(here::here("data", "output",
                                                 "osb_selection_pool.rds"))

# Merge the assigned coder information and the times coded to the selection
# pool.
special_selection_pool <- osb_selection_pool %>% 
  inner_join(y = remaining_valid, by = "study_id")



# Part 3: Taking a sample

# Set the new batch details.
this_batch_num <- 903
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

# Write the summary as a CSV to the temp folder.
readr::write_csv(x = batch_summary,
                 path = here::here("data", "temp",
                                   paste0(this_batch_id, "_summary.csv")))



# Part 4: Updating the register, and uploading the new summary CSV.

# Upload the summary CSV.
drive_upload(media = here::here("data", "temp", paste0(this_batch_id, "_summary.csv")),
             path = as_id(folder_id_summaries),
             name = paste0(this_batch_id, "_summary.csv"),
             type = "csv",
             overwrite = FALSE)

# Update the batch register in Google Sheets.
# coder andy
to_append_to_reg <- data.frame(batch_id = this_batch_id,
                               coder = "andy",
                               date_assigned = this_batch_date)
if (sheets_has_token() == TRUE) {
  googlesheets4::sheet_append(data =to_append_to_reg,
                              ss = batch_reg_url,
                              sheet = "batch_register")
}

# Update the coder's spreadsheet.
# coder andy
folder_id_the_coder <- googledrive::drive_find(n_max = 100,
                                               q = c(paste0("name = 'andy'"),
                                                     paste0("'", as_id(folder_id_coders), "' in parents"),
                                                     "mimeType = 'application/vnd.google-apps.folder'"))

file_id_coder_form <- drive_find(n_max = 100,
                                 q = c(paste0("name = 'stage1_data_collection_andy'"),
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



# Part 5: Creating folders.
# coder andy
folder_id_the_coder <- googledrive::drive_find(n_max = 100,
                                               q = c(paste0("name = 'andy'"),
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