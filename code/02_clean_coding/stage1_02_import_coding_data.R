################################################################################
# stage1_02_import_coding_data.R
#
# This script imports the completed data entry spreadsheets of all the coders.
#
# The data entry spreadsheets have been manually downloaded from Google Sheets
# in CSV format and are in the project folder /data/raw/stage1_coding/.

# Load required packages.
library(tidyverse)
library(here)



################################################################################
# Coder adrian

# Import data.
data_entry_adrian_raw <- read_csv(file = here("data", "raw",
                                              "stage1_coding",
                                              "stage1_data_collection_adrian - data_entry.csv"))



################################################################################
# Coder aniruddah

# Import data.
data_entry_aniruddah_raw <- read_csv(file = here("data", "raw",
                                                 "stage1_coding",
                                                 "stage1_data_collection_aniruddah - data_entry.csv"))



################################################################################
# Coder kongmanas

# Import data.
data_entry_kongmanas_raw <- read_csv(file = here("data", "raw",
                                                 "stage1_coding",
                                                 "stage1_data_collection_kongmanas - data_entry.csv"))



################################################################################
# Coder matthew

# Import data.
data_entry_matthew_raw <- read_csv(file = here("data", "raw",
                                               "stage1_coding",
                                               "stage1_data_collection_matthew - data_entry.csv"))



################################################################################
# Coder michelle

# Import data.
data_entry_michelle_raw <- read_csv(file = here("data", "raw",
                                                "stage1_coding",
                                                "stage1_data_collection_michelle - data_entry.csv"))



################################################################################
# Coder nguyenlam

# Import data.
data_entry_nguyenlam_raw <- read_csv(file = here("data", "raw",
                                                 "stage1_coding",
                                                 "stage1_data_collection_nguyenlam - data_entry.csv"))



################################################################################
# Coder oliver

# Import data.
data_entry_oliver_raw <- read_csv(file = here("data", "raw",
                                              "stage1_coding",
                                              "stage1_data_collection_oliver - data_entry.csv"))



################################################################################
# Coder phuong

# Import data.
data_entry_phuong_raw <- read_csv(file = here("data", "raw",
                                              "stage1_coding",
                                              "stage1_data_collection_phuong - data_entry.csv"))



################################################################################
# Coder sophia

# Import data.
data_entry_sophia_raw <- read_csv(file = here("data", "raw",
                                              "stage1_coding",
                                              "stage1_data_collection_sophia - data_entry.csv"))



################################################################################
# Coder evelyn

# Import the data below the repositioned header row.
data_entry_evelyn_raw1 <- read_csv(file = here("data", "raw",
                                              "stage1_coding",
                                              "stage1_data_collection_evelyn - data_entry.csv"),
                                   skip = 440, col_names = TRUE)

# Import the data above the repositioned header row.
data_entry_evelyn_raw2 <- read_csv(file = here("data", "raw",
                                               "stage1_coding",
                                               "stage1_data_collection_evelyn - data_entry.csv"),
                                   col_names = FALSE, n_max = 440)

# Rename the column names in the second import.
colnames(data_entry_evelyn_raw2) <- colnames(data_entry_evelyn_raw1)

# Bind the two sets of rows together.
data_entry_evelyn_raw <- bind_rows(data_entry_evelyn_raw2,
                                   data_entry_evelyn_raw1)

# Clean up.
rm(list = c("data_entry_evelyn_raw1", "data_entry_evelyn_raw2"))



################################################################################
# Coder steve

# Import data.
data_entry_steve_raw <- read_csv(file = here("data", "raw",
                                             "stage1_coding",
                                             "stage1_data_collection_steve - data_entry.csv"))



################################################################################
# Coder andy

# Import data.
data_entry_andy_raw <- read_csv(file = here("data", "raw",
                                            "stage1_coding",
                                            "stage1_data_collection_andy - data_entry.csv"))


