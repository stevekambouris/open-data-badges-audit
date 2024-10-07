################################################################################
# stage1_03_clean_coding_data_andy.R
#
# This script cleans the raw coding data entered by coder andy.
#
# The script assumes the raw data exists in the current environment with name
# data_entry_<coder name>_raw (imported by stage1_02_import_coding_data.R).
#
# The code takes the raw data, and makes use of a temporary data frame object
# to perform the cleaning required.

# Load required packages.
library(tidyverse)

# Remove the two demo records from the data set, as well as the 10 intro
# records.
data_entry_temp <- data_entry_andy_raw %>% 
  filter(!(article_id %in% c("demo01", "demo02",
                             "intro01", "intro02", "intro03", "intro04",
                             "intro05", "intro06", "intro07", "intro08",
                             "intro09", "intro10")))

# Set the clean data as a separate object.
data_entry_andy_clean <- data_entry_temp

# Clean up (to avoid confusion when the code for other coders is run).
rm(list = c("data_entry_temp"))

# Export the cleaned coding data set to file (CSV and RDS).
readr::write_rds(x = data_entry_andy_clean,
                 path = here::here("data", "output", "stage1",
                                   "after_cleaning",
                                   "data_entry_clean_andy.rds"))

readr::write_csv(x = data_entry_andy_clean,
                 path = here::here("data", "output", "stage1",
                                   "after_cleaning",
                                   "data_entry_clean_andy.csv"))