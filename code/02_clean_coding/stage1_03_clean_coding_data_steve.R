################################################################################
# stage1_03_clean_coding_data_steve.R
#
# This script cleans the raw coding data entered by coder steve.
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
data_entry_temp <- data_entry_steve_raw %>% 
  filter(!(article_id %in% c("demo01", "demo02"))) %>% 
  mutate(coder_initials = recode(coder_initials, SK = "steve"))

# Set the clean data as a separate object.
data_entry_steve_clean <- data_entry_temp

# Clean up (to avoid confusion when the code for other coders is run).
rm(list = c("data_entry_temp"))

# Export the cleaned coding data set to file (CSV and RDS).
readr::write_rds(x = data_entry_steve_clean,
                 path = here::here("data", "output", "stage1",
                                   "after_cleaning",
                                   "data_entry_clean_steve.rds"))

readr::write_csv(x = data_entry_steve_clean,
                 path = here::here("data", "output", "stage1",
                                   "after_cleaning",
                                   "data_entry_clean_steve.csv"))