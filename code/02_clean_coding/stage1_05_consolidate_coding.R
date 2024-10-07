################################################################################
# stage1_05_consolidate_coding.R
#
# This script concatenates the cleaned data sets of all coders into a single
# data set, and exports the data set to file.
#
# This can only be run after all the stage1_03_clean_coding_data_<coder>.R
# scripts have been run (or re-run).
#
# This script will need to be re-run every time that a coder's coding files
# get updated, to ensure that all coders' records are up to date.

# Load required packages.
library(tidyverse)
library(here)

# Construct a list of all the cleaned coding data .rds files to import.
clean_coding_filenames <- dir(path = here("data", "output", "stage1",
                                          "after_cleaning"),
                              pattern = "^data_entry_clean_[a-z]+\\.rds",
                              full.names = TRUE)

# Import all the cleaned coding data files, and concatenate.
# Perform a small amount of final cleaning:
# - remove unneeded column
# - recode the "intro" study ids back to their original format ids
# - rename coder_initials and article_id to coder and study_id
# - filter out the intro articles that are not part of the eligible set of
#   articles.
all_coding_separated <- lapply(X = clean_coding_filenames,
                 FUN = readr::read_rds)
all_clean_coding <- dplyr::bind_rows(all_coding_separated) %>% 
  select(-orig_row) %>% 
  mutate(article_id = case_when(
    article_id == "intro05" ~ "a000062",
    article_id == "intro02" ~ "a000465",
    article_id == "intro04" ~ "a000473",
    article_id == "intro10" ~ "a000880",
    article_id == "intro08" ~ "a001027",
    TRUE ~ article_id
  )) %>% 
  rename(coder = coder_initials, study_id = article_id) %>% 
  filter(! study_id %in% c("intro01", "intro03", "intro06", "intro07",
                           "intro09"))

# Export the concatenated cleaned coding data to file.
readr::write_rds(x = all_clean_coding,
                 path = here::here("data", "output", "stage1",
                                   "all_clean_coding.rds"))

readr::write_csv(x = all_clean_coding,
                 path = here::here("data", "output", "stage1",
                                   "all_clean_coding.csv"))