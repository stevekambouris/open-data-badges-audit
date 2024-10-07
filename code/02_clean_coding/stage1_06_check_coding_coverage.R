################################################################################
# stage1_06_check_coding_coverage.R
#
# This script compares the contents of the coders' cleaned data sets with the
# contents of the Open Science Badges article library. The code checks to see
# how many times each article in the library has been coded by a coder, and
# identifies "gaps" in the coding effort. These gaps are output to an Excel
# spreadsheet file for follow-up.

# Load required packages.
library(tidyverse)
library(here)
library(openxlsx)

# Open the Open Science Badges data set.
osb_selection_pool <- readr::read_rds(here("data", "output",
                                           "osb_selection_pool.rds"))

# Open the batch allocation data set.
all_assigned_articles <- readr::read_rds(here("data", "output", "stage1",
                                              "all_assigned_articles.rds"))

# Merge the complete library with the batch allocation data to determine those
# articles which were not assigned to coders (for whatever reason).
matched_articles <- semi_join(x = ungroup(osb_selection_pool),
                              y = all_assigned_articles,
                              by = c("study_id"))
unassigned_articles <- anti_join(x = ungroup(osb_selection_pool),
                                 y = all_assigned_articles,
                                 by = c("study_id"))

# Open the coders' cleaned data sets.
all_clean_coding <- readr::read_rds(here("data", "output", "stage1",
                                         "all_clean_coding.rds"))

# Within coders, collapse article ids (count number of actual links).
all_coded_articles <- all_clean_coding %>% 
  group_by(coder, study_id) %>% 
  summarise(n_links = n()) %>% 
  ungroup()

# Then concatenate and collapse coders (count number of coders per article).
coders_per_article <- all_clean_coding %>% 
  group_by(coder, study_id) %>% 
  summarise(n_links = n()) %>% 
  group_by(study_id) %>% 
  summarise(n_coders = n()) %>% 
  ungroup()

# Merge each coder's collapsed data with the batch allocation data to see if 
# there are coders who have missed any of their assigned articles entirely.
# Note that there will be at least some, since not all coders completely
# finished their final batch of articles.
missed_articles <- all_assigned_articles %>% 
  anti_join(all_coded_articles, by = c("coder", "study_id"))

# Merge the combined coder data with the batch allocation data to see if there
# are articles that do not have at least two codings associated with them.
remaining_articles <- all_assigned_articles %>%
  left_join(coders_per_article, by = "study_id") %>% 
  filter(n_coders < 2) %>% 
  arrange(study_id, date_assigned)

# Export the remaining articles for further analysis.
readr::write_rds(x = remaining_articles,
                 path = here::here("data", "output", "stage1",
                                   "remaining_articles.rds"))

readr::write_csv(x = remaining_articles,
                 path = here::here("data", "output", "stage1",
                                   "remaining_articles.csv"))