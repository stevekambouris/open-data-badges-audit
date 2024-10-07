################################################################################
# stage1_15_coding_cases_followup_step06.R
#
# This script uses the results of step 5 of the reviewing/finalising process to
# compare coders' responses for download_success for each cleaned resolved_url
# found.

# NOTE:
# Make use of the checking functions check_disagreement() and check_noother()
# which are defined in stage1_11_coding_cases_followup_step03.R
# The code for the functions is NOT repeated here, load from the above script
# first.


# Load required packages.
library(tidyverse)
library(here)



################################################################################
# Section 1: Import data

# Import the coders' cleaned data set.
all_clean_coding <- readr::read_rds(here("data", "output", "stage1",
                                         "all_clean_coding.rds"))

# Import the finalised step 5 data for the articles.
all_final_coding_step5 <- read_rds(here("data", "output", "stage1",
                                        "all_final_coding_step5.rds"))

# Import the link_url cleaning data for the articles.
all_clean_coding_resolved_url <- read_rds(here("data", "output", "stage1",
                                           "all_clean_coding_resolved_url.rds"))



################################################################################
# Section 2: Set up the data to be checked

# Get all the unique study_id values from Step 5 of the finalised dataset.
study_ids_to_check <- all_final_coding_step5 %>% 
  distinct(study_id)

# Get all cases in the cleaned coding data which meet these criteria:
# 1) The coder reports a value of "yes" or "other" for files_exist;
# 2) The coder can be found in the all_clean_coding_resolved_url lookup dataset.
#
# The second one is important; some records will have been dropped due to
# problems with the original coding. Appearing in the lookup dataset is the
# official way to know if a coding record is still valid or not.
coding_to_check <- all_clean_coding %>% 
  inner_join(study_ids_to_check, by = "study_id") %>% 
  filter(files_exist %in% c("yes", "other")) %>% 
  inner_join(all_clean_coding_resolved_url,
             by = c("study_id", "link_url", "coder", "link_number",
                    "resolved_url"))

# Check the coding mismatches - there should be an identifiable reason why
# these records were not matched to the resolved url lookup dataset.
coding_mismatches <- all_clean_coding %>% 
  inner_join(study_ids_to_check, by = "study_id") %>% 
  filter(files_exist %in% c("yes", "other")) %>% 
  anti_join(all_clean_coding_resolved_url,
            by = c("study_id", "link_url", "coder", "link_number", 
                   "resolved_url"))

# Check: have some coders been duplicated?
check_dupl_coders <- coding_to_check %>% 
  group_by(study_id, resolved_url_cleaned, coder) %>% 
  count() %>% 
  filter(n > 1)



################################################################################
# Section 3: Check the values of download_success

# Function to check for disagreement within a study_id and cleaned link_url.
check_disagreement <- function(in_data, checkvar) {
  result <- in_data %>% 
    group_by(study_id, resolved_url_cleaned) %>% 
    distinct(.data[[checkvar]]) %>% 
    count() %>% 
    filter(n > 1) %>% 
    select(study_id, resolved_url_cleaned) %>% 
    left_join(in_data, by = c("study_id", "resolved_url_cleaned"))
  
  return(result)
}

# Function to check for values of "no"/"other"/"partial" in cases where coders
# were unanimous.
check_noother <- function(in_data, checkvar) {
  result <- in_data %>% 
    group_by(study_id, resolved_url_cleaned) %>% 
    distinct(.data[[checkvar]]) %>% 
    count() %>% 
    filter(n == 1) %>% 
    select(study_id, resolved_url_cleaned) %>% 
    left_join(in_data, by = c("study_id", "resolved_url_cleaned")) %>% 
    filter(.data[[checkvar]] != "yes")
  
  return(result)
}

# Within each matched resolved_url, check for inconsistencies, and values of
# no/other/partial.

ds_disagree <- check_disagreement(in_data = coding_to_check,
                                  checkvar = "download_success") %>% 
  relocate(resolved_url_cleaned, .after = resolved_url_for_cleaning) %>% 
  mutate(use_code = NA_character_) %>% 
  mutate(review_note = NA_character_)

ds_nop <- check_noother(in_data = coding_to_check,
                            checkvar = "download_success") %>% 
  relocate(resolved_url_cleaned, .after = resolved_url_for_cleaning) %>% 
  mutate(review_coding = NA_character_) %>% 
  mutate(review_note = NA_character_)



################################################################################
# Iteration 1: No additional work needs to be done to the checking datasets.

# Note the file names here - update with each iteration of the data updates.
write_csv(x = ds_disagree,
          path = here("data", "output", "stage1", "follow_up",
                      "step06_follow_up_download_success_disagree_iter01.csv"))
write_rds(x = ds_disagree,
          path = here("data", "output", "stage1", "follow_up",
                      "step06_follow_up_download_success_disagree_iter01.rds"))

# Note the file names here - update with each iteration of the data updates.
write_csv(x = ds_nop,
          path = here("data", "output", "stage1", "follow_up",
                      "step06_follow_up_download_success_nop_iter01.csv"))
write_rds(x = ds_nop,
          path = here("data", "output", "stage1", "follow_up",
                      "step06_follow_up_download_success_nop_iter01.rds"))



################################################################################
# Iteration 2: Import the reviewed iteration 1 follow-up files.

# # Import the completed iteration 1 spreadsheets from Google Sheets.
# ds_disagree_iter01_results <- read_csv(file = here("data", "raw",
#                                                    "stage1_review",
#                                                    "stage1_step5_files_exist_disagree - iter01.csv"))
# 
# ds_noother_iter01_results <- read_csv(file = here("data", "raw",
#                                                   "stage1_review",
#                                                   "stage1_step5_files_exist_noother - iter01.csv"))
# 
# 
# 
# # Records with disagreement over the coding for files_exist
# 
# # Check to see how many of the records in the iter01 results can be found in
# # iter02.
# check_disagree_matched <- fe_disagree_iter01_results %>% 
#   semi_join(fe_disagree, by = c("study_id", "coder", "link_url"))
# 
# # Check to see which records in the iter01 results can't be found in the latest
# # iteration of disagreements.
# check_disagree_iter01only <- fe_disagree_iter01_results %>% 
#   anti_join(fe_disagree, by = c("study_id", "coder", "link_url"))
# 
# # Additional check - can the iter01-only records be found in the latest
# # finalised step 2 coding? The fact they are no longer found is likely because
# # they have been removed from the finalised dataset.
# check_disagree_i01_in_s2 <- check_disagree_iter01only %>% 
#   semi_join(all_final_coding_step4, by = c("study_id", "resolved_url_cleaned"))
# 
# # Check to see which records in the latest iteration of disagreements can't
# # be found in the iter01 results (i.e. the newly established disagreements).
# check_disagree_iter02only <- fe_disagree %>% 
#   anti_join(fe_disagree_iter01_results, by = c("study_id", "coder", "link_url"))
# 
# 
# 
# # Records with unanimous no/other coding for files_exist
# 
# # Check to see how many of the records in the iter01 results can be found in
# # iter02.
# check_noother_matched <- fe_noother_iter01_results %>% 
#   semi_join(fe_noother, by = c("study_id", "coder", "link_url",
#                                "link_url_cleaned"))
# 
# # Check to see which records in the iter01 results can't be found in the latest
# # iteration of no/other cases.
# check_noother_iter01only <- fe_noother_iter01_results %>% 
#   anti_join(fe_noother, by = c("study_id", "coder", "link_url",
#                                "link_url_cleaned"))
# 
# # Check to see if these records that can't be found in the latest iteration
# # have in fact moved to the disagreement dataset.
# check_noother_i01_not_dis <- check_noother_iter01only %>% 
#   anti_join(fe_disagree, by = c("study_id", "coder", "link_url"))
# 
# # Check to see which records in the latest iteration of no/other cases can't
# # be found in the iter01 results (i.e. the newly established no/other cases).
# # These are likely to be records coded by steve alone.
# check_noother_iter02only <- fe_noother %>% 
#   anti_join(fe_noother_iter01_results, by = c("study_id", "coder", "link_url",
#                                               "link_url_cleaned"))
# 
# 
# 
# # Format and export iteration 2 of the datasets to file for follow-up.
# fe_disagree_iter02 <- fe_disagree_iter01_results %>% 
#   select(study_id, coder, link_url, use_code, review_note) %>% 
#   right_join(fe_disagree, by = c("study_id", "coder", "link_url")) %>% 
#   select(-use_code.y, -review_note.y) %>% 
#   rename(use_code=use_code.x, review_note=review_note.x) %>% 
#   relocate(link_url, .after=number_of_links) %>% 
#   relocate(use_code, review_note, .after=resolved_url_cleaned) %>% 
#   arrange(study_id, resolved_url_cleaned, coder)
# 
# fe_noother_iter02 <- fe_noother_iter01_results %>% 
#   select(study_id, coder, link_url, review_coding, review_note) %>% 
#   right_join(fe_noother, by = c("study_id", "coder", "link_url")) %>% 
#   select(-review_coding.y, -review_note.y) %>% 
#   rename(review_coding=review_coding.x, review_note=review_note.x) %>% 
#   relocate(link_url, .after=number_of_links) %>% 
#   relocate(review_coding, review_note, .after=resolved_url_cleaned) %>% 
#   arrange(study_id, resolved_url_cleaned, coder)
# 
# write_csv(x = fe_disagree_iter02,
#           path = here("data", "output", "stage1", "follow_up",
#                       "step05_follow_up_files_exist_disagree_iter02.csv"))
# write_rds(x = fe_disagree_iter02,
#           path = here("data", "output", "stage1", "follow_up",
#                       "step05_follow_up_files_exist_disagree_iter02.rds"))
# 
# write_csv(x = fe_noother_iter02,
#           path = here("data", "output", "stage1", "follow_up",
#                       "step05_follow_up_files_exist_noother_iter02.csv"))
# write_rds(x = fe_noother_iter02,
#           path = here("data", "output", "stage1", "follow_up",
#                       "step05_follow_up_files_exist_noother_iter02.rds"))
