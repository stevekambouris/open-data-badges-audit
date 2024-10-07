################################################################################
# stage1_11_coding_cases_followup_step03.R
#
# This script uses the results of step 2 of the reviewing/finalising process to
# compare coders' responses for link_work and resolved_url for each cleaned
# link_url found.

# Load required packages.
library(tidyverse)
library(here)

# Import the coders' cleaned data set.
all_clean_coding <- readr::read_rds(here("data", "output", "stage1",
                                         "all_clean_coding.rds"))

# Import the finalised step 2 data for the articles.
all_final_coding_step2 <- read_rds(here("data", "output", "stage1",
                                        "all_final_coding_step2.rds"))

# Import the link_url cleaning data for the articles.
all_clean_coding_link_url <- read_rds(here("data", "output", "stage1",
                                           "all_clean_coding_link_url.rds"))



# Set up the data to be checked.

study_ids_to_check <- all_final_coding_step2 %>% 
  distinct(study_id)

coding_to_check <- all_clean_coding %>% 
  inner_join(study_ids_to_check, by = "study_id") %>% 
  filter(is.na(link_url) == FALSE) %>% 
  inner_join(all_clean_coding_link_url, by = c("study_id", "link_url", "coder",
                                               "link_number"))

coding_mismatches <- all_clean_coding %>% 
  inner_join(study_ids_to_check, by = "study_id") %>% 
  filter(is.na(link_url) == FALSE) %>% 
  anti_join(all_clean_coding_link_url, by = c("study_id", "link_url", "coder",
                                              "link_number"))

# Check: have some coders been duplicated?
check_dupl_coders <- coding_to_check %>% 
  group_by(study_id, link_url_cleaned, coder) %>% 
  count() %>% 
  filter(n > 1)



# Set up checking functions.

# Function to check for disagreement within a study_id and cleaned link_url.
check_disagreement <- function(in_data, checkvar) {
  result <- in_data %>% 
    group_by(study_id, link_url_cleaned) %>% 
    distinct(.data[[checkvar]]) %>% 
    count() %>% 
    filter(n > 1) %>% 
    select(study_id, link_url_cleaned) %>% 
    left_join(in_data, by = c("study_id", "link_url_cleaned"))
  
  return(result)
}

# Function to check for values of "no" or "other" in cases where coders were
# unanimous.
check_noother <- function(in_data, checkvar) {
  result <- in_data %>% 
    group_by(study_id, link_url_cleaned) %>% 
    distinct(.data[[checkvar]]) %>% 
    count() %>% 
    filter(n == 1) %>% 
    select(study_id, link_url_cleaned) %>% 
    left_join(in_data, by = c("study_id", "link_url_cleaned")) %>% 
    filter(.data[[checkvar]] != "yes")
  
  return(result)
}


# Check link_work
# Within each matched link_url, check for inconsistencies, and values of
# no/other.

lw_disagree <- check_disagreement(in_data = coding_to_check,
                                  checkvar = "link_work") %>% 
  relocate(link_url_cleaned, .after = link_url_for_cleaning) %>% 
  mutate(use_code = NA_character_) %>% 
  mutate(review_note = NA_character_)

lw_noother <- check_noother(in_data = coding_to_check,
                            checkvar = "link_work") %>% 
  relocate(link_url_cleaned, .after = link_url_for_cleaning) %>% 
  mutate(review_coding = NA_character_) %>% 
  mutate(review_note = NA_character_)

# Iteration 1: Export to file.
# NOTE: this is no longer the way to export to file, look further down to see
# the code for each iteration.

# write_csv(x = lw_disagree,
#           path = here("data", "output", "stage1", "follow_up",
#                       "step03_follow_up_disagree_link_work_iter01.csv"))
# write_rds(x = lw_disagree,
#           path = here("data", "output", "stage1", "follow_up",
#                       "step03_follow_up_disagree_link_work_iter01.rds"))

# write_csv(x = lw_noother,
#           path = here("data", "output", "stage1", "follow_up",
#                       "step03_follow_up_noother_link_work_iter01.csv"))
# write_rds(x = lw_noother,
#           path = here("data", "output", "stage1", "follow_up",
#                       "step03_follow_up_noother_link_work_iter01.rds"))



################################################################################
# Iteration 2: Use the results from iteration 1 of the link_url follow-up to
# update the values of the follow-up file. This makes running the finalisation
# code easier, if all the messy updating is handled here, in the "follow-up"
# script.

# Import the completed iteration 1 spreadsheets from Google Sheets.
lw_disagree_iter01_results <- read_csv(file = here("data", "raw",
                                                   "stage1_review",
                                                   "stage1_step3_link_work_followup_iter01 - link_work_disagree.csv"))

lw_noother_iter01_results <- read_csv(file = here("data", "raw",
                                                  "stage1_review",
                                                  "stage1_step3_link_work_followup_iter01 - link_work_noother.csv"))



# Records with disagreement over the coding for link_work

# Check to see how many of the records in the iter01 results can be found in
# iter02.
check_disagree_matched <- lw_disagree_iter01_results %>% 
  semi_join(lw_disagree, by = c("study_id", "coder", "link_url"))

# Check to see which records in the iter01 results can't be found in the latest
# iteration of disagreements.
check_disagree_iter01only <- lw_disagree_iter01_results %>% 
  anti_join(lw_disagree, by = c("study_id", "coder", "link_url"))

# Additional check - can the iter01-only records be found in the latest
# finalised step 2 coding? The fact they are no longer found is likely because
# they have been removed from the finalised dataset.
check_disagree_i01_in_s2 <- check_disagree_iter01only %>% 
  semi_join(all_final_coding_step2, by = c("study_id", "link_url_cleaned"))

# Check to see which records in the latest iteration of disagreements can't
# be found in the iter01 results (i.e. the newly established disagreements).
check_disagree_iter02only <- lw_disagree %>% 
  anti_join(lw_disagree_iter01_results, by = c("study_id", "coder", "link_url"))



# Records with unanimous no/other coding for link_work

# Check to see how many of the records in the iter01 results can be found in
# iter02.
check_noother_matched <- lw_noother_iter01_results %>% 
  semi_join(lw_noother, by = c("study_id", "coder", "link_url",
                                "link_url_cleaned"))

# Check to see which records in the iter01 results can't be found in the latest
# iteration of no/other cases.
check_noother_iter01only <- lw_noother_iter01_results %>% 
  anti_join(lw_noother, by = c("study_id", "coder", "link_url",
                                "link_url_cleaned"))

# Check to see if these records that can't be found in the latest iteration
# have in fact moved to the disagreement dataset.
check_noother_i01_not_dis <- check_noother_iter01only %>% 
  anti_join(lw_disagree, by = c("study_id", "coder", "link_url"))

# Check to see which records in the latest iteration of no/other cases can't
# be found in the iter01 results (i.e. the newly established no/other cases).
# These are likely to be records coded by steve alone.
check_noother_iter02only <- lw_noother %>% 
  anti_join(lw_noother_iter01_results, by = c("study_id", "coder", "link_url",
                                               "link_url_cleaned"))



# Format and export iteration 2 of the datasets to file for follow-up.
lw_disagree_iter02 <- lw_disagree_iter01_results %>% 
  select(study_id, coder, link_url, use_code, review_note) %>% 
  right_join(lw_disagree, by = c("study_id", "coder", "link_url")) %>% 
  select(-use_code.y, -review_note.y) %>% 
  rename(use_code=use_code.x, review_note=review_note.x) %>% 
  relocate(link_url, .after=number_of_links) %>% 
  relocate(use_code, review_note, .after=link_url_cleaned) %>% 
  arrange(study_id, link_url_cleaned, coder)

lw_noother_iter02 <- lw_noother_iter01_results %>% 
  select(study_id, coder, link_url, review_coding, review_note) %>% 
  right_join(lw_noother, by = c("study_id", "coder", "link_url")) %>% 
  select(-review_coding.y, -review_note.y) %>% 
  rename(review_coding=review_coding.x, review_note=review_note.x) %>% 
  relocate(link_url, .after=number_of_links) %>% 
  relocate(review_coding, review_note, .after=link_url_cleaned) %>% 
  arrange(study_id, link_url_cleaned, coder)

write_csv(x = lw_disagree_iter02,
          path = here("data", "output", "stage1", "follow_up",
                      "step03_follow_up_disagree_link_work_iter02.csv"))
write_rds(x = lw_disagree_iter02,
          path = here("data", "output", "stage1", "follow_up",
                      "step03_follow_up_disagree_link_work_iter02.rds"))

write_csv(x = lw_noother_iter02,
          path = here("data", "output", "stage1", "follow_up",
                      "step03_follow_up_noother_link_work_iter02.csv"))
write_rds(x = lw_noother_iter02,
          path = here("data", "output", "stage1", "follow_up",
                      "step03_follow_up_noother_link_work_iter02.rds"))



################################################################################
# Iteration 3: Use the results from iteration 2 of the link_url follow-up to
# update the values of the follow-up file. This makes running the finalisation
# code easier, if all the messy updating is handled here, in the "follow-up"
# script.

# Import the completed iteration 2 spreadsheets from Google Sheets.
lw_disagree_iter02_results <- read_csv(file = here("data", "raw",
                                                   "stage1_review",
                                                   "stage1_step3_link_work_followup_iter02 - link_work_disagree.csv"))

lw_noother_iter02_results <- read_csv(file = here("data", "raw",
                                                  "stage1_review",
                                                  "stage1_step3_link_work_followup_iter02 - link_work_noother.csv"))


# Records with disagreement over the coding for link_work

# Check to see how many of the records in the iter02 results can be found in
# iter03. (This should be the number of records in the iter02 results.)
check_disagree_matched <- lw_disagree_iter02_results %>% 
  semi_join(lw_disagree, by = c("study_id", "coder", "link_url"))

# Check to see which records in the iter02 results can't be found in the latest
# iteration of disagreements. (This should be zero, unless some records have
# been removed from the coding data.)
check_disagree_iter02only <- lw_disagree_iter02_results %>% 
  anti_join(lw_disagree, by = c("study_id", "coder", "link_url"))

# Additional check - can the iter02-only records be found in the latest
# finalised step 2 coding? The fact they are no longer found is likely because
# they have been removed from the finalised dataset.
# (This is moot if the number of iter02-only records is zero.)
check_disagree_i02_in_s2 <- check_disagree_iter02only %>% 
  semi_join(all_final_coding_step2, by = c("study_id", "link_url_cleaned"))

# Check to see which records in the latest iteration of disagreements can't
# be found in the iter03 results (i.e. the newly established disagreements).
# (In practice, this will be the disagreements raised by an addition batch of
# coding.)
check_disagree_iter03only <- lw_disagree %>% 
  anti_join(lw_disagree_iter02_results, by = c("study_id", "coder", "link_url"))


# Records with unanimous no/other coding for link_work

# Check to see how many of the records in the iter02 results can be found in
# iter03.
check_noother_matched <- lw_noother_iter02_results %>% 
  semi_join(lw_noother, by = c("study_id", "coder", "link_url",
                               "link_url_cleaned"))

# Check to see which records in the iter02 results can't be found in the latest
# iteration of no/other cases.
check_noother_iter02only <- lw_noother_iter02_results %>% 
  anti_join(lw_noother, by = c("study_id", "coder", "link_url",
                               "link_url_cleaned"))

# Check to see if these records that can't be found in the latest iteration
# have in fact moved to the disagreement dataset.
check_noother_i02_not_dis <- check_noother_iter02only %>% 
  anti_join(lw_disagree, by = c("study_id", "coder", "link_url"))

# Check to see which records in the latest iteration of no/other cases can't
# be found in the iter02 results (i.e. the newly established no/other cases).
# These are likely to be records coded by steve alone.
check_noother_iter03only <- lw_noother %>% 
  anti_join(lw_noother_iter02_results, by = c("study_id", "coder", "link_url",
                                              "link_url_cleaned"))


# Format and export iteration 3 of the datasets to file for follow-up.
lw_disagree_iter03 <- lw_disagree_iter02_results %>% 
  select(study_id, coder, link_url, use_code, review_note) %>% 
  right_join(lw_disagree, by = c("study_id", "coder", "link_url")) %>% 
  select(-use_code.y, -review_note.y) %>% 
  rename(use_code=use_code.x, review_note=review_note.x) %>% 
  relocate(link_url, .after=number_of_links) %>% 
  relocate(use_code, review_note, .after=link_url_cleaned) %>% 
  arrange(study_id, link_url_cleaned, coder)

lw_noother_iter03 <- lw_noother_iter02_results %>% 
  select(study_id, coder, link_url, review_coding, review_note) %>% 
  right_join(lw_noother, by = c("study_id", "coder", "link_url")) %>% 
  select(-review_coding.y, -review_note.y) %>% 
  rename(review_coding=review_coding.x, review_note=review_note.x) %>% 
  relocate(link_url, .after=number_of_links) %>% 
  relocate(review_coding, review_note, .after=link_url_cleaned) %>% 
  arrange(study_id, link_url_cleaned, coder)

write_csv(x = lw_disagree_iter03,
          path = here("data", "output", "stage1", "follow_up",
                      "step03_follow_up_disagree_link_work_iter03.csv"))
write_rds(x = lw_disagree_iter03,
          path = here("data", "output", "stage1", "follow_up",
                      "step03_follow_up_disagree_link_work_iter03.rds"))

write_csv(x = lw_noother_iter03,
          path = here("data", "output", "stage1", "follow_up",
                      "step03_follow_up_noother_link_work_iter03.csv"))
write_rds(x = lw_noother_iter03,
          path = here("data", "output", "stage1", "follow_up",
                      "step03_follow_up_noother_link_work_iter03.rds"))



################################################################################
# Iteration 4: Use the results from iteration 3 of the link_url follow-up to
# update the values of the follow-up file. This makes running the finalisation
# code easier, if all the messy updating is handled here, in the "follow-up"
# script.

# Import the completed iteration 2 spreadsheets from Google Sheets.
lw_disagree_iter03_results <- read_csv(file = here("data", "raw",
                                                   "stage1_review",
                                                   "stage1_step3_link_work_disagree - iter03.csv"))

lw_noother_iter03_results <- read_csv(file = here("data", "raw",
                                                  "stage1_review",
                                                  "stage1_step3_link_work_noother - iter03.csv"))


# Records with disagreement over the coding for link_work

# Check to see how many of the records in the iter03 results can be found in
# iter04. (This should be the number of records in the iter03 results.)
check_disagree_matched <- lw_disagree_iter03_results %>% 
  semi_join(lw_disagree, by = c("study_id", "coder", "link_url"))

# Check to see which records in the iter03 results can't be found in the latest
# iteration of disagreements. (This should be zero, unless some records have
# been removed from the coding data.)
check_disagree_iter03only <- lw_disagree_iter03_results %>% 
  anti_join(lw_disagree, by = c("study_id", "coder", "link_url"))

# Additional check - can the iter03-only records be found in the latest
# finalised step 2 coding? The fact they are no longer found is likely because
# they have been removed from the finalised dataset.
# (This is moot if the number of iter03-only records is zero.)
check_disagree_i03_in_s2 <- check_disagree_iter03only %>% 
  semi_join(all_final_coding_step2, by = c("study_id", "link_url_cleaned"))

# Check to see which records in the latest iteration of disagreements can't
# be found in the iter04 results (i.e. the newly established disagreements).
# (In practice, this will be the disagreements raised by an addition batch of
# coding.)
check_disagree_iter04only <- lw_disagree %>% 
  anti_join(lw_disagree_iter03_results, by = c("study_id", "coder", "link_url"))


# Records with unanimous no/other coding for link_work

# Check to see how many of the records in the iter03 results can be found in
# iter04.
check_noother_matched <- lw_noother_iter03_results %>% 
  semi_join(lw_noother, by = c("study_id", "coder", "link_url",
                               "link_url_cleaned"))

# Check to see which records in the iter03 results can't be found in the latest
# iteration of no/other cases.
check_noother_iter03only <- lw_noother_iter03_results %>% 
  anti_join(lw_noother, by = c("study_id", "coder", "link_url",
                               "link_url_cleaned"))

# Check to see if these records that can't be found in the latest iteration
# have in fact moved to the disagreement dataset.
check_noother_i03_not_dis <- check_noother_iter03only %>% 
  anti_join(lw_disagree, by = c("study_id", "coder", "link_url"))

# Check to see which records in the latest iteration of no/other cases can't
# be found in the iter03 results (i.e. the newly established no/other cases).
# These are likely to be records coded by steve alone.
check_noother_iter04only <- lw_noother %>% 
  anti_join(lw_noother_iter03_results, by = c("study_id", "coder", "link_url",
                                              "link_url_cleaned"))


# Format and export iteration 3 of the datasets to file for follow-up.
lw_disagree_iter04 <- lw_disagree_iter03_results %>% 
  select(study_id, coder, link_url, use_code, review_note) %>% 
  right_join(lw_disagree, by = c("study_id", "coder", "link_url")) %>% 
  select(-use_code.y, -review_note.y) %>% 
  rename(use_code=use_code.x, review_note=review_note.x) %>% 
  relocate(link_url, .after=number_of_links) %>% 
  relocate(use_code, review_note, .after=link_url_cleaned) %>% 
  arrange(study_id, link_url_cleaned, coder)

lw_noother_iter04 <- lw_noother_iter03_results %>% 
  select(study_id, coder, link_url, review_coding, review_note) %>% 
  right_join(lw_noother, by = c("study_id", "coder", "link_url")) %>% 
  select(-review_coding.y, -review_note.y) %>% 
  rename(review_coding=review_coding.x, review_note=review_note.x) %>% 
  relocate(link_url, .after=number_of_links) %>% 
  relocate(review_coding, review_note, .after=link_url_cleaned) %>% 
  arrange(study_id, link_url_cleaned, coder)

write_csv(x = lw_disagree_iter04,
          path = here("data", "output", "stage1", "follow_up",
                      "step03_follow_up_link_work_disagree_iter04.csv"))
write_rds(x = lw_disagree_iter04,
          path = here("data", "output", "stage1", "follow_up",
                      "step03_follow_up_link_work_disagree_iter04.rds"))

write_csv(x = lw_noother_iter04,
          path = here("data", "output", "stage1", "follow_up",
                      "step03_follow_up_link_work_noother_iter04.csv"))
write_rds(x = lw_noother_iter04,
          path = here("data", "output", "stage1", "follow_up",
                      "step03_follow_up_link_work_noother_iter04.rds"))
