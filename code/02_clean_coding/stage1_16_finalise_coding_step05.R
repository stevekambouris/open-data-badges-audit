################################################################################
# stage1_16_finalise_coding_step05.R
#
# This script finalises the values of files_exist for all resolved_url records
# finalised in step 4.
#
# Note that this script may need to be re-run if coding is changed, updated,
# added to, etc.

# Load required packages.
library(tidyverse)
library(here)



################################################################################
# Section 1: Import required datasets.

# Import the cleaned coding data set.
all_clean_coding <- readr::read_rds(here("data", "output", "stage1",
                                         "all_clean_coding.rds"))

# Import the Step 4 finalised data set.
all_final_coding_step4 <- read_rds(here("data", "output", "stage1",
                                        "all_final_coding_step4.rds"))

# Import the resolved_url linking data set (used to link coders' link_url
# entries to cleaned values of link_url, and likewise for resolved_url).
all_clean_coding_resolved_url <- read_rds(here("data", "output", "stage1",
                                           "all_clean_coding_resolved_url.rds"))

# Import the results of the Step 5 coding follow-up.
# NOTE: The files names here will need to be updated each time another iteration
# of step 5 follow-up occurs.
step5_followup_results_disagree <- read_csv(file = here("data", "raw",
                                                        "stage1_review",
                                                        "stage1_step5_files_exist_disagree - iter02.csv"))

step5_followup_results_noother <- read_csv(file = here("data", "raw",
                                                       "stage1_review",
                                                       "stage1_step5_files_exist_noother - iter02.csv"))



################################################################################
# Section 2: Check and prepare the follow-up review results

# Cases where files_exist is unanimously no/other: check that none of these need
# further review. There should be none, otherwise another iteration of
# follow-up is required first.
check_noother_need_review <- step5_followup_results_noother %>% 
  filter(review_coding != FALSE)

# Cases of disagreement: check for missing use_code decisions. There should be
# no missing values.
check_disagree_missing <- step5_followup_results_disagree %>% 
  filter(is.na(use_code) == TRUE)

# Cases of disagreement: check for cases where all use_code values are FALSE
# for a url. This should not happen - expected that additional coding or
# re-coding would happen first before getting to this stage.
check_disagree_allF <- step5_followup_results_disagree %>% 
  group_by(study_id, link_url_cleaned) %>% 
  filter(all(use_code == FALSE))

# Cases of disagreement: check for cases where coder steve has a value of
# FALSE for use_code. In general, this should not happen - expected that I
# can code the correct/desired coding (or re-code before this point in the
# case of a genuine error).
check_disagree_steveF <- step5_followup_results_disagree %>% 
  filter(coder == "steve", use_code != TRUE)

# Cases of disagreement: are there cases where more than one record for each
# study_id/link combo is use_code TRUE?
check_disagree_manyT <- step5_followup_results_disagree %>% 
  group_by(study_id, resolved_url_cleaned, use_code) %>% 
  count() %>% 
  filter(use_code == TRUE, n > 1) %>% 
  ungroup() %>% 
  select(-use_code, -n) %>% 
  inner_join(step5_followup_results_disagree, by = c("study_id",
                                                     "resolved_url_cleaned"))

# Check: are the values of files_exist consistent between records with use_code
# TRUE?
# There should be zero cases here - if multiple records within a study/link
# combo have use_code TRUE, then they need to have the same value of files_exist.
# If they don't then there has been a coding review error.
check_disagree_consistent <- check_disagree_manyT %>% 
  group_by(study_id, resolved_url_cleaned, use_code) %>% 
  filter(use_code == TRUE) %>% 
  filter(first(files_exist) != files_exist)

# Clean up checking objects.
rm(list = c("check_noother_need_review", "check_disagree_missing",
            "check_disagree_allF", "check_disagree_steveF",
            "check_disagree_manyT", "check_disagree_consistent"))

# Create a list of the final files_exist disagreement resolutions.
step5_disagree_resolutions <- step5_followup_results_disagree %>% 
  arrange(study_id, resolved_url_cleaned, coder) %>% 
  filter(use_code == TRUE) %>% 
  group_by(study_id, resolved_url_cleaned) %>% 
  slice_tail() %>% 
  ungroup() %>% 
  select(study_id, resolved_url_cleaned, files_exist, files_exist_other)



################################################################################
# Section 3: Merge coders' coding with step 4 finalised data.

# Get all coders' files_exist and files_exist_other codings.
all_clean_coding_files_exist <- all_clean_coding %>% 
  select(study_id, coder, link_number, link_url, resolved_url,
         files_exist, files_exist_other)

# Quick check: can all cases in the resolved_url dataset still be found in the
# all clean coding dataset? The answer should definitely be yes - if not, then 
# something is deeply wrong, recheck Step 4 finalisation.
check_mismatches <- all_clean_coding_resolved_url %>% 
  anti_join(all_clean_coding_files_exist, by = c("study_id", "coder",
                                               "link_number", "link_url",
                                               "resolved_url"))

# Restrict the coding records to those which were confirmed as actually having
# link_url values to check back at the finalisation of Step 2.
all_clean_coding_files_exist2 <- all_clean_coding_resolved_url %>% 
  left_join(all_clean_coding_files_exist, by = c("study_id", "coder",
                                               "link_number", "link_url",
                                               "resolved_url"))

# Get all coding cases where all coders agreed files_exist = "yes".
coding_files_exist_allyes <- all_clean_coding_files_exist2 %>% 
  group_by(study_id, resolved_url_cleaned) %>% 
  filter(all(files_exist == "yes"))

# Get all coding cases where all coders agreed files_exist = "no".
coding_files_exist_allno <- all_clean_coding_files_exist2 %>% 
  group_by(study_id, resolved_url_cleaned) %>% 
  filter(all(files_exist == "no"))

# Get all coding cases where all coders agreed files_exist = "other".
coding_files_exist_allother <- all_clean_coding_files_exist2 %>% 
  arrange(study_id, resolved_url_cleaned, coder) %>% 
  group_by(study_id, resolved_url_cleaned) %>% 
  filter(all(files_exist == "other"))

# Get all coding cases where coders disagreed on files_exist.
coding_files_exist_disagree <- all_clean_coding_files_exist2 %>% 
  group_by(study_id, resolved_url_cleaned) %>% 
  filter(!all(files_exist == "yes"), !all(files_exist == "no"),
         !all(files_exist == "other"))

# Check: are there any mismatches between the "leftover" disagreement records
# and the imported Step 3 follow-up review results?
check_mismatches1 <- coding_files_exist_disagree %>% 
  anti_join(step5_followup_results_disagree,
            by = c("study_id", "coder", "link_url", "resolved_url"))
check_mismatches2 <- step5_followup_results_disagree %>% 
  anti_join(coding_files_exist_disagree,
            by = c("study_id", "coder", "link_url", "resolved_url"))

# Clean up.
rm(list = c("check_mismatches", "check_mismatches1", "check_mismatches2"))



################################################################################
# Section 4: Create datasets containing the finalised values of files_exist and
# files_exist_other for each study_id/link_url_cleaned combination.

files_exist_final_yes <- coding_files_exist_allyes %>% 
  slice_head() %>% 
  ungroup() %>% 
  select(study_id, resolved_url_cleaned, files_exist, files_exist_other)

files_exist_final_no <- coding_files_exist_allno %>% 
  slice_head() %>% 
  ungroup() %>% 
  select(study_id, resolved_url_cleaned, files_exist, files_exist_other)

files_exist_final_other <- coding_files_exist_allother %>% 
  slice_tail() %>% 
  ungroup() %>% 
  select(study_id, resolved_url_cleaned, files_exist, files_exist_other)

# Combine the records into a single dataset.
files_exist_final_concat <- bind_rows(files_exist_final_yes,
                                    files_exist_final_no,
                                    files_exist_final_other,
                                    step5_disagree_resolutions)

# Check: are there any duplicates in the dataset?
check_duplicates <- files_exist_final_concat %>% 
  group_by(study_id, resolved_url_cleaned) %>% 
  count() %>% 
  filter(n > 1)

# Check: can all records be found in the Step 4 finalised data set?
check_mismatches1 <- files_exist_final_concat %>% 
  anti_join(all_final_coding_step4, by = c("study_id", "resolved_url_cleaned"))

# Check: are all records in Step 4 finalised data set that can't be found in
# the files_exist dataset those records without resolved_url values?
check_mismatches2 <- all_final_coding_step4 %>% 
  anti_join(files_exist_final_concat, by = c("study_id", "resolved_url_cleaned"))

# Clean up.
rm(list = c("check_duplicates", "check_mismatches1", "check_mismatches2"))



################################################################################
# Section 5: Create and export the finalised Step 3 dataset.

all_final_coding_step5 <- all_final_coding_step4 %>% 
  left_join(files_exist_final_concat, by = c("study_id", "resolved_url_cleaned"))

# Check: do all records with a value for resolved_url have a files_exist value?
check_has_files_exist1 <- all_final_coding_step5 %>% 
  filter(is.na(resolved_url_cleaned) == FALSE, files_exist != "yes",
         files_exist != "no", files_exist != "other")

# Check: do all records with a yes/other value for link_work have a files_exist
# value?
check_has_files_exist2 <- all_final_coding_step5 %>% 
  filter(link_work %in% c("yes", "other"), files_exist != "yes",
         files_exist != "no", files_exist != "other")

# Check: do all records with "other" have a comment?
check_files_exist_valid <- all_final_coding_step5 %>% 
  filter((files_exist != "other" & is.na(files_exist_other) == FALSE)
         | (files_exist == "other" & is.na(files_exist_other) == TRUE))

# Show the number of each value for files_exist to ensure it makes sense.
print(table(all_final_coding_step5$files_exist, useNA = "always"))

# Export to file.
write_csv(x = all_final_coding_step5,
          path = here("data", "output", "stage1",
                      "all_final_coding_step5.csv"))
write_rds(x = all_final_coding_step5,
          path = here("data", "output", "stage1",
                      "all_final_coding_step5.rds"))