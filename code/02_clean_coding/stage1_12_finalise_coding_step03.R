################################################################################
# stage1_12_finalise_coding_step03.R
#
# This script finalises the values of link_work for all link_url finalised in
# step 2.
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

# Import the Step 2 finalised data set.
all_final_coding_step2 <- read_rds(here("data", "output", "stage1",
                                        "all_final_coding_step2.rds"))

# Import the link_url linking data set (used to link coders' link_url entries
# to cleaned values of link_url).
all_clean_coding_link_url <- read_rds(here("data", "output", "stage1",
                                           "all_clean_coding_link_url.rds"))

# Import the results of the Step 3 coding follow-up.
# NOTE: The files names here will need to be updated each time another iteration
# of step 3 follow-up occurs.
step3_followup_results_disagree <- read_csv(file = here("data", "raw",
                                                        "stage1_review",
                                                        "stage1_step3_link_work_disagree - iter04.csv"))

step3_followup_results_noother <- read_csv(file = here("data", "raw",
                                                       "stage1_review",
                                                       "stage1_step3_link_work_noother - iter04.csv"))

# Special re-coding for iteration 4:
# - change the coding for article a000330 so that steve's coding has the
#   correct link_work value of "other", rather than "yes", also need to update
#   resolved_url value as well (for me and andy).
step3_followup_results_disagree <- step3_followup_results_disagree %>% 
  mutate(link_work = case_when(
    study_id == "a000330" & coder == "steve" & link_work == "yes" ~ "other",
    TRUE ~ link_work
  )) %>% 
  mutate(link_work_noother = case_when(
    study_id == "a000330" & coder %in% c("steve", "andy")
      & link_work == "other" ~ "The link in the PDF is for a different study - correct link can be found on JBPA dataverse page",
    TRUE ~ link_work_noother
  )) %>% 
  mutate(resolved_url = case_when(
    study_id == "a000330" & coder %in% c("steve", "andy")
    & link_work == "other" ~ "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/XSRNRB",
    TRUE ~ resolved_url
  )) %>%
  mutate(use_code = case_when(
    study_id == "a000330" & coder == "steve" & link_work == "other"
      & use_code == FALSE ~ TRUE,
    TRUE ~ use_code
  ))



################################################################################
# Section 2: Check and prepare the follow-up review results

# Cases where link_work is unanimously no/other: check that none of these need
# further review. There should be none, otherwise another iteration of
# follow-up is required first.
check_noother_need_review <- step3_followup_results_noother %>% 
  filter(review_coding != FALSE)

# Cases of disagreement: check for missing use_code decisions. There should be
# no missing values.
check_disagree_missing <- step3_followup_results_disagree %>% 
  filter(is.na(use_code) == TRUE)

# Cases of disagreement: check for cases where all use_code values are FALSE
# for a url. This should not happen - expected that additional coding or
# re-coding would happen first before getting to this stage.
check_disagree_allF <- step3_followup_results_disagree %>% 
  group_by(study_id, link_url_cleaned) %>% 
  filter(all(use_code == FALSE))

# Cases of disagreement: check for cases where coder steve has a value of
# FALSE for use_code. In general, this should not happen - expected that I
# can code the correct/desired coding (or re-code before this point in the
# case of a genuine error).
check_disagree_steveF <- step3_followup_results_disagree %>% 
  filter(coder == "steve", use_code != TRUE)

# Cases of disagreement: are there cases where more than one record for each
# study_id/link combo is use_code TRUE?
check_disagree_manyT <- step3_followup_results_disagree %>% 
  group_by(study_id, link_url_cleaned, use_code) %>% 
  count() %>% 
  filter(use_code == TRUE, n > 1) %>% 
  ungroup() %>% 
  select(-use_code, -n) %>% 
  inner_join(step3_followup_results_disagree, by = c("study_id",
                                                     "link_url_cleaned"))

# Check: are the values of link_work consistent between records with use_code
# TRUE?
# There should be zero cases here - if multiple records within a study/link
# combo have use_code TRUE, then they need to have the same value of link_work.
# If they don't then there has been a coding review error.
check_disagree_consistent <- check_disagree_manyT %>% 
  group_by(study_id, link_url_cleaned, use_code) %>% 
  filter(use_code == TRUE) %>% 
  filter(first(link_work) != link_work)

# Clean up checking objects.
rm(list = c("check_noother_need_review", "check_disagree_missing",
            "check_disagree_allF", "check_disagree_steveF",
            "check_disagree_manyT", "check_disagree_consistent"))

# Create a list of the final link_work disagreement resolutions.
# Problem with duplicates remaining when using distinct() - issue with
# requiring link_work_noother to be the same for cases of "other" - happens
# for article a000518 where both andy and steve have "other" but different
# comments in link_work_noother.
# OLD:
# step3_disagree_resolutions <- step3_followup_results_disagree %>% 
#   filter(use_code == TRUE) %>% 
#   select(study_id, link_url_cleaned, link_work, link_work_noother) %>% 
#   distinct()
step3_disagree_resolutions <- step3_followup_results_disagree %>% 
  arrange(study_id, link_url_cleaned, coder) %>% 
  filter(use_code == TRUE) %>% 
  group_by(study_id, link_url_cleaned) %>% 
  slice_tail() %>% 
  ungroup() %>% 
  select(study_id, link_url_cleaned, link_work, link_work_noother)



################################################################################
# Section 3: Merge coders' coding with step 2 finalised data.

# Get all coders' link_work and link_work_noother codings.
all_clean_coding_link_work <- all_clean_coding %>% 
  select(study_id, coder, link_number, link_url, link_work,
         link_work_noother)

# Quick check: can all cases in the link_url dataset still be found in the all
# clean coding dataset? The answer should definitely be yes - if not, then 
# something is deeply wrong, recheck Step 2 finalisation.
check_mismatches <- all_clean_coding_link_url %>% 
  anti_join(all_clean_coding_link_work, by = c("study_id", "coder",
                                               "link_number", "link_url"))

# Restrict the coding records to those which were confirmed as actually having
# link_url values to check back at the finalisation of Step 2.
all_clean_coding_link_work2 <- all_clean_coding_link_url %>% 
  left_join(all_clean_coding_link_work, by = c("study_id", "coder",
                                               "link_number", "link_url"))

# Get all coding cases where all coders agreed link_work = "yes".
coding_link_work_allyes <- all_clean_coding_link_work2 %>% 
  group_by(study_id, link_url_cleaned) %>% 
  filter(all(link_work == "yes"))

# Get all coding cases where all coders agreed link_work = "no".
coding_link_work_allno <- all_clean_coding_link_work2 %>% 
  group_by(study_id, link_url_cleaned) %>% 
  filter(all(link_work == "no"))

# Get all coding cases where all coders agreed link_work = "other".
coding_link_work_allother <- all_clean_coding_link_work2 %>% 
  group_by(study_id, link_url_cleaned) %>% 
  filter(all(link_work == "other"))

# Get all coding cases where coders disagreed on link_work.
coding_link_work_disagree <- all_clean_coding_link_work2 %>% 
  group_by(study_id, link_url_cleaned) %>% 
  filter(!all(link_work == "yes"), !all(link_work == "no"),
         !all(link_work == "other"))

# Check: are there any mismatches between the "leftover" disagreement records
# and the imported Step 3 follow-up review results?
check_mismatches1 <- coding_link_work_disagree %>% 
  anti_join(step3_followup_results_disagree,
            by = c("study_id", "coder", "link_url"))
check_mismatches2 <- step3_followup_results_disagree %>% 
  anti_join(coding_link_work_disagree,
            by = c("study_id", "coder", "link_url"))

# Clean up.
rm(list = c("check_mismatches", "check_mismatches1", "check_mismatches2"))



################################################################################
# Section 4: Create datasets containing the finalised values of link_work and
# link_work_noother for each study_id/link_url_cleaned combination.

link_work_final_yes <- coding_link_work_allyes %>% 
  slice_head() %>% 
  ungroup() %>% 
  select(study_id, link_url_cleaned, link_work, link_work_noother)

link_work_final_no <- coding_link_work_allno %>% 
  slice_head() %>% 
  ungroup() %>% 
  select(study_id, link_url_cleaned, link_work, link_work_noother)

link_work_final_other <- coding_link_work_allother %>% 
  slice_head() %>% 
  ungroup() %>% 
  select(study_id, link_url_cleaned, link_work, link_work_noother)

# Combine the records into a single dataset.
link_work_final_concat <- bind_rows(link_work_final_yes,
                                    link_work_final_no,
                                    link_work_final_other,
                                    step3_disagree_resolutions)

# Check: are there any duplicates in the dataset?
check_duplicates <- link_work_final_concat %>% 
  group_by(study_id, link_url_cleaned) %>% 
  count() %>% 
  filter(n > 1)

# Check: can all records be found in the Step 2 finalised data set?
check_mismatches1 <- link_work_final_concat %>% 
  anti_join(all_final_coding_step2, by = c("study_id", "link_url_cleaned"))

# Check: are all records in Step 2 finalised data set that can't be found in
# the link_work dataset those records without link_url values?
check_mismatches2 <- all_final_coding_step2 %>% 
  anti_join(link_work_final_concat, by = c("study_id", "link_url_cleaned"))

# Clean up.
rm(list = c("check_duplicates", "check_mismatches1", "check_mismatches2"))



################################################################################
# Section 5: Create and export the finalised Step 3 dataset.

all_final_coding_step3 <- all_final_coding_step2 %>% 
  left_join(link_work_final_concat, by = c("study_id", "link_url_cleaned"))

# Check: do all records with a value for link_url have a link_work value?
check_has_link_work1 <- all_final_coding_step3 %>% 
  filter(is.na(link_url_cleaned) == FALSE, link_work != "yes",
         link_work != "no", link_work != "other")

# Check: do all records with a yes/other value for has_links have a link_work
# value?
check_has_link_work2 <- all_final_coding_step3 %>% 
  filter(article_has_links %in% c("yes", "other"), link_work != "yes",
         link_work != "no", link_work != "other")

# Check: do all records with no/other have a comment?
check_link_work_valid <- all_final_coding_step3 %>% 
  filter((link_work == "yes" & is.na(link_work_noother) == FALSE)
         | (link_work != "yes" & is.na(link_work_noother) == TRUE))

# Show the number of each value for link_work to ensure it makes sense.
print(table(all_final_coding_step3$link_work, useNA = "always"))

# Export to file.
write_csv(x = all_final_coding_step3,
          path = here("data", "output", "stage1",
                      "all_final_coding_step3.csv"))
write_rds(x = all_final_coding_step3,
          path = here("data", "output", "stage1",
                      "all_final_coding_step3.rds"))