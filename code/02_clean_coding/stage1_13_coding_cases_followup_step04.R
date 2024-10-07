################################################################################
# stage1_13_coding_cases_followup_step04.R
#
# This script uses the results of step 3 of the reviewing/finalising process to
# compare coders' responses at the resolved_url level within each article. Any
# discrepancies found between the links identified by coders are output for
# follow-up.

# Load required packages.
library(tidyverse)
library(here)



################################################################################
# Section 1: Import datasets.

# Import the coders' cleaned data set.
all_clean_coding <- readr::read_rds(here("data", "output", "stage1",
                                         "all_clean_coding.rds"))

# Import the finalised step 3 data for the articles.
all_final_coding_step3 <- read_rds(here("data", "output", "stage1",
                                        "all_final_coding_step3.rds"))

# Import the link_url linking data set (used to link coders' link_url entries
# to cleaned values of link_url).
all_clean_coding_link_url <- read_rds(here("data", "output", "stage1",
                                           "all_clean_coding_link_url.rds"))



################################################################################
# Section 2: Determine which Step 3 records are expected to have resolved_urls

# Keep only those articles whose link_work status is "yes" or "other".
link_urls_to_match <- all_final_coding_step3 %>% 
  filter(link_work %in% c("yes", "other")) %>% 
  select(study_id, link_url_cleaned)



################################################################################
# Section 3: Determine which coding records to keep for matching.

# Drop all codings where a coder has answered "no" to the article_has_links
# item - by definition, these codings will not have urls to compare (confirm).
check_no_link_work_with_resurl <- all_clean_coding %>% 
  filter(link_work == "no" & is.na(resolved_url) == FALSE)

# Determine the coding record to match.
coding_to_match <- all_clean_coding %>% 
  filter(link_work %in% c("yes", "other")) %>% 
  inner_join(all_clean_coding_link_url, by = c("study_id", "coder",
                                               "link_number", "link_url")) %>% 
  arrange(study_id, link_url_cleaned, coder)

# Create the set of codings matched to the final Step 3 link_url values.
matched_coding <- link_urls_to_match %>% 
  left_join(coding_to_match, by = c("study_id", "link_url_cleaned")) %>% 
  arrange(study_id, link_url_cleaned, coder)



################################################################################
# Section 4: Clean the resolved_url field to allow for better matching.

matched_coding_clean <- matched_coding %>% 
  mutate(resolved_url_cleaned = resolved_url) %>% 
  mutate(resolved_url_cleaned = case_when(
    substr(resolved_url_cleaned, 1, 8) == "https://" ~ sub("https://", "", resolved_url_cleaned),
    substr(resolved_url_cleaned, 1, 7) == "http://" ~ sub("http://", "", resolved_url_cleaned),
    TRUE ~ resolved_url_cleaned
  )) %>%
  mutate(resolved_url_cleaned = case_when(
    substr(resolved_url_cleaned, nchar(resolved_url_cleaned), nchar(resolved_url_cleaned)) %in% c("/") ~ substr(resolved_url_cleaned, 1, nchar(resolved_url_cleaned) - 1),
    TRUE ~ resolved_url_cleaned
  ))



################################################################################
# Section 5: Determine which link_urls have a single coder, and which have
# multiple coders.

# Ensure no coders are double-counted within each study_id and link_url_cleaned
# combination.
check_coders_per_link_url <- matched_coding_clean %>% 
  group_by(study_id, link_url_cleaned, coder) %>% 
  count()
print(table(check_coders_per_link_url$n, useNA = "always"))

num_coders_per_link_url <- matched_coding_clean %>% 
  group_by(study_id, link_url_cleaned) %>% 
  count(name = "n_coders_link_url")

# Get the cases where a link_url has only a single coder.
coders_per_link_url_single <- num_coders_per_link_url %>% 
  filter(n_coders_link_url < 2) %>% 
  select(-n_coders_link_url) %>% 
  ungroup()

# Get the cases where a link_url has multiple coders.
coders_per_link_url_many <- num_coders_per_link_url %>% 
  filter(n_coders_link_url > 1) %>% 
  select(-n_coders_link_url) %>% 
  ungroup()



################################################################################
# Section 6: Determine which link_urls have agreement over resolved_urls, and
# which have disagreement (where there are many coders)

resolved_url_agree <- matched_coding_clean %>% 
  inner_join(coders_per_link_url_many, by = c("study_id",
                                              "link_url_cleaned")) %>% 
  group_by(study_id, link_url_cleaned) %>% 
  filter(all(resolved_url_cleaned == first(resolved_url_cleaned))) %>% 
  ungroup() %>% 
  arrange(study_id, link_url_cleaned, coder)

resolved_url_disagree <- matched_coding_clean %>% 
  inner_join(coders_per_link_url_many, by = c("study_id",
                                              "link_url_cleaned")) %>% 
  group_by(study_id, link_url_cleaned) %>% 
  filter(any(resolved_url_cleaned != first(resolved_url_cleaned))) %>% 
  ungroup() %>% 
  arrange(study_id, link_url_cleaned, coder)



################################################################################
# Section 7: For the cases of agreement, filter out those where the resolved
# url does not match the cleaned link url value, just as an additional check.

resolved_url_agree_diff <- resolved_url_agree %>% 
  filter(link_url_cleaned != resolved_url_cleaned)



################################################################################
# Section 8: Get the coding of those cases where a single coder coded a link
# url

# Pull out the coding records of the links coded by a single coder, to
# determine which coder(s) they are.
resolved_url_single <- matched_coding_clean %>% 
  inner_join(coders_per_link_url_single, by = c("study_id",
                                              "link_url_cleaned"))

# Get the original coding (including all coders) for articles with a link_url
# coded by a single coder (other than steve)
coding_to_check_single <- resolved_url_single %>% 
  filter(coder != "steve") %>% 
  distinct(study_id) %>% 
  left_join(all_clean_coding, by = "study_id") %>% 
  left_join(all_clean_coding_link_url, by = c("study_id", "coder",
                                              "link_number", "link_url"))

# Get the original coding (including all coders) for articles with a link_url
# coded by steve only
coding_to_check_steve <- resolved_url_single %>% 
  filter(coder == "steve") %>% 
  distinct(study_id) %>% 
  left_join(all_clean_coding, by = "study_id") %>% 
  left_join(all_clean_coding_link_url, by = c("study_id", "coder",
                                              "link_number", "link_url"))



################################################################################
# Section 9: Format and output the datasets for checking

# Import bibliographic information about each article.
bib_info <- read_rds(path = here("data", "output",
                                 "osb_lib_with_study_id.rds")) %>% 
  select(study_id, `Publication Title`, DOI, `Publication Year`, Author, Title)

# Disagreement on resolved_url among many coders
forexport_disagree <- resolved_url_disagree %>% 
  relocate(link_url_cleaned, .after = link_url_for_cleaning) %>% 
  mutate(use_code = NA_character_, review_notes = NA_character_,
         corrected_resolved_url = NA_character_) %>% 
  left_join(bib_info, by = "study_id") %>% 
  arrange(study_id, link_url_cleaned, coder)

# Agreement among many coders, but link_url differs from resolved_url.
forexport_different <- resolved_url_agree_diff %>% 
  relocate(link_url_cleaned, .after = link_url_for_cleaning) %>% 
  mutate(review_coding = NA_character_, review_notes = NA_character_) %>% 
  left_join(bib_info, by = "study_id") %>% 
  arrange(study_id, link_url_cleaned, coder)

# link_urls coded by a single coder, who is not steve.
forexport_single <- coding_to_check_single %>% 
  mutate(review_coding = NA_character_, review_notes = NA_character_) %>% 
  left_join(bib_info, by = "study_id") %>% 
  arrange(study_id, link_url_cleaned, coder)

# link_urls coded by coder steve only.
forexport_steve <- coding_to_check_steve %>% 
  mutate(review_coding = NA_character_, review_notes = NA_character_) %>% 
  left_join(bib_info, by = "study_id") %>% 
  arrange(study_id, link_url_cleaned, coder)



################################################################################
# Section 10: Prepare iter02 files by incorporating review done for iter01
#
# This section imports the completed iter01 review files, and updates the
# values for:
# - use_code/review_notes/corrected_resolved_url (when coders disagree on the
# resolved url value)
# - review_coding/review_notes (link_url and resolved_url are different)
# - single-coder urls (coders who are not steve)
# - single-coder urls (coders who are steve)
#
# Then, iter02 files can be exported, "pre-filled" with decisions already made.

# Import the completed Step 4 iter01 follow-up files.
iter01_different <- readr::read_csv(file = here("data", "raw", "stage1_review",
                                                "stage1_step4_resolved_url_different - iter01.csv"))

iter01_disagree <- readr::read_csv(file = here("data", "raw", "stage1_review",
                                                "stage1_step4_resolved_url_disagreement - iter01.csv"))

iter01_single <- readr::read_csv(file = here("data", "raw", "stage1_review",
                                                "stage1_step4_resolved_url_singlecoder_notsteve - iter01.csv"))

iter01_steve <- readr::read_csv(file = here("data", "raw", "stage1_review",
                                                "stage1_step4_resolved_url_singlecoder_steve - iter01.csv"))

########################################
# Cases where link_url differs from resolved_url

# Check how many of the iter01 file can be found in the iter02 file.
check_different_match <- iter01_different %>% 
  semi_join(forexport_different, by = c("study_id", "coder",
                                        "link_url_cleaned",
                                        "resolved_url_cleaned"))

# Check which of the iter01 records can't be found in iter02.
# (These are likely cases of records that have been dropped since the last
# iteration, if there are any at all.)
check_different_iter01only <- iter01_different %>% 
  anti_join(forexport_different, by = c("study_id", "coder",
                                        "link_url_cleaned",
                                        "resolved_url_cleaned"))

# Additional check - can these records be found in the final step 3 coding at
# all?
check_different_i01_in_s3 <- all_final_coding_step3 %>% 
  semi_join(check_different_iter01only, by = c("study_id", "link_url_cleaned"))

# Check which of the iter02 records can't be found in iter01.
# (These will be cases which were changed or added since the previous
# iteration.)
check_different_iter02only <- forexport_different %>% 
  anti_join(iter01_different, by = c("study_id", "coder",
                                        "link_url_cleaned",
                                        "resolved_url_cleaned"))

# Format and export iter02 of the different dataset to file for follow-up.
forexport_different_iter02 <- iter01_different %>% 
  select(study_id, coder, link_url_cleaned, resolved_url_cleaned,
         review_coding, review_notes) %>% 
  right_join(forexport_different, by = c("study_id", "coder",
                                         "link_url_cleaned",
                                         "resolved_url_cleaned")) %>% 
  select(-review_coding.y, -review_notes.y) %>% 
  rename(review_coding=review_coding.x, review_notes=review_notes.x) %>% 
  relocate(link_url_cleaned, .after=link_url_for_cleaning) %>% 
  relocate(resolved_url_cleaned, .after=link_url_cleaned) %>% 
  relocate(review_coding, review_notes, .after=resolved_url_cleaned) %>% 
  arrange(study_id, resolved_url_cleaned, coder)

# Clean up.
rm(list = c("check_different_match", "check_different_iter01only",
            "check_different_i01_in_s3", "check_different_iter02only"))


########################################
# Cases where coders disagree on the resolved_url value.

# Check how many of the iter01 file can be found in the iter02 file.
check_disagree_match <- iter01_disagree %>% 
  semi_join(forexport_disagree, by = c("study_id", "coder",
                                       "link_url_cleaned",
                                       "resolved_url_cleaned"))

# Check which of the iter01 records can't be found in iter02.
# (These are likely cases of records that have been dropped since the last
# iteration, if there are any at all.)
check_disagree_iter01only <- iter01_disagree %>% 
  anti_join(forexport_disagree, by = c("study_id", "coder",
                                       "link_url_cleaned",
                                       "resolved_url_cleaned"))

# Additional check - can these records be found in the final step 3 coding at
# all?
check_disagree_i01_in_s3 <- all_final_coding_step3 %>% 
  semi_join(check_disagree_iter01only, by = c("study_id", "link_url_cleaned"))

# Check which of the iter02 records can't be found in iter01.
# (These will be cases which were changed or added since the previous
# iteration.)
check_disagree_iter02only <- forexport_disagree %>% 
  anti_join(iter01_disagree, by = c("study_id", "coder",
                                    "link_url_cleaned",
                                    "resolved_url_cleaned"))

# Format and export iter02 of the disagree dataset to file for follow-up.
forexport_disagree_iter02 <- iter01_disagree %>% 
  select(study_id, coder, link_url_cleaned, resolved_url_cleaned,
         use_code, review_notes, corrected_resolved_url) %>% 
  right_join(forexport_disagree, by = c("study_id", "coder",
                                        "link_url_cleaned",
                                        "resolved_url_cleaned")) %>% 
  select(-use_code.y, -review_notes.y, -corrected_resolved_url.y) %>% 
  rename(use_code=use_code.x,
         review_notes=review_notes.x,
         corrected_resolved_url=corrected_resolved_url.x) %>% 
  relocate(link_url_cleaned,
           .after=link_url_for_cleaning) %>% 
  relocate(resolved_url_cleaned,
           .after=link_url_cleaned) %>% 
  relocate(use_code, review_notes, corrected_resolved_url,
           .after=resolved_url_cleaned) %>% 
  arrange(study_id, resolved_url_cleaned, coder)

# Clean up.
rm(list = c("check_different_match", "check_different_iter01only",
            "check_different_i01_in_s3", "check_different_iter02only"))


########################################
# Cases where a resolved_url has a single coder (who is not steve).

# There are no cases of a resolved_url with a single coder (other than steve)
# in iter02.

########################################
# Cases where a resolved_url has a single coder (who is steve).

# Check how many of the iter01 file can be found in the iter02 file.
check_steve_match <- iter01_steve %>% 
  semi_join(forexport_steve, by = c("study_id", "coder",
                                        "link_url_cleaned"))

# Check which of the iter01 records can't be found in iter02.
# (These are likely cases of records that have been dropped since the last
# iteration, if there are any at all.)
check_steve_iter01only <- iter01_steve %>% 
  anti_join(forexport_steve, by = c("study_id", "coder",
                                        "link_url_cleaned"))

# Additional check - can these records be found in the final step 3 coding at
# all?
check_steve_i01_in_s3 <- all_final_coding_step3 %>% 
  semi_join(check_steve_iter01only, by = c("study_id", "link_url_cleaned"))

# Check which of the iter02 records can't be found in iter01.
# (These will be cases which were changed or added since the previous
# iteration.)
check_steve_iter02only <- forexport_steve %>% 
  anti_join(iter01_steve, by = c("study_id", "coder",
                                     "link_url_cleaned"))

# Format and export iter02 of the steve dataset to file for follow-up.
forexport_steve_iter02 <- iter01_steve %>% 
  select(study_id, coder, link_url, review_coding, review_notes) %>% 
  right_join(forexport_steve, by = c("study_id", "coder",
                                         "link_url")) %>% 
  select(-review_coding.y, -review_notes.y) %>% 
  rename(review_coding=review_coding.x, review_notes=review_notes.x) %>% 
  relocate(link_url, .after=link_number) %>% 
  relocate(review_coding, review_notes, .after=link_url_cleaned) %>% 
  arrange(study_id, link_url_cleaned, coder)

# Clean up.
rm(list = c("check_steve_match", "check_steve_iter01only",
            "check_steve_i01_in_s3", "check_steve_iter02only"))


########################################
# Export to file for manual follow-up and review
write_csv(x = forexport_disagree_iter02,
          path = here("data", "output", "stage1", "follow_up",
                      "step04_follow_up_disagreement_iter02.csv"))
write_rds(x = forexport_disagree_iter02,
          path = here("data", "output", "stage1", "follow_up",
                      "step04_follow_up_disagreement_iter02.rds"))

write_csv(x = forexport_different_iter02,
          path = here("data", "output", "stage1", "follow_up",
                      "step04_follow_up_links_different_iter02.csv"))
write_rds(x = forexport_different_iter02,
          path = here("data", "output", "stage1", "follow_up",
                      "step04_follow_up_links_different_iter02.rds"))

write_csv(x = forexport_single,
          path = here("data", "output", "stage1", "follow_up",
                      "step04_follow_up_single_coder_not_steve_iter02.csv"))
write_rds(x = forexport_single,
          path = here("data", "output", "stage1", "follow_up",
                      "step04_follow_up_single_coder_not_steve_iter02.rds"))

write_csv(x = forexport_steve_iter02,
          path = here("data", "output", "stage1", "follow_up",
                      "step04_follow_up_single_coder_steve_iter02.csv"))
write_rds(x = forexport_steve_iter02,
          path = here("data", "output", "stage1", "follow_up",
                      "step04_follow_up_single_coder_steve_iter02.rds"))



################################################################################
# Section 11: Prepare iter03 files by incorporating review done for iter02
#
# This section imports the completed iter02 review files, and updates the
# values for:
# - use_code/review_notes/corrected_resolved_url (when coders disagree on the
# resolved url value)
# - review_coding/review_notes (link_url and resolved_url are different)
# - single-coder urls (coders who are not steve)
# - single-coder urls (coders who are steve)
#
# Then, iter03 files can be exported, "pre-filled" with decisions already made.

# Import the completed Step 4 iter02 follow-up files.
iter02_different <- readr::read_csv(file = here("data", "raw", "stage1_review",
                                                "stage1_step4_resolved_url_different - iter02.csv"))

iter02_disagree <- readr::read_csv(file = here("data", "raw", "stage1_review",
                                               "stage1_step4_resolved_url_disagreement - iter02.csv"))

iter02_single <- readr::read_csv(file = here("data", "raw", "stage1_review",
                                             "stage1_step4_resolved_url_singlecoder_notsteve - iter02.csv"))

iter02_steve <- readr::read_csv(file = here("data", "raw", "stage1_review",
                                            "stage1_step4_resolved_url_singlecoder_steve - iter02.csv"))

########################################
# Cases where link_url differs from resolved_url

# Check how many of the iter02 file can be found in the iter03 file.
check_different_match <- iter02_different %>% 
  semi_join(forexport_different, by = c("study_id", "coder",
                                        "link_url_cleaned",
                                        "resolved_url_cleaned"))

# Check which of the iter02 records can't be found in iter03.
# (These are likely cases of records that have been dropped since the last
# iteration, if there are any at all.)
check_different_iter02only <- iter02_different %>% 
  anti_join(forexport_different, by = c("study_id", "coder",
                                        "link_url_cleaned",
                                        "resolved_url_cleaned"))

# Additional check - can these records be found in the final step 3 coding at
# all?
check_different_i02_in_s3 <- all_final_coding_step3 %>% 
  semi_join(check_different_iter02only, by = c("study_id", "link_url_cleaned"))

# Check which of the iter03 records can't be found in iter02.
# (These will be cases which were changed or added since the previous
# iteration.)
check_different_iter03only <- forexport_different %>% 
  anti_join(iter02_different, by = c("study_id", "coder",
                                     "link_url_cleaned",
                                     "resolved_url_cleaned"))

# Format and export iter03 of the different dataset to file for follow-up.
forexport_different_iter03 <- iter02_different %>% 
  select(study_id, coder, link_url_cleaned, resolved_url_cleaned,
         review_coding, review_notes) %>% 
  right_join(forexport_different, by = c("study_id", "coder",
                                         "link_url_cleaned",
                                         "resolved_url_cleaned")) %>% 
  select(-review_coding.y, -review_notes.y) %>% 
  rename(review_coding=review_coding.x, review_notes=review_notes.x) %>% 
  relocate(link_url_cleaned, .after=link_url_for_cleaning) %>% 
  relocate(resolved_url_cleaned, .after=link_url_cleaned) %>% 
  relocate(review_coding, review_notes, .after=resolved_url_cleaned) %>% 
  arrange(study_id, resolved_url_cleaned, coder)

# Clean up.
rm(list = c("check_different_match", "check_different_iter02only",
            "check_different_i02_in_s3", "check_different_iter03only"))


########################################
# Cases where coders disagree on the resolved_url value.

# Check how many of the iter02 file can be found in the iter03 file.
check_disagree_match <- iter02_disagree %>% 
  semi_join(forexport_disagree, by = c("study_id", "coder",
                                       "link_url_cleaned",
                                       "resolved_url_cleaned"))

# Check which of the iter02 records can't be found in iter03.
# (These are likely cases of records that have been dropped since the last
# iteration, if there are any at all.)
check_disagree_iter02only <- iter02_disagree %>% 
  anti_join(forexport_disagree, by = c("study_id", "coder",
                                       "link_url_cleaned",
                                       "resolved_url_cleaned"))

# Additional check - can these records be found in the final step 3 coding at
# all?
check_disagree_i02_in_s3 <- all_final_coding_step3 %>% 
  semi_join(check_disagree_iter02only, by = c("study_id", "link_url_cleaned"))

# Check which of the iter03 records can't be found in iter02.
# (These will be cases which were changed or added since the previous
# iteration.)
check_disagree_iter03only <- forexport_disagree %>% 
  anti_join(iter02_disagree, by = c("study_id", "coder",
                                    "link_url_cleaned",
                                    "resolved_url_cleaned"))

# Format and export iter03 of the disagree dataset to file for follow-up.
forexport_disagree_iter03 <- iter02_disagree %>% 
  select(study_id, coder, link_url_cleaned, resolved_url_cleaned,
         use_code, review_notes, corrected_resolved_url) %>% 
  right_join(forexport_disagree, by = c("study_id", "coder",
                                        "link_url_cleaned",
                                        "resolved_url_cleaned")) %>% 
  select(-use_code.y, -review_notes.y, -corrected_resolved_url.y) %>% 
  rename(use_code=use_code.x,
         review_notes=review_notes.x,
         corrected_resolved_url=corrected_resolved_url.x) %>% 
  relocate(link_url_cleaned,
           .after=link_url_for_cleaning) %>% 
  relocate(resolved_url_cleaned,
           .after=link_url_cleaned) %>% 
  relocate(use_code, review_notes, corrected_resolved_url,
           .after=resolved_url_cleaned) %>% 
  arrange(study_id, resolved_url_cleaned, coder)

# Clean up.
rm(list = c("check_disagree_match", "check_disagree_iter02only",
            "check_disagree_i02_in_s3", "check_disagree_iter03only"))


########################################
# Cases where a resolved_url has a single coder (who is not steve).

# There are no cases of a resolved_url with a single coder (other than steve)
# in iter03.

########################################
# Cases where a resolved_url has a single coder (who is steve).

# Check how many of the iter02 file can be found in the iter03 file.
check_steve_match <- iter02_steve %>% 
  semi_join(forexport_steve, by = c("study_id", "coder",
                                    "link_url_cleaned"))

# Check which of the iter02 records can't be found in iter03.
# (These are likely cases of records that have been dropped since the last
# iteration, if there are any at all.)
check_steve_iter02only <- iter02_steve %>% 
  anti_join(forexport_steve, by = c("study_id", "coder",
                                    "link_url_cleaned"))

# Additional check - can these records be found in the final step 3 coding at
# all?
check_steve_i02_in_s3 <- all_final_coding_step3 %>% 
  semi_join(check_steve_iter02only, by = c("study_id", "link_url_cleaned"))

# Check which of the iter03 records can't be found in iter02.
# (These will be cases which were changed or added since the previous
# iteration.)
check_steve_iter03only <- forexport_steve %>% 
  anti_join(iter02_steve, by = c("study_id", "coder",
                                 "link_url_cleaned"))

# Format and export iter03 of the steve dataset to file for follow-up.
forexport_steve_iter03 <- iter02_steve %>% 
  select(study_id, coder, link_url, review_coding, review_notes) %>% 
  right_join(forexport_steve, by = c("study_id", "coder",
                                     "link_url")) %>% 
  select(-review_coding.y, -review_notes.y) %>% 
  rename(review_coding=review_coding.x, review_notes=review_notes.x) %>% 
  relocate(link_url, .after=link_number) %>% 
  relocate(review_coding, review_notes, .after=link_url_cleaned) %>% 
  arrange(study_id, link_url_cleaned, coder)

# Clean up.
rm(list = c("check_steve_match", "check_steve_iter02only",
            "check_steve_i02_in_s3", "check_steve_iter03only"))


########################################
# Export to file for manual follow-up and review
write_csv(x = forexport_disagree_iter03,
          path = here("data", "output", "stage1", "follow_up",
                      "step04_follow_up_disagreement_iter03.csv"))
write_rds(x = forexport_disagree_iter03,
          path = here("data", "output", "stage1", "follow_up",
                      "step04_follow_up_disagreement_iter03.rds"))

write_csv(x = forexport_different_iter03,
          path = here("data", "output", "stage1", "follow_up",
                      "step04_follow_up_links_different_iter03.csv"))
write_rds(x = forexport_different_iter03,
          path = here("data", "output", "stage1", "follow_up",
                      "step04_follow_up_links_different_iter03.rds"))

write_csv(x = forexport_single,
          path = here("data", "output", "stage1", "follow_up",
                      "step04_follow_up_single_coder_not_steve_iter03.csv"))
write_rds(x = forexport_single,
          path = here("data", "output", "stage1", "follow_up",
                      "step04_follow_up_single_coder_not_steve_iter03.rds"))

write_csv(x = forexport_steve_iter03,
          path = here("data", "output", "stage1", "follow_up",
                      "step04_follow_up_single_coder_steve_iter03.csv"))
write_rds(x = forexport_steve_iter03,
          path = here("data", "output", "stage1", "follow_up",
                      "step04_follow_up_single_coder_steve_iter03.rds"))