################################################################################
# stage1_14_finalise_coding_step04.R
#
# This script makes use of the manually cleaned resolved_url follow-up files
# to attach a definitive value of resolved_url to each working link_url from
# step 3 of the data set.

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
# Section 2: Import the latest iteration of follow-up datasets.
#
# Be sure to update to the latest iteration in the import statements whenever
# this code is re-run.

# Import the "different" dataset.
followup_different <- readr::read_csv(file = here("data", "raw",
                                                  "stage1_review",
                                                  "stage1_step4_resolved_url_different - iter03.csv"))

# Import the "disagree" dataset.
followup_disagree <- readr::read_csv(file = here("data", "raw",
                                                 "stage1_review",
                                                 "stage1_step4_resolved_url_disagreement - iter03.csv"))

# Import the "single coder, not steve" dataset.
followup_notsteve <- readr::read_csv(file = here("data", "raw",
                                                 "stage1_review",
                                                 "stage1_step4_resolved_url_singlecoder_notsteve - iter03.csv"))

# Import the "single coder, steve" dataset.
followup_steve <- readr::read_csv(file = here("data", "raw",
                                              "stage1_review",
                                              "stage1_step4_resolved_url_singlecoder_steve - iter03.csv"))



################################################################################
# Section 3: Determine the set of coding(s) to be used for establishing
# resolved_url values.

# Pull out only those final step 3 records which have link(s).
step3_with_links <- all_final_coding_step3 %>% 
  filter(article_has_links %in% c("yes", "other"))

# Attach the cleaned link_url to the cleaned coding data.
# Reduce to those which can be found in final coding step 3 and have links.
temp1 <- all_clean_coding %>% 
  left_join(all_clean_coding_link_url,
            by = c("study_id", "coder", "link_number", "link_url")) %>% 
  semi_join(step3_with_links,
            by = c("study_id", "link_url_cleaned"))



################################################################################
# Section 4: Prepare the resolved disagreements data.

# Check: Do all article/link combos have at least one use_code value of TRUE?
# If there are cases here, check that at least one record has a value for
# corrected_resolved_url.
check_all_use_codeF <- followup_disagree %>% 
  group_by(study_id, link_url_cleaned) %>% 
  filter(all(use_code == FALSE))

# Split the disagreement records into three groups:
# - use_code TRUE, which means use resolved_url as it is;
# - use_code FALSE and corrected_resolved_url non-empty, which means correct
#   the coder's resolved_url value;
# - use_code FALSE and corrected_resolved_url empty, which means drop the
#   coder's record entirely.

keep_resolved_url <- followup_disagree %>% 
  filter(use_code == TRUE)

update_resolved_url <- followup_disagree %>% 
  filter(use_code == FALSE & !is.na(corrected_resolved_url))

drop_resolved_url <- followup_disagree %>% 
  filter(use_code == FALSE & is.na(corrected_resolved_url))

# Quick check: are there any left-over records in the disagreement dataset?
check_disagree_leftover <- followup_disagree %>% 
  anti_join(keep_resolved_url,
            by = c("study_id", "coder", "link_url_cleaned",
                   "resolved_url_cleaned")) %>% 
  anti_join(update_resolved_url,
            by = c("study_id", "coder", "link_url_cleaned",
                   "resolved_url_cleaned")) %>%
  anti_join(drop_resolved_url,
            by = c("study_id", "coder", "link_url_cleaned",
                   "resolved_url_cleaned"))

# Create datasets to do the patching/updating.
keep_resolved_url2 <- keep_resolved_url %>% 
  mutate(corrected_resolved_url = resolved_url) %>% 
  select(study_id, coder, link_url_cleaned, corrected_resolved_url)

update_resolved_url2 <- update_resolved_url %>% 
  select(study_id, coder, link_url_cleaned, corrected_resolved_url)

corrections_resolved_url2 <- bind_rows(keep_resolved_url2,
                                       update_resolved_url2)

drop_resolved_url2 <- drop_resolved_url %>% 
  select(study_id, coder, link_url_cleaned, resolved_url)

rm(list = c("check_all_use_codeF", "check_disagree_leftover",
            "keep_resolved_url", "update_resolved_url", "drop_resolved_url",
            "keep_resolved_url2", "update_resolved_url2"))



################################################################################
# Section 5: Create "corrected" resolved_url values and then clean them.

# Drop the records causing resolved_url disagreement that were flagged for
# removal.
temp2 <- temp1 %>% 
  anti_join(drop_resolved_url2,
            by = c("study_id", "coder", "link_url_cleaned", "resolved_url"))

# Attach the resolved_url corrections.
temp3 <- temp2 %>% 
  left_join(corrections_resolved_url2,
            by = c("study_id", "coder", "link_url_cleaned"))

# Check: Can all corrections be attached, or do some fail?
check_failed_matches <- temp3 %>% 
  filter(!is.na(corrected_resolved_url))
check_failed_matches2 <- corrections_resolved_url2 %>% 
  anti_join(check_failed_matches,
            by = c("study_id", "coder", "link_url_cleaned"))
rm(list = c("check_failed_matches", "check_failed_matches2"))

# Check: Of the coding data, where there are blank resolved_url values, do
# they correspond to link_work = no?
check_blank_resolved_url <- temp3 %>% 
  filter(is.na(resolved_url) == TRUE)
print(table(check_blank_resolved_url$link_work, useNA = "always"))

# Fill in the values for corrected_resolved_url - all records with a non-blank
# value for this will be deemed to be correct, and so their value of
# resolved_url can be transferred over as-is to corrected_resolved_url.
temp4 <- temp3 %>% 
  mutate(resolved_url_for_cleaning = case_when(
    link_work == "no" ~ NA_character_,
    is.na(corrected_resolved_url) ~ resolved_url,
    TRUE ~ corrected_resolved_url
  ))

# Check: Of the coding data, where there are blank resolved_url values, do
# they correspond to link_work = no?
check_blank_resolved_url2 <- temp4 %>% 
  filter(is.na(resolved_url_for_cleaning) == TRUE)
print(table(check_blank_resolved_url2$link_work, useNA = "always"))

# Clean the corrected resolved_url values, in preparation for matching within
# study_id/link_url_cleaned values.
temp5 <- temp4 %>% 
  mutate(resolved_url_cleaned = resolved_url_for_cleaning) %>% 
  mutate(resolved_url_cleaned = case_when(
    substr(resolved_url_cleaned, 1, 8) == "https://" ~ sub("https://", "", resolved_url_cleaned),
    substr(resolved_url_cleaned, 1, 7) == "http://" ~ sub("http://", "", resolved_url_cleaned),
    TRUE ~ resolved_url_cleaned
  )) %>%
  mutate(resolved_url_cleaned = case_when(
    substr(resolved_url_cleaned, nchar(resolved_url_cleaned), nchar(resolved_url_cleaned)) %in% c("/") ~ substr(resolved_url_cleaned, 1, nchar(resolved_url_cleaned) - 1),
    TRUE ~ resolved_url_cleaned
  ))

# Check: Of the coding data, where there are blank resolved_url values, do
# they correspond to link_work = no?
check_blank_resolved_url3 <- temp5 %>% 
  filter(is.na(resolved_url_cleaned) == TRUE)
print(table(check_blank_resolved_url3$link_work, useNA = "always"))

rm(list = c("check_blank_resolved_url", "check_blank_resolved_url2",
            "check_blank_resolved_url3"))



################################################################################
# Section 6: Determine which are link_urls are single-coder and which are
# many-coder

coders_per_link <- temp5 %>% 
  group_by(study_id, link_url_cleaned) %>% 
  count()

single_coder_links <- coders_per_link %>% 
  filter(n == 1)

check_single_coder_links <- temp5 %>% 
  semi_join(single_coder_links,
            by = c("study_id", "link_url_cleaned"))



################################################################################
# Section 7: Determine if any other edits need to be made to the data, based
# on results from the other follow-up files.

# Check the single-coder (steve) follow-up file for any records that need
# review.
toreview_steve <- followup_steve %>% 
  filter(review_coding == TRUE)

# Check the "different" follow-up file for any records that need review.
toreview_different <- followup_different %>% 
  filter(review_coding == TRUE)

# Applies to iter03:
# On the basis of these results, the following needs to happen:
# - all the records found in toreview_steve need to be dropped from the data;
# - the value of resolved_url for andy & steve for a000330 (in the "different"
#   follow-up dataset) should be changed from its current value, and oliver's
#   record should be dropped.

check_not_yet_dropped <- temp5 %>% 
  semi_join(toreview_steve,
            by = c("study_id", "coder", "link_url"))

temp6 <- temp5 %>% 
  anti_join(toreview_steve,
            by = c("study_id", "coder", "link_url")) %>% 
  filter(!(coder == "oliver" & study_id == "a000330")) %>% 
  mutate(resolved_url_for_cleaning = case_when(
    study_id == "a000330"
    & coder %in% c("steve", "andy")
    & link_url_cleaned == "doi.org/10.7910/DVN/WD3IUA" ~ "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/XSRNRB",
    TRUE ~ resolved_url_for_cleaning
  )) %>%
  mutate(resolved_url_cleaned = case_when(
    study_id == "a000330"
    & coder %in% c("steve", "andy")
    & link_url_cleaned == "doi.org/10.7910/DVN/WD3IUA" ~ "dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/XSRNRB",
    TRUE ~ resolved_url_cleaned
  ))

check_corrections <- temp6 %>% 
  filter(study_id == "a000330")

# Clean up.
rm(list = c("toreview_steve", "toreview_different", "check_not_yet_dropped",
            "check_corrections"))



################################################################################
# Section 8: Check for resolved_url_cleaned mismatches.

check_resolved_url_disagree <- temp6 %>% 
  group_by(study_id, link_url_cleaned) %>% 
  filter(any(resolved_url_cleaned != first(resolved_url_cleaned))) %>% 
  ungroup() %>% 
  arrange(study_id, link_url_cleaned, coder)


################################################################################
# Section 9: Create a resolved url lookup data set.

all_clean_coding_resolved_url <- temp6 %>% 
  select(study_id, coder, link_number,
         link_url, corrected_link_url, link_url_for_cleaning, link_url_cleaned,
         resolved_url, corrected_resolved_url, resolved_url_for_cleaning,
         resolved_url_cleaned) %>% 
  arrange(study_id, link_url_cleaned, coder)

# Check: what can and can't be matched between the link_url and resolved_url
# lookup datasets?
check_link_url_only <- all_clean_coding_link_url %>% 
  anti_join(all_clean_coding_resolved_url,
            by = c("study_id", "coder", "link_number", "link_url",
                   "corrected_link_url", "link_url_for_cleaning",
                   "link_url_cleaned"))
check_resolved_url_only <- all_clean_coding_resolved_url %>% 
  anti_join(all_clean_coding_link_url,
            by = c("study_id", "coder", "link_number", "link_url",
                   "corrected_link_url", "link_url_for_cleaning",
                   "link_url_cleaned"))
rm(list = c("check_link_url_only", "check_resolved_url_only"))

# Export the resolved_url lookup dataset to file.
write_csv(x = all_clean_coding_resolved_url,
          path = here("data", "output", "stage1",
                      "all_clean_coding_resolved_url.csv"))
write_rds(x = all_clean_coding_resolved_url,
          path = here("data", "output", "stage1",
                      "all_clean_coding_resolved_url.rds"))



################################################################################
# Section 10: Create a finalised step 4 dataset.

final_resolved_urls <- temp6 %>% 
  filter(is.na(resolved_url_cleaned) == FALSE) %>% 
  distinct(study_id, link_url_cleaned, resolved_url_cleaned)

check_resolved_url_disagree2 <- final_resolved_urls %>% 
  group_by(study_id, link_url_cleaned) %>% 
  filter(any(resolved_url_cleaned != first(resolved_url_cleaned))) %>% 
  ungroup() %>% 
  arrange(study_id, link_url_cleaned)

all_final_coding_step4 <- all_final_coding_step3 %>% 
  left_join(final_resolved_urls,
            by = c("study_id", "link_url_cleaned"))

check_blank_resolved_url4 <- all_final_coding_step4 %>% 
  filter(is.na(resolved_url_cleaned))
check_link_work_no <- all_final_coding_step4 %>% 
  filter(link_work == "no")
check_no_links <- all_final_coding_step4 %>% 
  filter(article_has_links == "no")
check_blank_link_url <- all_final_coding_step4 %>% 
  filter(is.na(link_url_cleaned))

# Export to file.
write_csv(x = all_final_coding_step4,
          path = here("data", "output", "stage1",
                      "all_final_coding_step4.csv"))
write_rds(x = all_final_coding_step4,
          path = here("data", "output", "stage1",
                      "all_final_coding_step4.rds"))

# Then, separate out single-coder link_url records from many-coder records.

# For the many-coder records, match the resolved urls within link_urls - there
# should be no mismatches now.

# For the single-coder records, ensure that none are still up for review.

# Combine the single- and many-coder results.

# For cases where link_url does not match resolved_url, check that all
# have review_coding status FALSE.

# Create a data set which link coders' actual resolved urls to the cleaned
# version.

# Attach the definitive resolved url value to the step 3 data file. This
# becomes the step 4 finalised dataset.