################################################################################
# stage1_10_finalise_coding_step02.R
#
# This script makes use of the manually cleaned link_url data and the updated
# coding to determine the final set of link_url values for each article.
#
# Create a "lookup table" which gives the cleaned link_url value for each
# coder's personal link_url value.

# Load required packages.
library(tidyverse)
library(here)


################################################################################
# Section 1: Importing required data

# Import the coders' cleaned data set.
all_clean_coding <- readr::read_rds(here("data", "output", "stage1",
                                         "all_clean_coding.rds"))

# Import the finalised step 1 data for the articles.
all_final_coding_step1 <- read_rds(here("data", "output", "stage1",
                                        "all_final_coding_step1.rds"))

# Import the latest iteration of the completed link_url cleaning data.
# Note that this file name will need to be updated to match the latest file.
link_url_followup_results <- read_csv(file = here("data", "raw",
                                                  "stage1_review",
                                                  "stage1_step2_link_url_followup - iter03.csv"))



################################################################################
# Section 2: Create the dataset to be used for cleaning and checking.

# Repeat the reduction of the coding dataset that was done in the stage1_09
# code.

# Drop all articles that have been finalised as containing no links.
study_ids_to_match <- all_final_coding_step1 %>% 
  filter(article_has_links != "no") %>% 
  select(study_id)

# Determine all cases of articles where only one coder has coded it (these
# should all be cases of coder steve). These articles should be removed from
# the comparison, because there will be no other coders' urls to compare with.
single_coder_articles <- all_clean_coding %>% 
  group_by(study_id) %>% 
  distinct(coder) %>% 
  filter(n() == 1)

# Drop all codings where a coder has answered "no" to the article_has_links
# item - by definition, these codings will not have urls to compare (confirm).
check_coding_no_link_with_url <- all_clean_coding %>% 
  filter(article_has_links == "no" & is.na(link_url) == FALSE)

# Create a set of coding records with (uncorrected/cleaned) links to then do
# additional cleaning.
# (Updated to no longer exclude the single-coder articles.)
coding_has_links <- all_clean_coding %>% 
  filter(article_has_links != "no") %>% 
  inner_join(study_ids_to_match, by = "study_id")

# Clean up any unneeded objects (inspect them before deleting them to ensure
# any issues have been picked up).
rm(list = c("single_coder_articles", "check_coding_no_link_with_url"))



################################################################################
# Section 3: Incorporate the url cleaning from the follow-up dataset.

# From the link_url follow-up data, determine the links to drop.
links_to_drop <- link_url_followup_results %>% 
  filter(use_code == FALSE & is.na(corrected_link_url))

# Check that these links can be matched to the main coding dataset.
records_to_be_dropped <- links_to_drop %>% 
  select(study_id, coder, link_number, link_url, use_code, review_notes) %>% 
  inner_join(coding_has_links, by = c("study_id", "coder", "link_number",
                                      "link_url")) %>% 
  select(study_id, coder, link_number, link_url)

# Remove the records to be dropped from the coding dataset.
coding_to_check <- coding_has_links %>% 
  anti_join(records_to_be_dropped, by = c("study_id", "coder", "link_number",
                                          "link_url"))

# From the link_url follow-up data, determine the links to correct and the
# links that have been confirmed as correct.
# Perform some ad hoc cleaning to deal with a line break encoding issue.
links_to_correct <- link_url_followup_results %>% 
  mutate(corrected_link_url = ifelse(
    use_code == TRUE & is.na(corrected_link_url) == TRUE,
    link_url,
    corrected_link_url
  )) %>% 
  filter(is.na(corrected_link_url) == FALSE) %>% 
  mutate(link_url = ifelse(
    study_id == "a000591" & coder == "nguyenlam" & link_url == "https://osf.io/xytch\n",
    "https://osf.io/xytch\r",
    link_url
  ))

# Check that these links can be matched to the main coding dataset.
check_correct_match1 <- links_to_correct %>% 
  select(study_id, coder, link_number, link_url, corrected_link_url) %>% 
  inner_join(coding_to_check, by=c("study_id", "coder", "link_number",
                                   "link_url"))

# Check for any links that can't be matched to the main coding dataset.
# If this includes article a000063, that is OK - that article was reviewed,
# but has been dropped from the final dataset due to being retracted.
check_correct_match2 <- links_to_correct %>% 
  select(study_id, coder, link_number, link_url, corrected_link_url) %>% 
  anti_join(coding_to_check, by=c("study_id", "coder", "link_number",
                                  "link_url"))

# Add the corrected urls to the coding data.
coding_to_check2 <- links_to_correct %>% 
  select(study_id, coder, link_number, link_url, corrected_link_url) %>% 
  right_join(coding_to_check, by=c("study_id", "coder", "link_number",
                                   "link_url"))

# Clean up any unneeded objects (inspect them before deleting them to ensure
# any issues have been picked up).
rm(list = c("links_to_drop", "records_to_be_dropped", "links_to_correct",
            "check_correct_match1", "check_correct_match2"))



################################################################################
# Section 3.5: Remove some records from the link_url dataset.

# Article a001125: This has a link to Mendeley data on the article web page.
# All coders other than steve coded an irrelevant url. Drop those records from
# the data set.
#
# Article a001280: One of the links coded by coders matthew and michelle is
# irrelevant (it is prereg information, not data/code). Drop these coding
# records.
#
# Duplicate link_url codings by a single coder.
# Coder evelyn coded link_url "https://osf.io/ge2d5/" twice for a000596.
# Coder oliver coded link_url "https://osf.io/hd42c/" twice for a001217.
# Coder sophia coded link_url "https://osf.io/wki3y/" twice for a001308.
# In each case, drop one of the duplicate codings.
#
# Additional removals found in batch0912:
# (NOTE: after iteration 03 of step 2, I these particular in-code
# deletions are no longer necessary - keep them in here for now, they do no
# harm, because they have already been deleted.)
#
# Article a000692: This has a link to Mendeley data on the article web page.
# All coders other than steve coded an irrelevant url. Drop those records from
# the data set.
#
# Article a000769: This has a link to Mendeley data on the article web page.
# All coders other than steve coded an irrelevant url. Drop those records from
# the data set.
#
# Article a001329: All coders other than steve coded an irrelevant url. Drop
# those records from the data set.

coding_to_check2a <- coding_to_check2 %>%
  filter(!(study_id == "a001125"
           & coder %in% c("oliver", "aniruddah", "sophia"))) %>% 
  filter(!(study_id == "a001280"
           & link_url == "https://osf.io/y3zcn/"
           & coder %in% c("matthew", "michelle"))) %>% 
  filter(!(study_id == "a000596"
           & coder == "evelyn"
           & link_url == "https://osf.io/ge2d5/"
           & link_number == 2)) %>% 
  filter(!(study_id == "a001217"
           & coder == "oliver"
           & link_url == "https://osf.io/hd42c/"
           & link_number == 2)) %>% 
  filter(!(study_id == "a001308"
           & coder == "sophia"
           & link_url == "https://osf.io/wki3y/"
           & link_number == 2)) %>% 
  filter(!(study_id == "a000692"
           & coder %in% c("evelyn", "michelle"))) %>% 
  filter(!(study_id == "a000769"
           & coder %in% c("oliver", "sophia"))) %>% 
  filter(!(study_id == "a001329"
           & coder %in% c("evelyn", "kongmanas")))

# Check that the correct records were removed.
check_removed_records <- coding_to_check2 %>% 
  anti_join(coding_to_check2a, by=c("study_id", "coder", "link_number",
                            "link_url"))

# Clean up any unneeded objects (inspect them before deleting them to ensure
# any issues have been picked up).
rm(list = c("check_removed_records"))



################################################################################
# Section 4: Check the number of coders per article for articles with urls.

# Determine the number of coders per article, for the subset of articles where
# coders thought that the article had links (this might reveal that some
# articles were only coded with links once).
coders_per_article <- coding_to_check2a %>% 
  group_by(study_id) %>% 
  distinct(coder) %>% 
  count(name = "n_coders") %>% 
  ungroup()
print(table(coders_per_article$n_coders))

# Check that all of the articles with only one coder were coded by steve.
check_single_coder_articles <- coders_per_article %>% 
  filter(n_coders == 1) %>% 
  select(study_id) %>% 
  inner_join(coding_to_check2a, by = "study_id")

# Clean up any unneeded objects (inspect them before deleting them to ensure
# any issues have been picked up).
rm(list = c("coders_per_article", "check_single_coder_articles"))



################################################################################
# Section 5: Auto-clean the rest of the urls in the data set.

# Use the url cleaning syntax from stage1_09 to clean the urls, including those
# which have a corrected url.
# Keep the original link_url variable unchanged.
# Create a new variable, link_url_for_cleaning, which uses either the original
# link_url or the corrected_link_url (if present).
# Create the cleaned link_url for matching from the for_cleaning variable.
coding_to_check3 <- coding_to_check2a %>% 
  mutate(link_url_for_cleaning = ifelse(
    is.na(corrected_link_url) == TRUE,
    link_url,
    corrected_link_url
  )) %>% 
  mutate(link_url_cleaned = case_when(
    substr(link_url_for_cleaning, 1, 8) == "https://" ~ sub("https://", "", link_url_for_cleaning),
    substr(link_url_for_cleaning, 1, 7) == "http://" ~ sub("http://", "", link_url_for_cleaning),
    substr(link_url_for_cleaning, 1, 7) == "https//" ~ sub("https//", "", link_url_for_cleaning),
    TRUE ~ link_url_for_cleaning
  )) %>% 
  mutate(link_url_cleaned = case_when(
    substr(link_url_cleaned, 1, 30) == "doi-org.ezp.lib.unimelb.edu.au" ~ sub("doi-org.ezp.lib.unimelb.edu.au", "doi.org", link_url_cleaned),
    substr(link_url_cleaned, 1, 11) == "www.osf_.io" ~ sub("www.osf_.io", "osf.io", link_url_cleaned),
    substr(link_url_cleaned, 1, 10) == "www.osf.io" ~ sub("www.", "", link_url_cleaned),
    substr(link_url_cleaned, 1, 21) == "www.iris-database.org" ~ sub("www.", "", link_url_cleaned),
    substr(link_url_cleaned, nchar(link_url_cleaned), nchar(link_url_cleaned)) %in% c("/", ".") ~ substr(link_url_cleaned, 1, nchar(link_url_cleaned) - 1),
    TRUE ~ link_url_cleaned
  )) %>% 
  mutate(link_url_cleaned = case_when(
    substr(link_url_cleaned, nchar(link_url_cleaned), nchar(link_url_cleaned)) %in% c("/") ~ substr(link_url_cleaned, 1, nchar(link_url_cleaned) - 1),
    TRUE ~ link_url_cleaned
  )) %>% 
  arrange(study_id, link_url_cleaned, coder)



################################################################################
# Section 5.1: Perform additional cleaning/recoding of some link_url values
# due to issues identified during the coding of batch0912.
#
# Article a000390: For consistency, all link_url_cleaned values will be
# formatted as "DOI 10.7910/DVN/SURSEO" instead of "10.7910/DVN/SURSEO" (steve)
# or "doi:10.7910/DVN/SURSEO" (evelyn and oliver).
#
# Article a000511: All link_url_cleaned values will be changed to the malformed
# value of "https:osf.io/mnz8j" as identified by steve.
#
# Article a000745: All link_url_cleaned values will be changed to the malformed
# value of "hppt://www.osf.io/uqg5m/#))" as identified by steve.
#
# NOTE: After iter03 of step 2 review, only the a000390 case is still relevant,
# since the other two cases were corrected in the step 2 iter03 review file.

coding_to_check4 <- coding_to_check3 %>% 
  mutate(link_url_cleaned = case_when(
    study_id == "a000390" & link_url_cleaned == "10.7910/DVN/SURSEO" ~ "DOI 10.7910/DVN/SURSEO",
    study_id == "a000390" & link_url_cleaned == "doi:10.7910/DVN/SURSEO" ~ "DOI 10.7910/DVN/SURSEO",
    study_id == "a000511" & link_url_cleaned == "osf.io/mnz8j" ~ "https:osf.io/mnz8j",
    study_id == "a000745" & link_url_cleaned == "osf.io/uqg5m" ~ "hppt://www.osf.io/uqg5m/#))",
    TRUE ~ link_url_cleaned
  ))



################################################################################
# Section 6: Determine the number of coders per cleaned link_url and final
# checking before export.

# Determine the number of coders per (cleaned) link_url.
coders_per_url <- coding_to_check4 %>% 
  group_by(study_id, link_url_cleaned) %>% 
  distinct(coder) %>% 
  count(name = "n_coders_link") %>% 
  ungroup()

# Perform a double-check that the only cleaned link_urls with only one coder
# were coded by steve.
check_single_coder_urls <- coders_per_url %>% 
  filter(n_coders_link == 1) %>% 
  inner_join(coding_to_check4, by = c("study_id", "link_url_cleaned"))

# For cases where the link_url_for_cleaning is the same before auto-cleaning,
# use the link_url_for_cleaning as the cleaned link_url value.
check_precleaned_links <- coding_to_check4 %>% 
  group_by(study_id, link_url_cleaned) %>% 
  distinct(link_url_for_cleaning) %>% 
  count() %>% 
  filter(n == 1) %>% 
  ungroup()



################################################################################
# Section x: Placeholder - restore the "original" link_url value to the cleaned
# link_url variable as much as possible.



################################################################################
# Section 7: Create a dataset containing the cleaned link_url value that can
# be used for matching.

# Create a dataset which contains all of the link cleaning information for all
# those articles deemed to actually have links.
all_clean_coding_link_url <- coding_to_check4 %>% 
  select(study_id, coder, link_number, link_url, corrected_link_url,
         link_url_for_cleaning, link_url_cleaned) %>% 
  arrange(study_id, link_url_cleaned, coder)

# Export the dataset to file.
write_csv(x = all_clean_coding_link_url,
          path = here("data", "output", "stage1",
                      "all_clean_coding_link_url.csv"))
write_rds(x = all_clean_coding_link_url,
          path = here("data", "output", "stage1",
                      "all_clean_coding_link_url.rds"))

# Merge the number of coders per cleaned link_url with the step 1 final coding
# to create a step 2 final coding dataset.

all_final_coding_step2 <- all_final_coding_step1 %>% 
  left_join(coders_per_url, by = "study_id") %>% 
  select(-n_coders_link) %>% 
  arrange(study_id, link_url_cleaned)

check_no_links <- all_final_coding_step2 %>% 
  filter(article_has_links == "no")

check_other_links <- all_final_coding_step2 %>% 
  filter(article_has_links == "other")

check_yes_links <- all_final_coding_step2 %>% 
  filter(article_has_links == "yes") %>% 
  filter(is.na(link_url_cleaned))

check_na_links <- all_final_coding_step2 %>% 
  filter(is.na(link_url_cleaned))

# Export the dataset to file.
write_csv(x = all_final_coding_step2,
          path = here("data", "output", "stage1",
                      "all_final_coding_step2.csv"))
write_rds(x = all_final_coding_step2,
          path = here("data", "output", "stage1",
                      "all_final_coding_step2.rds"))
