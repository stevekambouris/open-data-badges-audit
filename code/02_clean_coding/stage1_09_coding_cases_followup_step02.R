################################################################################
# stage1_09_coding_cases_followup_step02.R
#
# This script uses the results of step 1 of the reviewing/finalising process to
# compare coders' responses at the link_url level within each article. Any
# discrepancies found between the links identified by coders are output for
# follow-up.

# Load required packages.
library(tidyverse)
library(here)

# Import the coders' cleaned data set.
all_clean_coding <- readr::read_rds(here("data", "output", "stage1",
                                         "all_clean_coding.rds"))

# Import the finalised step 1 data for the articles.
all_final_coding_step1 <- read_rds(here("data", "output", "stage1",
                                        "all_final_coding_step1.rds"))

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

# Create the set of articles to compare urls. Create a cleaned link_url
# variable which strips out as much unnecessary noise in the url text as
# possible to maximise the number of matches across coders within articles.
coding_has_links <- all_clean_coding %>% 
  anti_join(single_coder_articles, by = "study_id") %>% 
  filter(article_has_links != "no") %>% 
  inner_join(study_ids_to_match, by = "study_id") %>% 
  mutate(link_url_cleaned = case_when(
    substr(link_url, 1, 8) == "https://" ~ sub("https://", "", link_url),
    substr(link_url, 1, 7) == "http://" ~ sub("http://", "", link_url),
    substr(link_url, 1, 7) == "https//" ~ sub("https//", "", link_url),
    TRUE ~ link_url
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
  ))

# Determine the number of coders per article, for the subset of articles where
# coders thought that the article had links (this might reveal that some
# articles were only coded with links once).
coders_per_article <- coding_has_links %>% 
  group_by(study_id) %>% 
  distinct(coder) %>% 
  count(name = "n_coders") %>% 
  ungroup()
print(table(coders_per_article$n_coders))

# Determine the number of coders per unique url within each article.
urls_by_article <- coding_has_links %>% 
  distinct(study_id, link_url_cleaned)

coders_per_url <- coding_has_links %>% 
  group_by(study_id, link_url_cleaned) %>% 
  distinct(coder) %>% 
  count(name = "n_coders_link") %>% 
  ungroup()

# Check for any missed articles (articles that weren't pre-flagged as single
# coder articles).
missed_articles <- study_ids_to_match %>% 
  anti_join(coders_per_url, by = "study_id") %>% 
  anti_join(single_coder_articles, by = "study_id")

# Determine the mismatches, i.e. when the number of coders per url does not
# match the number of coders who looked at the article.
link_coding_matches <- coders_per_article %>% 
  inner_join(coders_per_url, by = "study_id") %>% 
  filter(n_coders == n_coders_link)

link_coding_mismatches <- coders_per_article %>% 
  inner_join(coders_per_url, by = "study_id") %>% 
  filter(n_coders != n_coders_link)

# Create a data set of the original coding for all article ids found, but
# exclude the codings with article_has_links = "no".
link_url_followup <- link_coding_mismatches %>% 
  distinct(study_id) %>% 
  inner_join(all_clean_coding, by = "study_id") %>% 
  filter(article_has_links != "no") %>% 
  mutate(use_code = NA_character_) %>% 
  mutate(review_notes = NA_character_) %>% 
  mutate(corrected_link_url = NA_character_) %>% 
  arrange(study_id, link_url)

# Export the data to file for follow-up.
write_csv(x = link_url_followup,
          path = here("data", "output", "stage1", "follow_up",
                      "step02_link_url_followup.csv"))
write_rds(x = link_url_followup,
          path = here("data", "output", "stage1", "follow_up",
                      "step02_link_url_followup.rds"))



################################################################################
# Iteration 2: Use the results from iteration 1 of the link_url follow-up to
# update the values of the follow-up file. This makes running the code in
# stage1_10_finalise_coding_step02.R easier, if all the messy updating is
# handled here in the "follow-up" script.

# Import the completed link_url cleaning data.
link_url_followup_results_iter01 <- read_csv(file = here("data", "raw",
                                                         "stage1_review",
                                                         "iter01",
                                                         "stage1_step2_link_url_followup_iter01 - link_url_followup.csv"))

# Fix the encoding issue that affects one record from the imported follow-up
# file.
link_url_followup_results_iter01 <- link_url_followup_results_iter01 %>% 
  mutate(link_url = ifelse(
    study_id == "a000591" & coder == "nguyenlam" & link_url == "https://osf.io/xytch\n",
    "https://osf.io/xytch\r",
    link_url
  ))

# Check which iteration 1 records can be matched easily with the latest
# follow-up dataset.
check_matched <- link_url_followup_results_iter01 %>% 
  semi_join(link_url_followup, by = c("study_id", "coder", "link_number",
                                      "link_url"))

# Check which iteration 1 records are missing from the the latest follow-up
# dataset.
# There should only be the records for the retracted article a000063.
check_missing <- link_url_followup_results_iter01 %>% 
  anti_join(link_url_followup, by = c("study_id", "coder", "link_number",
                                      "link_url"))

# Check which records in the latest follow-up dataset are new/extra to iteration
# 1 (and thus make up iteration 2 of checking).
check_extra <- link_url_followup %>% 
  anti_join(link_url_followup_results_iter01, by = c("study_id", "coder",
                                                     "link_number", "link_url"))

# Merge the results from the completed follow-up results with the latest
# follow-up template.
link_url_followup_iter02 <- link_url_followup_results_iter01 %>% 
  select(study_id, coder, link_number, link_url, use_code, review_notes,
         corrected_link_url) %>% 
  right_join(link_url_followup, by = c("study_id", "coder", "link_number",
                                       "link_url")) %>% 
  select(-use_code.y, -review_notes.y, -corrected_link_url.y) %>% 
  rename(use_code = use_code.x, review_notes = review_notes.x,
         corrected_link_url = corrected_link_url.x) %>% 
  relocate(link_number, link_url, .after = number_of_links) %>% 
  relocate(use_code, review_notes, corrected_link_url, .after = notes) %>% 
  arrange(study_id, link_url, coder)

# Export iteration 2 of the dataset to file for follow-up.
write_csv(x = link_url_followup_iter02,
          path = here("data", "output", "stage1", "follow_up",
                      "step02_link_url_followup_iter02.csv"))
write_rds(x = link_url_followup_iter02,
          path = here("data", "output", "stage1", "follow_up",
                      "step02_link_url_followup_iter02.rds"))



################################################################################
# Iteration 3: Use previously confirmed results from all_clean_coding_link_url
# to update the values of the follow-up file. This makes running the code in
# stage1_10_finalise_coding_step02.R easier, if all the messy updating is
# handled here in the "follow-up" script.

# Import the completed link_url cleaning data.
link_url_followup_results_iter02 <- read_csv(file = here("data", "raw",
                                                         "stage1_review",
                                                         "stage1_step2_link_url_followup - iter02.csv"))

# Fix the encoding issue that affects one record from the imported follow-up
# file.
link_url_followup_results_iter02 <- link_url_followup_results_iter02 %>% 
  mutate(link_url = ifelse(
    study_id == "a000591" & coder == "nguyenlam" & link_url == "https://osf.io/xytch\n",
    "https://osf.io/xytch\r",
    link_url
  ))

# Check which iteration 2 records can be matched easily with the latest
# follow-up dataset.
check_matched <- link_url_followup_results_iter02 %>% 
  semi_join(link_url_followup, by = c("study_id", "coder", "link_number",
                                      "link_url"))

# Check which iteration 2 records are missing from the the latest follow-up
# dataset.
# There should only be the records for the retracted article a000063.
check_missing <- link_url_followup_results_iter02 %>% 
  anti_join(link_url_followup, by = c("study_id", "coder", "link_number",
                                      "link_url"))

# Check which records in the latest follow-up dataset are new/extra to iteration
# 1 (and thus make up iteration 2 of checking).
check_extra <- link_url_followup %>% 
  anti_join(link_url_followup_results_iter02, by = c("study_id", "coder",
                                                     "link_number", "link_url"))

# Merge the results from the completed follow-up results with the latest
# follow-up template.
link_url_followup_iter03 <- link_url_followup_results_iter02 %>% 
  select(study_id, coder, link_number, link_url, use_code, review_notes,
         corrected_link_url) %>% 
  right_join(link_url_followup, by = c("study_id", "coder", "link_number",
                                       "link_url")) %>% 
  select(-use_code.y, -review_notes.y, -corrected_link_url.y) %>% 
  rename(use_code = use_code.x, review_notes = review_notes.x,
         corrected_link_url = corrected_link_url.x) %>% 
  relocate(link_number, link_url, .after = number_of_links) %>% 
  relocate(use_code, review_notes, corrected_link_url, .after = notes) %>% 
  arrange(study_id, link_url, coder)

# Export iteration 2 of the dataset to file for follow-up.
write_csv(x = link_url_followup_iter03,
          path = here("data", "output", "stage1", "follow_up",
                      "step02_link_url_followup_iter03.csv"))
write_rds(x = link_url_followup_iter03,
          path = here("data", "output", "stage1", "follow_up",
                      "step02_link_url_followup_iter03.rds"))