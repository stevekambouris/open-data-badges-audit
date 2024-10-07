################################################################################
# stage1_07_coding_cases_followup.R
#
# This script looks at the responses among the coders for each article, and
# checks for cases where additional follow-up is required. Examples include
# cases of articles without Open Data badges, cases where an account login is
# required, cases where the URL did not work, etc.
#
# The cases with disagreement for a codebook item are exported for review and
# checking.

# Load required packages.
library(tidyverse)
library(here)
library(openxlsx)

# Open the coders' cleaned data sets.
all_clean_coding <- readr::read_rds(here("data", "output", "stage1",
                                         "all_clean_coding.rds"))

# Function to check for disagreement within a study_id.
check_disagreement <- function(in_data, checkvar) {
  result <- in_data %>% 
    group_by(study_id) %>% 
    distinct(.data[[checkvar]]) %>% 
    count() %>% 
    filter(n > 1) %>% 
    select(study_id) %>% 
    left_join(in_data, by = "study_id")
  
  return(result)
}

# Function to check for values of "no" or "other" in cases where coders were
# unanimous.
check_noother <- function(in_data, checkvar) {
  result <- in_data %>% 
    group_by(study_id) %>% 
    distinct(.data[[checkvar]]) %>% 
    count() %>% 
    filter(n == 1) %>% 
    select(study_id) %>% 
    left_join(in_data, by = "study_id") %>% 
    filter(.data[[checkvar]] != "yes")
  
  return(result)
}


################################################################################
# Column has_data_badge

# Check for cases where there is disagreement and export to file.
hdb_disagreement <- check_disagreement(in_data = all_clean_coding,
                                       checkvar = "has_data_badge") %>% 
  mutate(use_code = NA_character_)

write_csv(x = hdb_disagreement,
          path = here("data", "output", "stage1", "follow_up",
                      "follow_up_disagree_has_data_badge.csv"))
write_rds(x = hdb_disagreement,
          path = here("data", "output", "stage1", "follow_up",
                      "follow_up_disagree_has_data_badge.rds"))

# Check for cases where the response is unanimously "no" or "other"
hdb_noother <- check_noother(in_data = all_clean_coding,
                             checkvar = "has_data_badge")

write_csv(x = hdb_noother,
          path = here("data", "output", "stage1", "follow_up",
                      "follow_up_noother_has_data_badge.csv"))
write_rds(x = hdb_noother,
          path = here("data", "output", "stage1", "follow_up",
                      "follow_up_noother_has_data_badge.rds"))




################################################################################
# Column data_availability_statement

# Check for cases where there is disagreement and export to file.
das_disagreement <- check_disagreement(in_data = all_clean_coding,
                                       checkvar = "data_availability_statement") %>% 
  mutate(use_code = NA_character_)

write_csv(x = das_disagreement,
          path = here("data", "output", "stage1", "follow_up",
                      "follow_up_disagree_data_availability_statement.csv"))
write_rds(x = das_disagreement,
          path = here("data", "output", "stage1", "follow_up",
                      "follow_up_disagree_data_availability_statement.rds"))

# Check for cases where the response is unanimously "no" or "other"
das_noother <- check_noother(in_data = all_clean_coding,
                             checkvar = "data_availability_statement")

write_csv(x = das_noother,
          path = here("data", "output", "stage1", "follow_up",
                      "follow_up_noother_data_availability_statement.csv"))
write_rds(x = das_noother,
          path = here("data", "output", "stage1", "follow_up",
                      "follow_up_noother_data_availability_statement.rds"))



################################################################################
# Column article_has_links
# Check for cases where there is disagreement
ahl_disagreement <- check_disagreement(in_data = all_clean_coding,
                                       checkvar = "article_has_links") %>% 
  mutate(use_code = NA_character_)

write_csv(x = ahl_disagreement,
          path = here("data", "output", "stage1", "follow_up",
                      "follow_up_disagree_article_has_links.csv"))
write_rds(x = ahl_disagreement,
          path = here("data", "output", "stage1", "follow_up",
                      "follow_up_disagree_article_has_links.rds"))

# Check for cases where the response is unanimously "no" or "other"
ahl_noother <- check_noother(in_data = all_clean_coding,
                             checkvar = "article_has_links")

write_csv(x = ahl_noother,
          path = here("data", "output", "stage1", "follow_up",
                      "follow_up_noother_article_has_links.csv"))
write_rds(x = ahl_noother,
          path = here("data", "output", "stage1", "follow_up",
                      "follow_up_noother_article_has_links.rds"))



################################################################################
# Column number_of_links

# Check for cases where there is disagreement
nol_disagreement <- check_disagreement(in_data = all_clean_coding,
                                       checkvar = "number_of_links")



# Check by link_url - look for non-matched urls within articles.
links_unmatched <- all_clean_coding %>% 
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
  )) %>% 
  group_by(study_id, link_url_cleaned) %>% 
  count()



# For every unique study_id/url combination, make a blank record for coder
# "final".
#
# 