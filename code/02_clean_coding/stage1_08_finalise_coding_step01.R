################################################################################
# stage1_08_finalise_coding_step01.R
#
# This script begins the construction of a "finalised" data set, which resolves
# differences between coders in article-level variables for each article, and
# also removes some articles due to changes in their badge status that the
# review has uncovered.
#
# Note that this script may need to be re-run  if the step 1 coding cases
# review is updated.

# Load required packages.
library(tidyverse)
library(here)

# Create a "master" list of all _coded_ articles by study_id. At stage 1, the
# article-level codes are finalised.

# Import cleaned coding data set.
all_clean_coding <- readr::read_rds(here("data", "output", "stage1",
                                         "all_clean_coding.rds"))

# Get the distinct study_id values
all_coded_study_ids <- all_clean_coding %>% 
  distinct(study_id)

# Remove the following articles from the data set:
#
# a000026, DOI:10.1111/ajps.12321 (American Journal of Political Science)
# This paper was entered into the database in April 2018 as having Open Data
# and Open Materials badges. The PDF of the article shows these badges, but
# the web page for the article on the journal website does not mention any
# badges. Additionally, there is no data availability statement below the
# abstract in the PDF, the usual location for it.
# The ambiguous status of the badges, plus the absence of data availability
# information, means that this article should be removed from the data set,
# but mentioned in the paper.
#
# a001130, DOI:10.1111/ajps.12273 (American Journal of Political Science)
# This paper was entered into the database in April 2018 as having Open Data
# and Open Materials badges. The PDF of the article shows these badges, but
# the web page for the article on the journal website does not mention any
# badges. Additionally, there is no data availability statement below the
# abstract in the PDF, the usual location for it.
# The ambiguous status of the badges, plus the absence of data availability
# information, means that this article should be removed from the data set,
# but mentioned in the paper.
#
# a001347, DOI:10.1080/00224545.2017.1389684 (The Journal of Social Psychology)
# This paper was corrected to remove its open data badge according to the
# Correction published 14 May 2019 (DOI:10.1080/00224545.2019.1599547).
#
# a001526, DOI:10.1111/ajps.12302 (American Journal of Political Science)
# This paper was entered into the database in April 2018 as having Open Data
# and Open Materials badges, but (i) the PDF does not have such badges and (ii)
# the website does not show any badges. Interestingly, the article does have
# a data availability statement, but there is no link. This article was either
# added to the database in error, or the journal website in 2018 erroneously
# indicated that the article did have badges.
#
# a000685, DOI:10.1111/ajps.12360 (American Journal of Political Science)
# This paper was awarded an open data badge when originally entered into
# database, but the data was only available upon request. According to the
# journal website and PDF, it has an Open Data badge, but according to the 
# Harvard Dataverse page for the article, it only has an Open Materials badge.
# This could be interpreted either way, but better to remove it and mention it
# in the paper.
#
# a001600, DOI:10.1111/ajps.12301 (American Journal of Political Science)
# This paper was entered into the database in April 2018 as having Open Data
# and Open Materials badges. The PDF of the article shows these badges, but
# the web page for the article on the journal website does not mention any
# badges. Additionally, there is no data availability statement below the
# abstract in the PDF, the usual location for it.
# The ambiguous status of the badges, plus the absence of data availability
# information, means that this article should be removed from the data set,
# but mentioned in the paper.
#
# a000063, DOI:10.1177/0956797615609900 (Psychological Science)
# This paper has been retracted since being entered into the database. As such,
# it should not be considered further in this study.
#
# a000839, DOI:10.1111/ajps.12320 (American Journal of Political Science)
# This paper was entered into the database in April 2018 as having Open Data
# and Open Materials badges. The PDF of the article shows these badges, but
# the web page for the article on the journal website does not mention any
# badges. Additionally, there is no data availability statement below the
# abstract in the PDF, the usual location for it.
# The ambiguous status of the badges, plus the absence of data availability
# information, means that this article should be removed from the data set,
# but mentioned in the paper.
final_coded_study_ids <- all_coded_study_ids %>% 
  filter(! study_id %in% c("a000026", "a001130", "a001347", "a001526",
                           "a000685", "a001600", "a000063", "a000839"))



process_rev_disagree <- function(invar, invar_other, infile, allcoding) {
  
  # Import the review of the has_data_badge disagreements.
  dis_rev_raw <- read_csv(file = infile)
  
  # Keep the information for those with use_code = TRUE.
  dis_rev_use_init <- dis_rev_raw %>% 
    arrange(study_id, coder) %>% 
    filter(use_code == TRUE) %>% 
    select(study_id, {{ invar }}, {{ invar_other }}, use_code,
           review_note)
  
  # If there are no records with use_code TRUE, then warn and return early.
  if (nrow(dis_rev_use_init) == 0) {
    warning(paste0("There were no records found with use_code value TRUE."))
    return(list(raw = dis_rev_raw,
                use_init = dis_rev_use_init,
                steve_coded = dis_rev_use_init,
                use_final = dis_rev_use_init,
                check = NULL))
  }
  
  # Check that within study_ids, the values marked as TRUE are consistent.
  dis_rev_checkn <- dis_rev_use_init %>% 
    distinct(study_id, {{ invar }}) %>% 
    group_by(study_id) %>% 
    summarise(n = n()) %>% 
    filter(n != 1)
  
  # If there are inconsistent values marked as use_code TRUE, then warn and
  # return early.
  if (nrow(dis_rev_checkn) != 0) {
    warning(paste0("There were ", nrow(dis_rev_checkn),
                   " study_id values where different values of ", invar,
                   " had use_code value set to TRUE."))
    return(list(raw = dis_rev_raw,
                use_init = dis_rev_use_init,
                steve_coded = NULL,
                use_final = NULL,
                check = dis_rev_checkn))
  }
  
  # At this point, remove "duplicate" records that have been marked use_code
  # TRUE. Only need one record marked TRUE per study_id.
  dis_rev_use_init2 <- dis_rev_use_init %>% 
    group_by(study_id) %>% 
    slice_tail() %>% 
    ungroup()
  message(paste0("Note: There were ",
                 nrow(dis_rev_use_init) - nrow(dis_rev_use_init2),
                 " duplicated use_code = TRUE ",
                 "records removed from the dataset."))
  
  # Check for any articles which have a use_code = TRUE value that have also
  # been coded by coder steve (in the main coding data set).
  checked_by_steve_all <- allcoding %>% 
    filter(coder == "steve") %>% 
    distinct(study_id)
  
  dis_rev_steve_coded <- dis_rev_use_init2 %>% 
    semi_join(checked_by_steve_all, by = "study_id")
  if (nrow(dis_rev_steve_coded) > 0) {
    warning(paste0("Of the articles found with use_code value TRUE, ",
                   nrow(dis_rev_steve_coded), " were coded by steve. ",
                   "These have been removed."))
  }
  
  dis_rev_use_final <- dis_rev_use_init2 %>% 
    anti_join(checked_by_steve_all, by = "study_id")
  
  if (nrow(dis_rev_use_final) == 0) {
    warning(paste0("There were no records found with use_code value TRUE ",
                   "after removing articles coded by steve."))
  }
  
  # Remove these articles from the list of codes to use.
  
  return(list(raw = dis_rev_raw,
              use_init = dis_rev_use_init,
              steve_coded = dis_rev_steve_coded,
              use_final = dis_rev_use_final,
              check = dis_rev_checkn))
}



process_rev_noother <- function(invar, invar_other, infile, allcoding) {
  
  # Import the review of the cases where has_data_badge is no or other.
  noo_rev_raw <- read_csv(file = infile)
  
  # Keep the invar information for those cases where review_coding is
  # unanimously FALSE.
  # Arbitrarily select the first record for each study_id. For "no", this
  # doesn't matter, but for "other", the first value of invar_other
  # will be chosen.
  noo_rev_use_init <- noo_rev_raw %>% 
    group_by(study_id) %>% 
    filter(all(review_coding == FALSE)) %>% 
    slice_head() %>% 
    ungroup() %>% 
    mutate( {{ invar_other }} := as.character(.data[[invar_other]])) %>% 
    select(study_id, {{ invar }}, {{ invar_other }}, review_coding,
           review_note)
  
  # Warn if no records can be used.
  if (nrow(noo_rev_use_init) == 0) {
    warning(paste0("There were no records found with review_coding value FALSE."))
  }
  
  # Check for any articles where review_coding is unanimously FALSE value that
  # have also been coded by coder steve (in the main coding data set).
  # Remove them from the dataset.
  checked_by_steve_all <- allcoding %>% 
    filter(coder == "steve") %>% 
    distinct(study_id)
  
  noo_rev_steve_coded <- noo_rev_use_init %>% 
    semi_join(checked_by_steve_all, by = "study_id")
  
  if (nrow(noo_rev_steve_coded) > 0) {
    warning(paste0("Of the records found with use_code value TRUE, ",
                   nrow(noo_rev_steve_coded), " were coded by steve. ",
                   "These have been removed."))
  }
    
  noo_rev_use_final <- noo_rev_use_init %>% 
    anti_join(checked_by_steve_all, by = "study_id")
  
  if (nrow(noo_rev_use_final) == 0) {
    warning(paste0("There were no records found with use_code value TRUE ",
                   "after removing articles coded by steve."))
  }
  
  return(list(raw = noo_rev_raw,
              use_init = noo_rev_use_init,
              steve_coded = noo_rev_steve_coded,
              use_final = noo_rev_use_final))
}



process_orig_yes <- function(all_coding, invar, invar_other) {
  
  # Get the study_ids of all studies where all coders unanimously coded
  # has_data_badge as "yes".
  yes_ori_use <- all_coding %>% 
    group_by(study_id) %>% 
    filter(all(.data[[invar]] == "yes")) %>% 
    slice_head() %>% 
    ungroup() %>% 
    select(study_id, {{ invar }}, {{ invar_other }})
  
  if (nrow(yes_ori_use) == 0) {
    warning(paste0("There were no records where coders unanimously coded \"yes\"."))
  }
  
  return(yes_ori_use)
}



combine_and_check1 <- function(indisagree, innoother, inyes, invar,
                               invar_other, all_ids, all_coding) {
  
  # Combine the sources of final values for the variable.
  final1 <- bind_rows(indisagree, innoother, inyes) %>% 
    select(study_id, {{ invar }}, {{ invar_other }})
    
  
  # Check: are there duplicate study_ids?
  check_dupl1 <- final1 %>% 
    filter(duplicated(.[["study_id"]]))
  
  if (nrow(check_dupl1) != 0) {
    warning(paste0("After combining disagreement, noother, and unanimous yes",
                   ", there were ", nrow(check_dupl1), " cases of duplicate",
                   " study_ids."))
  }
  
  # Check: are there "left over" study_ids without a final value?
  leftover1 <- anti_join(x = all_ids,
                            y = final1,
                            by = "study_id")
  
  message(paste0("After combining disagreement, noother, and unanimous yes",
                 ", there were ", nrow(leftover1), " leftover",
                 " study_ids."))
  
  # If there are, see if coder "steve" has coded the article. If so, use this
  # value.
  steve_use <- all_coding %>% 
    inner_join(leftover1, by = "study_id") %>% 
    filter(coder == "steve") %>% 
    group_by(study_id) %>% 
    slice_head() %>% 
    ungroup() %>% 
    select(study_id, {{ invar }}, {{ invar_other }})
  
  message(paste0("Of the ", nrow(leftover1), " leftover study_ids, ",
                 nrow(steve_use), " study_ids were coded by coder steve."))
  
  return(list(combined = final1,
              duplicates = check_dupl1,
              leftover = leftover1,
              steve = steve_use))
}



combine_and_check2 <- function(infinal, insteve, all_ids) {
  
  # Combine the sources of final values for the variable
  final2 <- bind_rows(infinal, insteve)
  
  # Check: are there duplicate study_ids?
  check_dupl2 <- final2 %>% 
    filter(duplicated(.[["study_id"]]))
  
  if (nrow(check_dupl2) != 0) {
    warning(paste0("After combining disagreement, noother, unanimous yes, ",
                   "and the articles coded by coder steve, there were ",
                   nrow(check_dupl2), " cases of duplicate study_ids."))
  } else {
    message("There were no duplicate study_ids detected after incorporating ",
            "the articles coded by steve.")
  }
  
  # Check: are there "left over" study_ids without a final value?
  leftover2 <- anti_join(x = all_ids,
                         y = final2,
                         by = "study_id")
  
  if (nrow(leftover2) != 0) {
    warning(paste0("After combining with the articles coded by steve, there",
                   " were still ", nrow(leftover2), "study_ids left over."))
  } else {
    message(paste0("There were ", nrow(leftover2), " study_ids left over",
                   " after incorporating the articles coded by steve."))
  }
  
  # Check: are there "extra" study_ids that aren't in the final list?
  # (These should be among the six studies that were removed due to badge
  #  issues.)
  extra2 <- anti_join(x = final2,
                      y = all_ids,
                      by = "study_id")
  
  if (nrow(extra2) != 0) {
    message(paste0("There were ", nrow(extra2), " study_ids in the combined ",
                   "finalised coding data that were not in the final set of ",
                   "study_ids. Check that all these \"extra\" study_ids are ",
                   "known removals due to badge problems."))
  }
  
  return(list(final = final2,
              duplicates = check_dupl2,
              leftover = leftover2,
              extra = extra2))
}



################################################################################
# has_data_badge

hdb_step1 <- process_rev_disagree(invar = "has_data_badge",
                                  invar_other = "has_data_badge_other",
                                  infile = here("data", "raw", "stage1_review",
                                                "stage1_step1_has_data_badge_disagree - iter04.csv"),
                                  allcoding = all_clean_coding)

hdb_step2 <- process_rev_noother(invar = "has_data_badge",
                                 invar_other = "has_data_badge_other",
                                 infile = here("data", "raw", "stage1_review",
                                               "stage1_step1_has_data_badge_noother - iter04.csv"),
                                 allcoding = all_clean_coding)

hdb_step3 <- process_orig_yes(all_coding = all_clean_coding,
                              invar = "has_data_badge",
                              invar_other = "has_data_badge_other")

hdb_step4 <- combine_and_check1(indisagree = hdb_step1$use_final,
                                innoother = hdb_step2$use_final,
                                inyes = hdb_step3,
                                invar = "has_data_badge",
                                invar_other = "has_data_badge_other",
                                all_ids = final_coded_study_ids,
                                all_coding = all_clean_coding)

hdb_step5 <- combine_and_check2(infinal = hdb_step4$combined,
                                insteve = hdb_step4$steve,
                                all_ids = final_coded_study_ids)



################################################################################
# data_availability_statement

das_step1 <- process_rev_disagree(invar = "data_availability_statement",
                                  invar_other = "data_availability_statement_other",
                                  infile = here("data", "raw", "stage1_review",
                                                "stage1_step1_data_availability_statement_disagree - iter04.csv"),
                                  allcoding = all_clean_coding)

das_step2 <- process_rev_noother(invar = "data_availability_statement",
                                 invar_other = "data_availability_statement_other",
                                 infile = here("data", "raw", "stage1_review",
                                               "stage1_step1_data_availability_statement_noother - iter04.csv"),
                                 allcoding = all_clean_coding)

das_step3 <- process_orig_yes(all_coding = all_clean_coding,
                              invar = "data_availability_statement",
                              invar_other = "data_availability_statement_other")

das_step4 <- combine_and_check1(indisagree = das_step1$use_final,
                                innoother = das_step2$use_final,
                                inyes = das_step3,
                                invar = "data_availability_statement",
                                invar_other = "data_availability_statement_other",
                                all_ids = final_coded_study_ids,
                                all_coding = all_clean_coding)

das_step5 <- combine_and_check2(infinal = das_step4$combined,
                                insteve = das_step4$steve,
                                all_ids = final_coded_study_ids)



################################################################################
# article_has_links

ahl_step1 <- process_rev_disagree(invar = "article_has_links",
                                  invar_other = "article_has_links_other",
                                  infile = here("data", "raw", "stage1_review",
                                                "stage1_step1_article_has_links_disagree - iter04.csv"),
                                  allcoding = all_clean_coding)

ahl_step2 <- process_rev_noother(invar = "article_has_links",
                                 invar_other = "article_has_links_other",
                                 infile = here("data", "raw", "stage1_review",
                                               "stage1_step1_article_has_links_noother - iter04.csv"),
                                 allcoding = all_clean_coding)

ahl_step3 <- process_orig_yes(all_coding = all_clean_coding,
                              invar = "article_has_links",
                              invar_other = "article_has_links_other")

ahl_step4 <- combine_and_check1(indisagree = ahl_step1$use_final,
                                innoother = ahl_step2$use_final,
                                inyes = ahl_step3,
                                invar = "article_has_links",
                                invar_other = "article_has_links_other",
                                all_ids = final_coded_study_ids,
                                all_coding = all_clean_coding)

ahl_step5 <- combine_and_check2(infinal = ahl_step4$combined,
                                insteve = ahl_step4$steve,
                                all_ids = final_coded_study_ids)



################################################################################
# Merge the three variables.

all_final_coding_step1 <- final_coded_study_ids %>% 
  left_join(hdb_step5$final, by = "study_id") %>% 
  left_join(das_step5$final, by = "study_id") %>% 
  left_join(ahl_step5$final, by = "study_id") %>% 
  arrange(study_id)

# Check the duplicates.
check_dups <- all_final_coding_step1 %>% 
  group_by(study_id) %>% 
  count() %>% 
  filter(n > 1)
  


# Check that there are no study_ids with missing values.
print(table(all_final_coding_step1$has_data_badge, useNA = "always"))
print(table(all_final_coding_step1$data_availability_statement, useNA = "always"))
print(table(all_final_coding_step1$article_has_links, useNA = "always"))


# Export the no/other values to file, for further checking.

article_details <- read_rds(here("data", "output",
                                 "osb_lib_with_study_id.rds")) %>% 
  select(study_id, Author, `Publication Year`, Title, `Publication Title`,
         DOI)

tocheck_hdb <- all_final_coding_step1 %>% 
  filter(has_data_badge != "yes") %>% 
  left_join(article_details, by = "study_id")

tocheck_das <- all_final_coding_step1 %>% 
  filter(data_availability_statement != "yes") %>% 
  left_join(article_details, by = "study_id")

tocheck_ahl <- all_final_coding_step1 %>% 
  filter(article_has_links != "yes") %>% 
  left_join(article_details, by = "study_id")

write_csv(x = tocheck_hdb,
          path = here("data", "output", "stage1", "final_step01",
                      "check_final_noother_has_data_badge.csv"))
write_rds(x = tocheck_hdb,
          path = here("data", "output", "stage1", "final_step01",
                      "check_final_noother_has_data_badge.rds"))

write_csv(x = tocheck_das,
          path = here("data", "output", "stage1", "final_step01",
                      "check_final_noother_data_availability_statement.csv"))
write_rds(x = tocheck_das,
          path = here("data", "output", "stage1", "final_step01",
                      "check_final_noother_data_availability_statement.rds"))

write_csv(x = tocheck_ahl,
          path = here("data", "output", "stage1", "final_step01",
                      "check_final_noother_article_has_links.csv"))
write_rds(x = tocheck_ahl,
          path = here("data", "output", "stage1", "final_step01",
                      "check_final_noother_article_has_links.rds"))



################################################################################
# Output the finalised step 1 data

write_csv(x = all_final_coding_step1,
          path = here("data", "output", "stage1",
                      "all_final_coding_step1.csv"))
write_rds(x = all_final_coding_step1,
          path = here("data", "output", "stage1",
                      "all_final_coding_step1.rds"))