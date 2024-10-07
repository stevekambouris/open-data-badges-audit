################################################################################
# stage1_07_coding_cases_followup_step01.R
#
# This script looks at the responses among the coders for each article, and
# checks for cases where additional follow-up is required. This is step 1 of
# the review process, and checks for discrepancies in variables that are 
# relevant at the "article" level:
#
# - has_data_badge
# - data_availability_statement
# - article_has_links
#
# The cases with disagreement among coders, or a unanimous coding of no/other
# are exported for review and checking.
#
# The checking is performed outside of R, and the checked cases are re-imported
# for finalisation in the next R script.
#
# Note that this checking code may need to be run over multiple iterations,
# as coders' data is updated.

# Load required packages.
library(tidyverse)
library(here)
library(openxlsx)



################################################################################
# Section 1: Defining functions for checking

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
# Section 2: Performing the checks for the current set of coding

# Open the coders' cleaned data sets.
all_clean_coding <- readr::read_rds(here("data", "output", "stage1",
                                         "all_clean_coding.rds"))

#########################
# Column has_data_badge #
#########################

# Check for cases where there is disagreement and export to file.
hdb_disagreement <- check_disagreement(in_data = all_clean_coding,
                                       checkvar = "has_data_badge") %>% 
  mutate(use_code = NA_character_) %>% 
  mutate(review_note = NA_character_)

# Check for cases where the response is unanimously "no" or "other"
hdb_noother <- check_noother(in_data = all_clean_coding,
                             checkvar = "has_data_badge") %>% 
  mutate(review_coding = NA_character_) %>% 
  mutate(review_note = NA_character_)


######################################
# Column data_availability_statement #
######################################

# Check for cases where there is disagreement and export to file.
das_disagreement <- check_disagreement(in_data = all_clean_coding,
                                       checkvar = "data_availability_statement") %>% 
  mutate(use_code = NA_character_) %>% 
  mutate(review_note = NA_character_)

# Check for cases where the response is unanimously "no" or "other"
das_noother <- check_noother(in_data = all_clean_coding,
                             checkvar = "data_availability_statement") %>% 
  mutate(review_coding = NA_character_) %>% 
  mutate(review_note = NA_character_)


############################
# Column article_has_links #
############################

# Check for cases where there is disagreement
ahl_disagreement <- check_disagreement(in_data = all_clean_coding,
                                       checkvar = "article_has_links") %>% 
  mutate(use_code = NA_character_) %>% 
  mutate(review_note = NA_character_)

# Check for cases where the response is unanimously "no" or "other"
ahl_noother <- check_noother(in_data = all_clean_coding,
                             checkvar = "article_has_links") %>% 
  mutate(review_coding = NA_character_) %>% 
  mutate(review_note = NA_character_)



################################################################################
# Section 3: Incorporating the results of previous iterations' checking

# Function to check and incorporate previous checking.
incorporate_old_data <- function(new_data, old_data, ftype = "dis") {
  
  # Set the name of the "flag" column name - it is either "use_code" (for
  # cases of disagreement) or "review_coding" (for cases of unanimous no/other
  # responses). This choice will feed through to the renaming/merging code.
  if (ftype == "dis") {
    flag_colname <- "use_code"
  } else if (ftype == "noo") {
    flag_colname <- "review_coding"
  } else {
    flag_colname <- "use_code"
  }
  notes_colname <- "review_note"
  
  # Check how many records are unique to the old data only.
  data_oldonly <- old_data %>% 
    anti_join(new_data, by = c("study_id", "coder", "link_number"))
  n_oldonly <- data_oldonly %>% nrow()
  
  # Check how many records are unique to the new data only.
  data_newonly <- new_data %>% 
    anti_join(old_data, by = c("study_id", "coder", "link_number"))
  n_newonly <- data_newonly %>% nrow()
  
  # Check how many records can be found in common between the two data sets.
  n_common <- new_data %>% 
    semi_join(old_data, by = c("study_id", "coder", "link_number")) %>% 
    nrow()
  
  # If there are some common records, patch in the code flag and comment
  # values from the old data.
  # If there are no common records, just return the new data as-is.
  if (n_common > 0) {
    
    old_review <- old_data %>% 
      select(study_id, coder, link_number,
             .data[[flag_colname]],
             .data[[notes_colname]])
    
    patched_data <- new_data %>% 
      select(-.data[[flag_colname]], -.data[[notes_colname]]) %>% 
      left_join(old_review,
                by = c("study_id", "coder", "link_number"))

  } else {
    data_patched <- new_data
  }
  
  return(list(n_common = n_common,
              n_newonly = n_newonly,
              n_oldonly = n_oldonly,
              newonly = data_newonly,
              oldonly = data_oldonly,
              patched = patched_data))
}


#########################
# Column has_data_badge #
#########################

# Import the previous completed set of checking of these records.

# Disagreement
prev_hdb_dis <- read_csv(file = here("data", "raw", "stage1_review",
                                     "stage1_step1_has_data_badge_disagree - iter01.csv"))

# No/other
prev_hdb_noo <- read_csv(file = here("data", "raw", "stage1_review",
                                     "stage1_step1_has_data_badge_noother - iter01.csv"))

# Incorporate the previous coding into the new set of follow-up cases.
check_hdb_dis <- incorporate_old_data(hdb_disagreement,
                                      prev_hdb_dis,
                                      ftype = "dis")
check_hdb_noo <- incorporate_old_data(hdb_noother,
                                      prev_hdb_noo,
                                      ftype = "noo")


######################################
# Column data_availability_statement #
######################################

# Import the previous completed set of checking of these records.

# Disagreement
prev_das_dis <- read_csv(file = here("data", "raw", "stage1_review",
                                     "stage1_step1_data_availability_statement_disagree - iter01.csv"))

# No/other
prev_das_noo <- read_csv(file = here("data", "raw", "stage1_review",
                                     "stage1_step1_data_availability_statement_noother - iter01.csv"))

# Incorporate the previous coding into the new set of follow-up cases.
check_das_dis <- incorporate_old_data(das_disagreement,
                                      prev_das_dis,
                                      ftype = "dis")
check_das_noo <- incorporate_old_data(das_noother,
                                      prev_das_noo,
                                      ftype = "noo")


############################
# Column article_has_links #
############################

# Import the previous completed set of checking of these records.

# Disagreement
prev_ahl_dis <- read_csv(file = here("data", "raw", "stage1_review",
                                     "stage1_step1_article_has_links_disagree - iter01.csv"))

# No/other
prev_ahl_noo <- read_csv(file = here("data", "raw", "stage1_review",
                                     "stage1_step1_article_has_links_noother - iter01.csv"))

# Incorporate the previous coding into the new set of follow-up cases.
check_ahl_dis <- incorporate_old_data(ahl_disagreement,
                                      prev_ahl_dis,
                                      ftype = "dis")
check_ahl_noo <- incorporate_old_data(ahl_noother,
                                      prev_ahl_noo,
                                      ftype = "noo")



################################################################################
# Section 4: Export the updated set of records to follow-up to file.

# Important: set the step number of this follow-up step.
# Important: set the iteration number of this set of outputs.
stepnum <- "01"
iternum <- "04"

# Function to output data files.
output_step1_followup_to_file <- function(out_data,
                                          outpath = "data/output/stage1/follow_up",
                                          stepnum = "01",
                                          iternum,
                                          out_name) {
  
  readr::write_csv(x = out_data,
            path = here::here(outpath,
                        paste0("step", stepnum,
                               "_follow_up_", out_name,
                               "_iter", iternum,
                               ".csv")))

  readr::write_rds(x = out_data,
            path = here::here(outpath,
                        paste0("step", stepnum,
                               "_follow_up_", out_name,
                               "_iter", iternum,
                               ".rds")))
  
  return(invisible(NULL))
}

#########################
# Column has_data_badge #
#########################

# Disagreement
output_step1_followup_to_file(out_data = check_hdb_dis$patched,
                              iternum = iternum,
                              out_name = "has_data_badge_disagree")

# Unanimous no/other
output_step1_followup_to_file(out_data = check_hdb_noo$patched,
                              iternum = iternum,
                              out_name = "has_data_badge_noother")


######################################
# Column data_availability_statement #
######################################

# Disagreement
output_step1_followup_to_file(out_data = check_das_dis$patched,
                              iternum = iternum,
                              out_name = "data_availability_statement_disagree")

# Unanimous no/other
output_step1_followup_to_file(out_data = check_das_noo$patched,
                              iternum = iternum,
                              out_name = "data_availability_statement_noother")


############################
# Column article_has_links #
############################

# Disagreement
output_step1_followup_to_file(out_data = check_ahl_dis$patched,
                              iternum = iternum,
                              out_name = "article_has_links_disagree")

# Unanimous no/other
output_step1_followup_to_file(out_data = check_ahl_noo$patched,
                              iternum = iternum,
                              out_name = "article_has_links_noother")
