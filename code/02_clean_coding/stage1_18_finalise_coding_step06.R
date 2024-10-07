# stage1_18_finalise_coding_step06.R -------------------------------------------
#
# This script finalises the values of download_success for all resolved_url
# records with valid file_exist values finalised in step 5.
#
# Note that this script may need to be re-run if coding is changed, updated,
# added to, etc.



# Load required packages -------------------------------------------------------
library(tidyverse)
library(here)



# Section 1: Import required datasets ------------------------------------------

# Import the cleaned coding data set.
all_clean_coding <- readr::read_rds(here("data", "output", "stage1",
                                         "all_clean_coding.rds"))

# Import the Step 5 finalised data set.
all_final_coding_step5 <- read_rds(here("data", "output", "stage1",
                                        "all_final_coding_step5.rds"))

# Import the resolved_url linking data set (used to link coders' link_url
# entries to cleaned values of link_url, and likewise for resolved_url).
all_clean_coding_resolved_url <- read_rds(here("data", "output", "stage1",
                                               "all_clean_coding_resolved_url.rds"))

# Import the results of the Step 6 coding follow-up.
# NOTE: The files names here will need to be updated each time another iteration
# of step 6 follow-up occurs.
step6_followup_results_disagree <- read_csv(file = here("data", "raw",
                                                        "stage1_review",
                                                        "stage1_step6_download_success_disagree - iter01.csv"))

step6_followup_results_nop <- read_csv(file = here("data", "raw",
                                                       "stage1_review",
                                                       "stage1_step6_download_success_nop - iter01.csv"))

# The final two columns in iter01 of the checking files are unnamed - they
# are informal notes made by me of (X10) the size of the download, and (X11)
# the source of the record being checked. They can be safely ignored.
step6_followup_results_checking <- read_csv(file = here("data", "raw",
                                                        "stage1_review",
                                                        "stage1_step6_download_success_checking - iter01.csv"))



# Section 2: Check and prepare the follow-up review results --------------------

# Cases where download_success is unanimously no/other/partial: double check
# that none of these need further review.
# For download_success, those that are review_coding = TRUE should all be able
# to be found in the Google sheet stage1_step6_download_success_checking.
check_nop_need_review <- step6_followup_results_nop %>% 
  filter(review_coding != FALSE)

# Cases of disagreement: check for missing use_code decisions. There should be
# no missing values.
check_disagree_missing <- step6_followup_results_disagree %>% 
  filter(is.na(use_code) == TRUE)

# Cases of disagreement: check for cases where all use_code values are FALSE
# for a url.
# For download_success, the only reason this should happen is if the record in
# question has been checked and put into the Google sheet
# stage1_step6_download_success_checking.
check_disagree_allF <- step6_followup_results_disagree %>% 
  group_by(study_id, link_url_cleaned) %>% 
  filter(all(use_code == FALSE))

# Cases of disagreement: check for cases where coder steve has a value of
# FALSE for use_code. In general, this should not happen - expected that I
# can code the correct/desired coding (or re-code before this point in the
# case of a genuine error).
check_disagree_steveF <- step6_followup_results_disagree %>% 
  filter(coder == "steve", use_code != TRUE)

# Cases of disagreement: are there cases where more than one record for each
# study_id/link combo is use_code TRUE?
check_disagree_manyT <- step6_followup_results_disagree %>% 
  group_by(study_id, resolved_url_cleaned, use_code) %>% 
  count() %>% 
  filter(use_code == TRUE, n > 1) %>% 
  ungroup() %>% 
  select(-use_code, -n) %>% 
  inner_join(step6_followup_results_disagree, by = c("study_id",
                                                     "resolved_url_cleaned"))

# Check: are the values of download_success consistent between records with
# use_code TRUE?
# There should be zero cases here - if multiple records within a study/link
# combo have use_code TRUE, then they need to have the same value of
# download_success.
# If they don't then there has been a coding review error.
check_disagree_consistent <- check_disagree_manyT %>% 
  group_by(study_id, resolved_url_cleaned, use_code) %>% 
  filter(use_code == TRUE) %>% 
  filter(first(files_exist) != files_exist)

# Clean up checking objects.
rm(list = c("check_noother_need_review", "check_disagree_missing",
            "check_disagree_allF", "check_disagree_steveF",
            "check_disagree_manyT", "check_disagree_consistent"))

# Create a list of the final download_success disagreement resolutions.
step6_disagree_resolutions <- step6_followup_results_disagree %>% 
  arrange(study_id, resolved_url_cleaned, coder) %>% 
  filter(use_code == TRUE) %>% 
  group_by(study_id, resolved_url_cleaned) %>% 
  slice_tail() %>% 
  ungroup() %>% 
  select(study_id, resolved_url_cleaned, download_success, download_success_other)

# Create a list of final download_success values from the "checking" dataset
step6_checking_resolutions <- step6_followup_results_checking |> 
  filter(use_code == TRUE) |> 
  select(study_id, resolved_url_cleaned, download_success, download_success_other)




# Section 3: Merge coders' coding with step 6 finalised data -------------------

# Get all coders' download_success and download_success_other codings.
all_clean_coding_download_success <- all_clean_coding %>% 
  select(study_id, coder, link_number, link_url, resolved_url,
         download_success, download_success_other)

# Quick check: can all cases in the resolved_url dataset still be found in the
# all clean coding dataset? The answer should definitely be yes - if not, then 
# something is deeply wrong, recheck Step 5 finalisation.
check_mismatches <- all_clean_coding_resolved_url %>% 
  anti_join(all_clean_coding_download_success, by = c("study_id", "coder",
                                                      "link_number", "link_url",
                                                      "resolved_url"))

# Restrict the coding records to those which were confirmed as actually having
# link_url values to check back at the finalisation of Step 2.
all_clean_coding_download_success2 <- all_clean_coding_resolved_url %>% 
  left_join(all_clean_coding_download_success, by = c("study_id", "coder",
                                                      "link_number", "link_url",
                                                      "resolved_url"))

# Get all coding cases where all coders agreed download_success = "yes".
coding_dl_success_allyes <- all_clean_coding_download_success2 %>% 
  group_by(study_id, resolved_url_cleaned) %>% 
  filter(all(download_success == "yes"))

# Get all coding cases where all coders agreed download_success = "no".
coding_dl_success_allno <- all_clean_coding_download_success2 %>% 
  group_by(study_id, resolved_url_cleaned) %>% 
  filter(all(download_success == "no"))

# Get all coding cases where all coders agreed download_success = "other".
coding_dl_success_allother <- all_clean_coding_download_success2 %>% 
  arrange(study_id, resolved_url_cleaned, coder) %>% 
  group_by(study_id, resolved_url_cleaned) %>% 
  filter(all(download_success == "other"))

# Get all coding cases where all coders agreed download_success = "partial".
coding_dl_success_allpartial <- all_clean_coding_download_success2 %>% 
  arrange(study_id, resolved_url_cleaned, coder) %>% 
  group_by(study_id, resolved_url_cleaned) %>% 
  filter(all(download_success == "partial"))

# Get all coding cases where coders disagreed on download_success.
coding_dl_success_disagree <- all_clean_coding_download_success2 %>% 
  group_by(study_id, resolved_url_cleaned) %>% 
  filter(
    (!all(download_success == "yes") &
            !all(download_success == "no") &
            !all(download_success == "other") &
            !all(download_success == "partial")) |
           (any(is.na(download_success)) == TRUE & all(is.na(download_success)) == FALSE)
         )

# Check: are there any mismatches between the "leftover" disagreement records
# and the imported Step 3 follow-up review results?
check_mismatches1 <- coding_dl_success_disagree %>% 
  anti_join(step6_followup_results_disagree,
            by = c("study_id", "coder", "link_url", "resolved_url"))
check_mismatches2 <- step6_followup_results_disagree %>% 
  anti_join(coding_dl_success_disagree,
            by = c("study_id", "coder", "link_url", "resolved_url"))

# Clean up.
rm(list = c("check_mismatches", "check_mismatches1", "check_mismatches2"))



# Section 4: Finalise values of download_success -------------------------------
# Create datasets containing the finalised values of download_success
# and download_success_other for each study_id/link_url_cleaned combination.

dl_success_final_yes <- coding_dl_success_allyes %>% 
  slice_head() %>% 
  ungroup() %>% 
  select(study_id, resolved_url_cleaned, download_success, download_success_other)

dl_success_final_no <- coding_dl_success_allno %>% 
  slice_head() %>% 
  ungroup() %>% 
  select(study_id, resolved_url_cleaned, download_success, download_success_other)

dl_success_final_other <- coding_dl_success_allother %>% 
  slice_tail() %>% 
  ungroup() %>% 
  select(study_id, resolved_url_cleaned, download_success, download_success_other)

dl_success_final_partial <- coding_dl_success_allpartial %>% 
  slice_tail() %>% 
  ungroup() %>% 
  select(study_id, resolved_url_cleaned, download_success, download_success_other)

# Combine the records into a single dataset.
dl_success_final_concat <- bind_rows(dl_success_final_yes,
                                    dl_success_final_no,
                                    dl_success_final_other,
                                    dl_success_final_partial,
                                    step6_disagree_resolutions) |> 
  anti_join(step6_checking_resolutions, by = c("study_id", "resolved_url_cleaned")) |> 
  bind_rows(step6_checking_resolutions)




# Check: are there any duplicates in the dataset?
check_duplicates <- dl_success_final_concat %>% 
  group_by(study_id, resolved_url_cleaned) %>% 
  count() %>% 
  filter(n > 1)

# Check: can all records be found in the Step 5 finalised data set?
check_mismatches1 <- dl_success_final_concat %>% 
  anti_join(all_final_coding_step5, by = c("study_id", "resolved_url_cleaned"))

# Check: are all records in Step 5 finalised data set that can't be found in
# the dl_success dataset those records without resolved_url values?
check_mismatches2 <- all_final_coding_step5 %>% 
  anti_join(dl_success_final_concat, by = c("study_id", "resolved_url_cleaned"))

# Clean up.
rm(list = c("check_duplicates", "check_mismatches1", "check_mismatches2"))



# Section 5: Create and export the finalised Step 6 dataset --------------------

# Special: apply the final ones, then apply the checking one?
all_final_coding_step6 <- all_final_coding_step5 %>% 
  left_join(dl_success_final_concat, by = c("study_id", "resolved_url_cleaned"))



# Check: do all records with files_exist = yes, other have a download_success
# value?
check_dl_success1 <- all_final_coding_step6 |> 
  filter(files_exist %in% c("yes", "other"),
         download_success != "yes",
         download_success != "no",
         download_success != "other",
         download_success != "partial")

# Check: what do records with files_exist = no have for download_success?
check_dl_success2a <- all_final_coding_step6 |> 
  filter(files_exist == "no")

check_dl_success2b <- all_final_coding_step6 |> 
  filter(is.na(download_success))

check_dl_success2c <- all_final_coding_step6 |> 
  filter(files_exist %in% c("yes", "other")) |> 
  filter(is.na(download_success) == TRUE) |> 
  mutate(code_out = str_c("study_id == \"", study_id,
                          "\" & resolved_url_cleaned == \"", resolved_url_cleaned,
                          "\" ~\"\",\r\n"))

# Check if 2c are in any of the follow-up files.
check_2cfu_checking <- step6_followup_results_checking |> 
  semi_join(check_dl_success2c, by = "study_id")
check_2cfu_disagree <- step6_followup_results_disagree |> 
  semi_join(check_dl_success2c, by = "study_id")
check_2cfu_nop <- step6_followup_results_nop |> 
  semi_join(check_dl_success2c, by = "study_id")

# Get the codings for the cases that couldn't be found in step 6 iter01  
check_coding2c_1a <- all_clean_coding_download_success2 |> 
  semi_join(check_dl_success2c, by = c("study_id", "resolved_url_cleaned"))

# Check: do all records with "other" have a comment?
check_dl_success_valid <- all_final_coding_step6 %>% 
  filter((! download_success %in% c("other", "partial") & is.na(download_success_other) == FALSE)
         | (download_success %in% c("other", "partial") & is.na(download_success_other) == TRUE))

# Perform final fixes to coding
all_final_coding_step6b <- all_final_coding_step6 |> 
  mutate(download_success = case_when(
    study_id == "a000001" & resolved_url_cleaned == "osf.io/d2jwb" ~ "yes",
    study_id == "a000037" & resolved_url_cleaned == "data.donders.ru.nl/collections/di/dccn/DSC_3018026.01_234" ~ "no",
    study_id == "a000074" & resolved_url_cleaned == "osf.io/t9c52" ~ "yes",
    study_id == "a000119" & resolved_url_cleaned == "osf.io/3ymbu" ~ "yes",
    study_id == "a000169" & resolved_url_cleaned == "osf.io/4cg2s" ~ "yes",
    study_id == "a000250" & resolved_url_cleaned == "dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/TZRNKD" ~ "yes",
    study_id == "a000259" & resolved_url_cleaned == "dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/SZ9YXZ" ~ "yes",
    study_id == "a000292" & resolved_url_cleaned == "www.iris-database.org/iris/app/home/detail?id=york:934328" ~ "other",
    study_id == "a000569" & resolved_url_cleaned == "osf.io/vh5jt" ~ "yes",
    study_id == "a000581" & resolved_url_cleaned == "reshare.ukdataservice.ac.uk/852882" ~ "partial",
    study_id == "a000649" & resolved_url_cleaned == "osf.io/3qskj" ~ "yes",
    study_id == "a000810" & resolved_url_cleaned == "osf.io/bqzpt" ~ "yes",
    study_id == "a001066" & resolved_url_cleaned == "reshare.ukdataservice.ac.uk/852127" ~ "partial",
    study_id == "a001083" & resolved_url_cleaned == "www.icpsr.umich.edu/web/NACDA/studies/4652/version/7" ~ "other",
    study_id == "a001274" & resolved_url_cleaned == "psy.fsu.edu/~catlab/alienssoa_pub/index.php" ~ "partial",
    study_id == "a001476" & resolved_url_cleaned == "www.worldvaluessurvey.org/wvs.jsp" ~ "partial",
    study_id == "a001525" & resolved_url_cleaned == "osf.io/yjer4" ~ "yes",
    study_id == "a001582" & resolved_url_cleaned == "osf.io/kcp9z" ~ "yes",
    study_id == "a001634" & resolved_url_cleaned == "hrsonline.isr.umich.edu" ~ "no",
    study_id == "a001634" & resolved_url_cleaned == "midus.colectica.org/Account/Login" ~ "no",
    TRUE ~ download_success
  ),
  download_success_other = case_when(
    study_id == "a000292" & resolved_url_cleaned == "www.iris-database.org/iris/app/home/detail?id=york:934328" ~ "Registration form required before data can be downloaded",
    study_id == "a000581" & resolved_url_cleaned == "reshare.ukdataservice.ac.uk/852882" ~ "Was able to download documentation and readme files but not actual data files",
    study_id == "a001066" & resolved_url_cleaned == "reshare.ukdataservice.ac.uk/852127" ~ "Was able to download some files which were labelled as open access",
    study_id == "a001083" & resolved_url_cleaned == "www.icpsr.umich.edu/web/NACDA/studies/4652/version/7" ~ "Could download all 'Document only' data, when I tried to download the other ones such as 'SAS', 'SPSS' etc, it takes me into a login screen",
    study_id == "a001274" & resolved_url_cleaned == "psy.fsu.edu/~catlab/alienssoa_pub/index.php" ~ "2 out of the 3 files were able to be downloaded, but I was not allowed access to one zip file",
    study_id == "a001476" & resolved_url_cleaned == "www.worldvaluessurvey.org/wvs.jsp" ~ "Many files across different pages, just downloaded codebook for Wave 7 to check",
    TRUE ~ download_success_other
  ))

# Check: do all records with files_exist = yes, other have a download_success
# value?
check_dl_success3 <- all_final_coding_step6b |> 
  filter(files_exist %in% c("yes", "other"),
         download_success != "yes",
         download_success != "no",
         download_success != "other",
         download_success != "partial")

# Check: what do records with files_exist = no have for download_success?
check_dl_success4a <- all_final_coding_step6b |> 
  filter(files_exist == "no")

check_dl_success4b <- all_final_coding_step6b |> 
  filter(is.na(download_success))

check_dl_success4c <- all_final_coding_step6b |> 
  filter(files_exist %in% c("yes", "other")) |> 
  filter(is.na(download_success) == TRUE) |> 
  mutate(code_out = str_c("study_id == \"", study_id,
                          "\" & resolved_url_cleaned == \"", resolved_url_cleaned,
                          "\" ~\"\",\r\n"))


# Show the number of each value for download_success to ensure it makes sense.
print(table(all_final_coding_step6b$download_success, useNA = "always"))



# Export to file ---------------------------------------------------------------
write_excel_csv(x = all_final_coding_step6b,
          file = here("data", "output", "stage1",
                      "all_final_coding_step6.csv"),
          eol = "\r\n",
          na = "",
          quote = "all")

write_rds(x = all_final_coding_step6b,
          file = here("data", "output", "stage1",
                      "all_final_coding_step6.rds"))
