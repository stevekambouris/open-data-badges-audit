# stage1_20_clean_coding_data.R ------------------------------------------------
#
# This script takes the post-step 6 finalised coding, the variables about file
# repositories used, adds some manual records for articles removed from the
# coding earlier but now being put back in, and creates a final clean data set.
# It also adds in the final notes from the coders for each article, and the
# bibliographic details of the articles.



# Load required packages -------------------------------------------------------
library(tidyverse)
library(here)



# Import the step 6 finalised coding -------------------------------------------
data_coding_step6 <- read_rds(here("data", "output", "stage1",
                                   "all_final_coding_step6.rds"))



# Import the repository data ---------------------------------------------------
data_coding_repo <- read_rds(here("data", "output", "stage1",
                                  "all_final_repository_coding.rds"))



# Import the latest version of the OSB library ---------------------------------
osb_lib_raw <- read_rds(here("data", "output",
                             "osb_lib_with_study_id_latest.rds"))

# Get the bibliographic details of the articles in the coding set
osb_lib_relevant <- osb_lib_raw |> 
  semi_join(data_coding_step6, by = "study_id")

# Determine which articles have been "left out" of the coding set
osb_selection_pool <- read_rds(here("data", "output", "osb_selection_pool.rds"))

articles_removed <- osb_selection_pool |> 
  ungroup() |> 
  anti_join(osb_lib_relevant, by = "study_id") |> 
  relocate(study_id, .before = `Title`)



# Create coding records for the eight "left out" articles ----------------------
data_coding_leftout <- tibble(study_id = c("a000026",
                                           "a000063",
                                           "a000685",
                                           "a000839",
                                           "a001130",
                                           "a001347",
                                           "a001526",
                                           "a001600"),
                              has_data_badge = c("yes",
                                                 "other",
                                                 "yes",
                                                 "yes",
                                                 "yes",
                                                 "other",
                                                 "yes",
                                                 "yes"),
                              has_data_badge_other = c(NA_character_,
                                                       "Article has been retracted",
                                                       NA_character_,
                                                       NA_character_,
                                                       NA_character_,
                                                       "Article was assigned an Open Data badge in error, correction made in Erratum (https://doi.org/10.1080/00224545.2019.1599547)",
                                                       NA_character_,
                                                       NA_character_),
                              data_availability_statement = c("no",
                                                              NA_character_,
                                                              "yes",
                                                              "no",
                                                              "no",
                                                              NA_character_,
                                                              "yes",
                                                              "no"),
                              data_availability_statement_other = c(NA_character_,
                                                                    NA_character_,
                                                                    NA_character_,
                                                                    NA_character_,
                                                                    NA_character_,
                                                                    NA_character_,
                                                                    NA_character_,
                                                                    NA_character_),
                              article_has_links = c("no",
                                                    NA_character_,
                                                    "other",
                                                    "no",
                                                    "no",
                                                    NA_character_,
                                                    "no",
                                                    "no"),
                              article_has_links_other = c(NA_character_,
                                                          NA_character_,
                                                          "The statement clearly says data is legally restricted to those who meet criteria to view the confidential data and is (potentially) available upon request from the authors. There is a link provided for materials and code only.",
                                                          NA_character_,
                                                          NA_character_,
                                                          NA_character_,
                                                          NA_character_,
                                                          NA_character_),
                              link_url_cleaned = NA_character_,
                              link_work = NA_character_,
                              link_work_noother = NA_character_,
                              resolved_url_cleaned = NA_character_,
                              files_exist = NA_character_,
                              files_exist_other = NA_character_,
                              download_success = NA_character_,
                              download_success_other = NA_character_)



# Get all the notes from the original coders -----------------------------------
coders_notes_raw <- read_rds(here("data", "output", "stage1",
                                  "all_clean_coding.rds")) |> 
  select(study_id, coder, notes)

check_notes1 <- coders_notes_raw |> 
  group_by(notes) |> 
  count() |> 
  ungroup() |> 
  arrange(desc(n))

coders_notes_fmt <- coders_notes_raw |> 
  arrange(study_id, coder, notes) |> 
  mutate(final_notes = replace_na(notes, "(no note made)")) |> 
  mutate(final_notes = str_c(coder, ": ", str_squish(final_notes))) |> 
  group_by(study_id) |> 
  mutate(all_notes = paste(final_notes, collapse = "; ")) |> 
  ungroup(study_id) |> 
  distinct(study_id, all_notes)



# Bibliographic details of articles to attach to the coding --------------------
osb_lib_formerge <- osb_lib_raw |> 
  semi_join(osb_selection_pool, by = "study_id") |> 
  select(study_id, `Publication Year`:`Publication Title`,
         ISSN, DOI, Pages, Issue, Volume,
         OpenDataBadge:PreregisteredPlus)




# Combine the coding data sets, clean ------------------------------------------
data_coding_clean <- bind_rows(data_coding_step6, data_coding_leftout) |> 
  left_join(data_coding_repo, by = c("study_id",
                                     "link_url_cleaned",
                                     "resolved_url_cleaned")) |> 
  left_join(coders_notes_fmt, by = "study_id") |> 
  left_join(osb_lib_formerge, by = "study_id")


check_norepo <- data_coding_clean |> 
  filter(is.na(is_repository) == TRUE | is_repository == "no")

check_nobib <- data_coding_clean |> 
  filter(is.na(`Publication Year`) | is.na(`Publication Title`) | is.na(Author))


# Apply final coding to has_data_badge -----------------------------------------
# (Apply no cleaning here; cases of "other" will be dropped for summary)
data_coding_final1 <- data_coding_clean



# Apply final coding to data_availability_statement ----------------------------
# Resolve cases of "other" to either "yes" or "no"

# Check some special cases
check_das1 <- data_coding_clean |> 
  filter(data_availability_statement == "other")
check_das2 <- data_coding_clean |> 
  filter(data_availability_statement == "no")
check_das3 <- data_coding_clean |> 
  filter(`Publication Title` == "Social Psychology")

# Final coding decisions:
# - All Social Psychology articles have no D.A.S. - all used the acknowledgment
#   section for data links.
# - Special case a001067 had four sub-sections called "Open Science" in body of
#   article, this should count.
# - All remaining "other" cases should be coded to "no" - upon a final review,
#   they shouldn't count as "proper" data availability statements.
# - All other cases of "no" should remain as "no".
data_coding_final2 <- data_coding_final1 |> 
  mutate(final_data_avail_statement = case_when(
    
    # All Social Psychology articles have no D.A.S. - all used the
    # Acknowledgments section for data links
    `Publication Title` == "Social Psychology" ~ "no",
    
    # Special case a001067 had four sub-sections called "Open Science" in body
    # of article, this should count
    study_id == "a001067" ~ "yes",
    
    # All remaining cases of "other" should be counted as "no"
    data_availability_statement == "other" ~ "no",
    
    TRUE ~ data_availability_statement
  ))

# Look at the before and after final recode
table(data_coding_final2$data_availability_statement, useNA = "always")
table(data_coding_final2$final_data_avail_statement, useNA = "always")



# Apply final coding to article_has_links --------------------------------------
# Resolve cases of "other" to either "yes" or "no"

# Check cases
check_ahl1 <- data_coding_clean |> 
  filter(article_has_links == "no")
check_ahl2 <- data_coding_clean |> 
  filter(article_has_links == "other")

# Final coding decisions:
# - Article a000685 is the case of the data being available upon request only,
#   but which has links for materials and code. Should be "no".
# - All remaining cases of "other" should be "yes" - they all results in links
#   to data some way or other.
data_coding_final3 <- data_coding_final2 |> 
  mutate(final_art_has_links = case_when(
    
    # Article a000685 is the case of the data being available upon request only,
    # but which has links for materials and code. Should be "no".
    study_id == "a000685" ~ "no",
    
    # All remaining cases of "other" should be "yes" - they all have nominal
    # links to data.
    article_has_links == "other" ~ "yes",
    
    TRUE ~ article_has_links
  ))

# Look at the before and after final recode
table(data_coding_final3$article_has_links, useNA = "always")
table(data_coding_final3$final_art_has_links, useNA = "always")



# Apply final coding to link_work ----------------------------------------------
# Resolve cases of "other" to either "yes" or "no"

# Check cases
check_lw1 <- data_coding_clean |> 
  filter(link_work == "no")
check_lw2 <- data_coding_clean |> 
  filter(link_work == "other")
check_lw3 <- data_coding_clean |> 
  filter(study_id == "a001476")

# Final coding decisions:
# - Keep the four cases of "no" link work as "no" - in two cases, the DOI as
#   reported simply doesn't exist, in one case, the DOI gets caught in a 302
#   redirect loop, and in the final case, the URL can't resolve at all (a DNS
#   server lookup failure?).
# - Make one "other" case "no", since it is a 404 that gets redirected to the
#   main site (a001476, url forscenter.ch/en/our-surveys/swiss-household-panel)
# - Make some "other" cases "no", due to DOIs not existing (and so leading to
#   an error page on the DOI website or the ezid website).
# - Recode all remaining cases of "other" to "yes", since they all technically
#   work, or require a trivial correction to work.
data_coding_final4 <- data_coding_final3 |> 
  mutate(final_link_works = case_when(
    
    # Redirects to a 404 error page on the main domain
    study_id == "a001476" & link_url_cleaned == "forscenter.ch/en/our-surveys/swiss-household-panel" ~ "no",
    study_id == "a001250" & link_url_cleaned == "dbk.gesis.org/DBKSearch/SDesc2.asp?no=6701" ~ "no",
    study_id == "a001429" & link_url_cleaned == "www.tessexperiments.org/data/ferrer825.html" ~ "no",
    
    # DOI not found (redirects to main doi.org domain)
    study_id == "a000367" & link_url_cleaned == "doi.org/10.7910/DVN/YM4LDG" ~ "no",
    study_id == "a000777" & link_url_cleaned == "dx.doi.org/10.7910/DVN/LWED0F" ~ "no",
    study_id == "a001221" & link_url_cleaned == "doi.org/10.7910/DVN/BAG0DS" ~ "no",
    study_id == "a001501" & link_url_cleaned == "dx.doi.org/10.7910/DVN/DEMP8Q" ~ "no",
    
    # DOI not found (redirects to ezid.cdlib.org domain)
    study_id == "a000849" & link_url_cleaned == "dx.doi.org/10.7910/DVN/NQOWDE" ~ "no",
    
    # All remaining cases of "other" to be recoded to "yes".
    link_work == "other" ~ "yes",
    
    TRUE ~ link_work
  ))

# Look at the before and after final recode
table(data_coding_final4$link_work, useNA = "always")
table(data_coding_final4$final_link_works, useNA = "always")



# Apply final coding to files_exist --------------------------------------------
# Resolve cases of "other" to either "yes" or "no"

# Check cases
check_fe1 <- data_coding_clean |> 
  filter(files_exist == "no")
check_fe2 <- data_coding_clean |> 
  filter(files_exist == "other")
check_fe3 <- data_coding_clean |> 
  filter(study_id == "a000793")
check_fe4 <- data_coding_clean |> 
  filter(link_work == "other") |> 
  relocate(study_id, link_work, link_work_noother, link_url_cleaned,
           resolved_url_cleaned, files_exist, files_exist_other)

# Rules for dictating a files-exist failure:
# - If the link resolves to a general project website
# - If the link resolves to the main page of a repository, without a unique ID
#   to locate the data with (e.g., DOI, accession number)
# - If the link resolves to instructions for accessing data, rather than files
#   themselves
# - If the link resolves to a destination which is clearly not a location with
#   available files
# - If the link resolves to a login page with no indication of the contents of
#   the destination webpage

# Final coding decisions:
# - For those cases where final_link_works is now "no", force the final files-
#   exist code to also be "no" by default (even though in some cases, the
#   appropriate files could be found by searching).
# - Keep all original "no" cases as "no"
# - For those cases where the link "works" but doesn't lead anywhere useful,
#   re-code these as "no" for files exist
# - For cases of files-exist-other where the link resolves to a login screen,
#   code to "no"
data_coding_final5 <- data_coding_final4 |> 
  mutate(final_files_exist = case_when(
    
    # All cases of non-working links are set to N/A by default
    final_link_works == "no" ~ NA_character_,
    
    # Sent to main/general repository site, no ID/Accession number
    study_id == "a000011" & link_url_cleaned == "www.ukbiobank.ac.uk" ~ "no",
    study_id == "a000031" & link_url_cleaned == "five.dartmouth.edu/datasets" ~ "no",
    study_id == "a000160" & link_url_cleaned == "childes.talkbank.org" ~ "no",
    study_id == "a000280" & link_url_cleaned == "www.icpsr.umich.edu/icpsrweb/ICPSR/series/203" ~ "no",
    study_id == "a000295" & link_url_cleaned == "iris-database.org" ~ "no",
    study_id == "a000501" & link_url_cleaned == "iris-database.org/iris/app/home/search?query=Jung+in+press" ~ "no",
    study_id == "a000518" & link_url_cleaned == "iris-database.org" ~ "no",
    study_id == "a000688" & link_url_cleaned == "iris-database.org" ~ "no",
    study_id == "a000785" & link_url_cleaned == "iris-database.org" ~ "no",
    study_id == "a000845" & link_url_cleaned == "iris-database.org" ~ "no",
    study_id == "a001121" & link_url_cleaned == "iris-database.org" ~ "no",
    study_id == "a001215" & link_url_cleaned == "iris-database.org" ~ "no",
    study_id == "a001247" & link_url_cleaned == "iris-database.org" ~ "no",
    study_id == "a001285" & link_url_cleaned == "iris-database.org" ~ "no",
    study_id == "a001296" & link_url_cleaned == "iris-database.org" ~ "no",
    study_id == "a001490" & link_url_cleaned == "osf.io" ~ "no",
    study_id == "a001634" & link_url_cleaned == "hrsonline.isr.umich.edu" ~ "no",
    
    # Sent to a non-data page about requesting access
    study_id == "a000132" & link_url_cleaned == "www.pairfam.de/en/data/data-access" ~ "no",
    study_id == "a000260" & link_url_cleaned == "fcon_1000.projects.nitrc.org/indi/enhanced/access.html" ~ "no",
    
    # Some other error
    study_id == "a000330" & link_url_cleaned == "doi.org/10.7910/DVN/WD3IUA" ~ "no", # different study?
    study_id == "a000334" & link_url_cleaned == "dx.doi.org/10.7910" ~ "no", # incomplete DOI which happens to resolve
    study_id == "a000168" & link_url_cleaned == "doi.org/10.3886/ICPSR21600.v17" ~ "no", # data superseded, no longer available
    study_id == "a000948" & link_url_cleaned == "osf.io/677jr" ~ "no", # no data file, only PDF, BUT FIXED IN 2022
    study_id == "a001027" & link_url_cleaned == "doi.org/10.3886/ICPSR04652.v6" ~ "no", # data superseded, no longer available
    study_id == "a001430" & files_exist == "other" ~ "no", # Files listed for upload, but not accessible?
    
    # Sent to opaque login screens
    study_id == "a000581" & link_url_cleaned == "osf.io/n8v3c" ~ "no",
    study_id == "a000935" & link_url_cleaned == "osf.io/pg9ca" ~ "no",
    study_id %in% c("a001027", "a001186", "a001634") & resolved_url_cleaned == "midus.colectica.org/Account/Login" ~ "no",
    study_id == "a000655" & link_url_cleaned == "db.humanconnectome.org" ~ "no",
    study_id == "a000965" & link_url_cleaned == "osf.io/b6n8t" ~ "no",
    study_id == "a000965" & link_url_cleaned == "osf.io/fs67k" ~ "no",
    
    # All remaining cases of "other" recoded to "yes"
    files_exist == "other" ~ "yes",
    
    TRUE ~ files_exist
  ))
  
# Look at the before and after final recode
table(data_coding_final5$files_exist, useNA = "always")
table(data_coding_final5$final_files_exist, useNA = "always")

# Perform additional checks
check_fe5 <- data_coding_final5 |> 
  filter(is.na(files_exist) & !is.na(final_files_exist))




# Apply final coding to download_success ---------------------------------------
# Resolve cases of "other" to either "yes" or "no"

# Check cases
check_dl1 <- data_coding_clean |> 
  filter(download_success == "no")
check_dl2 <- data_coding_clean |> 
  filter(download_success == "other")
check_dl3 <- data_coding_clean |> 
  filter(download_success == "partial")

check_dl1b <- data_coding_final5 |> 
  filter(final_files_exist == "yes", download_success == "no")
check_dl2b <- data_coding_final5 |> 
  filter(final_files_exist == "yes", download_success == "other")
check_dl3b <- data_coding_final5 |> 
  filter(final_files_exist == "yes", download_success == "partial")


# dl succ other: a000935, osf.io/pg9ca - OSF login screen with no ability to see the contents beforehand. Should be files_exist no
# dl succ other: a001250 (moot since link didn't work, but requires a request to access)
# 


# Final coding decisions:
# - Cases of final_files_exist = "no" should be N/A
# - Cases of final_files_exist = N/A should be N/A
# - a001239, osf.io/dux3n: could be fully downloaded, change from "other" to
#   "yes"
# - a000904, dataverse.nl/dvn/dv/chemosignaling: the three locked zip files are
#   possibly duplicated within the repo (same file sizes, with `_c` added to)
#   file names, change from "other" to "yes".
# - a001135, some locked files (raw experiment files) with no explanation? Set
#   to "no".
# - In general, the dl_success "other" cases can be considered "yes" since all
#   that is needed is a registration or to provide details.
# - In general, the dl_success "partial" cases can be considered "yes".

data_coding_final6 <- data_coding_final5 |> 
  
  # Create a flag indicating all links where an account is required
  mutate(flag_credentials_required = case_when(
    
    # Deal with the cases where download_success is "no"
    files_exist %in% c("yes", "other") & 
      download_success == "no" & 
      study_id != "a000948" ~ "yes",
    
    # Deal with the cases where download_success is "other"
    ! (study_id %in% c("a000031", "a000292", "a000904", "a001239",
                       "a001289")) & 
      download_success == "other" ~ "yes",
    
    # Deal with the cases where download_success is "partial"
    study_id %in% c("a000581", "a000601", "a001066") & 
      download_success == "partial" ~ "yes",
    
    # The remainder do not need an account to download
    TRUE ~ "no"
  )) |> 
  
  # Make final decisions on how to recode the download success variable
  mutate(final_download_success = case_when(
  
    # For cases where the final decision has been that there were no files
    # available, or no link to data, the download success variable should be NA.
    is.na(final_files_exist) ~ NA_character_,
    final_files_exist == "no" ~ NA_character_,
    
    # Files on IRIS, but can be downloaded, don't need account
    study_id == "a000292" & download_success == "other" ~ "yes",
    
    # Files on ScholarBank@NUS (National University of Singapore), require
    # a guestbook completion but no account required
    study_id == "a000031" & download_success == "other" ~ "yes",
    
    # Files on a university departmental web site (which no longer exists);
    # one out of the three files listed was unable to be downloaded. Unclear
    # on whether a user was "not allowed" or "unable" to due to error.
    study_id == "a001274" & download_success == "partial" ~ "no",
    
    # Restricted files are media articles which are copyright restricted, but
    # which are obtainable via their originally published locations
    study_id == "a001289" & download_success == "other" ~ "yes",
    
    # Re-checked 2024-09-05; was able to download data fully
    study_id == "a001239" & download_success == "other" ~ "yes",
    
    # There are 3 restricted files don't seem to matter - there are identical
    # files named slightly differently ("Part 1.zip" vs "Part1_c.zip", etc.)
    # which are available, and look to be the same size as the restricted
    # versions.
    study_id == "a000904" & download_success == "other" ~ "yes",
    
    # There are 43 restricted files included without explanation or recourse - 
    # they cannot even be requested. They are all E-Prime or Tobii database
    # files, related to eye-tracking.
    study_id == "a001135" & download_success == "other" ~ "no",
    
    # This is the study with available RADAR data, but LiDAR data is only
    # available on request (there is a readme on the file server, nothing
    # to do with needing an account, you need to email someone).
    study_id == "a001577" & download_success == "other" ~ "no",
    
    # Cases of "no" but which require credentials should be coded as "account"
    final_files_exist == "yes" &
      download_success == "no" &
      flag_credentials_required == "yes" ~ "account",
    
    # Cases of "other" but which require credentials should be coded as
    # "account"
    final_files_exist == "yes" &
      download_success == "other" &
      flag_credentials_required == "yes" ~ "account",
    
    # Cases of "partial" but which require credentials should be coded as
    # "account"
    final_files_exist == "yes" &
      download_success == "partial" &
      flag_credentials_required == "yes" ~ "account",
    
    TRUE ~ download_success
  )) |> 
  mutate(final_download_success2 = case_when(
    final_download_success %in% c("account", "partial") ~ "yes",
    TRUE ~ final_download_success
  ))

# Look at the before and after final recode
table(data_coding_final6$download_success, useNA = "always")
table(data_coding_final6$final_download_success, useNA = "always")
table(data_coding_final6$flag_credentials_required, useNA = "always")
table(data_coding_final6$final_download_success2, useNA = "always")

# Do some additional checking to ensure recoding has been performed correctly
# temp_check1 <- data_coding_final6 |> 
#   filter(final_download_success %in% c("no", "other"))
# temp_check2 <- data_coding_final6 |> 
#   filter(final_download_success %in% c("partial"))
# temp_check3 <- data_coding_final6 |> 
#   filter(final_download_success %in% c("account"))
# temp_check4 <- data_coding_final6 |> 
#   filter(final_download_success %in% c("yes"), flag_credentials_required != "no")
# temp_check5 <- data_coding_final6 |> 
#   filter(final_download_success %in% c("yes"), is.na(flag_credentials_required) == TRUE)
# temp_check6 <- data_coding_final6 |> 
#   filter(final_download_success == "yes", (download_success != "yes" | is.na(download_success) == TRUE))



# Export to file ---------------------------------------------------------------
write_rds(data_coding_final6,
          here("data", "output", "stage1", "all_final_coding_final.rds"))

write_excel_csv(data_coding_final6,
                here("data", "output", "stage1", "all_final_coding_final.csv"),
                eol = "\r\n",
                quote = "all",
                na = "")
