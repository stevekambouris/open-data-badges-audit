# 04_create_master_selection_pool.R
# Create a dataset that contains the list of articles that are eligible to be
# sampled by the research assistants. This is the "master" dataset that will be
# queried each time a new batch of articles is to be made.
#
# Once this has been created, it is expected to be static. If the underlying
# OSB library of articles changes, only then will this dataset need to be
# changed.

# Load packages.
library(tidyverse)
library(here)
library(stringr)

# Read in the OSB library with study_id dataset from file.
osb_lib_with_study_id <- readr::read_rds(here::here("data", "output",
                                                    "osb_lib_with_study_id.rds"))

# Reduce the rows in the dataset to those that are valid (i.e. the N = 1,210
# open data badge articles published 2014-19).
osb_selection_pool <- osb_lib_with_study_id %>% 
  filter(OpenDataBadge == TRUE,
         `Publication Year` %in% c(2014, 2015, 2016, 2017, 2018, 2019),
         ErratumTag == FALSE)



# Investigate those cases where file attachments are NA.
check_no_files <- osb_selection_pool %>% 
  filter(is.na(`File Attachments`))

# Check for cases where an article has more than one file attached.
check_files <- osb_selection_pool %>% 
  pull(`File Attachments`)
check_semicolons <- str_count(check_files, ";")
check_pdf <- str_count(check_files, ".pdf")



# Determine the articles which have an identifiable PDF that can be copied.
#
# Create an "allowed in pool" flag.
# Remove from the pool:
# - articles from Geoscience Data Journal and Internet Archaeology,
# - articles which don't have a PDF
#
# Drop unneeded variables.
osb_selection_pool <- osb_selection_pool %>% 
  mutate(pdf_count = str_count(`File Attachments`, ".pdf")) %>% 
  separate(col = `File Attachments`,
           into = c("file_att1", "file_att2", "file_att3"),
           sep = ";") %>% 
  rowwise %>% 
  mutate(file_count = sum(!is.na(file_att1), !is.na(file_att2), !is.na(file_att3))) %>% 
  mutate(
    pdf_to_copy = case_when(
      study_id %in% c("a000382", "a001309") ~ paste0(file_att1, ";", file_att2),
      file_count == 1 & pdf_count == 1 ~ file_att1,
      file_count > 1 & pdf_count == 1 & str_count(file_att1, ".pdf") == 1 ~ file_att1,
      file_count > 1 & pdf_count == 1 & str_count(file_att2, ".pdf") == 1 ~ file_att2,
      file_count > 1 & pdf_count == 1 & str_count(file_att3, ".pdf") == 1 ~ file_att3,
      pdf_count > 1 & study_id == "a000244" ~ file_att1,
      pdf_count > 1 & study_id == "a001115" ~ file_att1,
      pdf_count > 1 & study_id == "a001229" ~ file_att2,
      pdf_count > 1 & study_id == "a001259" ~ file_att1,
      TRUE ~ ""
    )
  ) %>% 
  mutate(
    allowed_in_pool = case_when(
      `Publication Title` %in% c("Internet Archaeology", "Geoscience Data Journal") ~ FALSE,
      pdf_to_copy == "" ~ FALSE,
      TRUE ~ TRUE
    )
  ) %>% 
  select(-(Editor:`Legislative Body`)) %>% 
  select(-(`Series Number`:Place)) %>% 
  select(-c(ISBN, `Num Pages`, `Number Of Volumes`, Type, Archive,
            `Archive Location`, `Call Number`, `Link Attachments`)) %>% 
  select(-c(rowid, pdf_count, file_count)) %>% 
  select(-starts_with("manual_tags"))

# Removed: the clause of case_when which removed the training papers.
# study_id %in% c("a000465", "a000473", "a000062", "a001027", "a000880") ~ FALSE,

# Check the "not allowed" cases.
check_not_allowed <- osb_selection_pool %>% 
  filter(allowed_in_pool != TRUE)

# Export the selection pool dataset.
readr::write_rds(x = osb_selection_pool,
                 path = here::here("data", "output",
                                   "osb_selection_pool.rds"))

readr::write_csv(x = osb_selection_pool,
                 path = here::here("data", "output",
                                   "osb_selection_pool.csv"))
