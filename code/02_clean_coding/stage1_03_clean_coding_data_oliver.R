################################################################################
# stage1_03_clean_coding_data_oliver.R
#
# This script cleans the raw coding data entered by coder oliver.
#
# The script assumes the raw data exists in the current environment with name
# data_entry_<coder name>_raw (imported by stage1_02_import_coding_data.R).
#
# The code takes the raw data, and makes use of a temporary data frame object
# to perform the cleaning required.

# Load required packages.
library(tidyverse)

# Remove the two demo records from the data set.
data_entry_temp <- data_entry_oliver_raw %>% 
  filter(!(article_id %in% c("demo01", "demo02")))



# Clean responses for articles a000211 and a000750 - apparently PDFs were not
# accessible?
rows_to_update <- which(data_entry_temp$article_id %in% c("a000211",
                                                          "a000750"))
data_entry_temp[rows_to_update, "has_data_badge_other"] <- "(see notes field)"
data_entry_temp[rows_to_update, "data_availability_statement"] <- "other"
data_entry_temp[rows_to_update, "data_availability_statement_other"] <- "(not completed by coder)"
data_entry_temp[rows_to_update, "article_has_links"] <- "no"
rm(list = c("rows_to_update"))



# Cleaning data_availability_statement
# Set response of "no" to "other", since data_availability_statement_other was
# filled out and comment makes clear that "other" is appropriate.
data_entry_temp <- data_entry_temp %>% 
  mutate(data_availability_statement = ifelse(
    article_id == "a001485"
    & data_availability_statement == "no"
    & is.na(data_availability_statement_other) == FALSE,
    "other",
    data_availability_statement
  ))



# Cleaning article_has_links

# Add value of "no" to four articles for which field is empty - it is implied
# from the other completed fields that no links were found.
data_entry_temp <- data_entry_temp %>% 
  mutate(article_has_links = case_when(
    article_id %in% c("a000799", "a000140", "a000502", "a000672")
    & is.na(article_has_links) == TRUE ~ "no",
    TRUE ~ article_has_links
  ))

# For article a000720 where link_work = "no", append comment in link_work_other
# to the notes field and clear all redundant fields.
data_entry_temp <- data_entry_temp %>% 
  mutate(notes = ifelse(
    article_id == "a000720"
    & article_has_links == "no"
    & is.na(article_has_links_other) == FALSE,
    paste0(notes, " - ", article_has_links_other),
    notes
  )) %>% 
  mutate(link_work = ifelse(
    article_id == "a000720"
    & article_has_links == "no"
    & is.na(article_has_links_other) == FALSE
    & link_work == "other",
    NA_character_,
    link_work
  )) %>% 
  mutate(article_has_links_other = ifelse(
    article_id == "a000720"
    & article_has_links == "no"
    & is.na(article_has_links_other) == FALSE,
    NA_character_,
    article_has_links_other
  ))

# For three articles with article_has_links = "other", change to "no" -
# comments make clear that no specific links to data are included. Append
# comments in article_has_links_other to notes field, and clear field
# article_has_links_other.
data_entry_temp <- data_entry_temp %>% 
  mutate(notes = case_when(
    article_id %in% c("a000518", "a000532")
    & article_has_links == "other" ~ paste0(notes, ". ",
                                            article_has_links_other),
    article_id == "a001186"
    & article_has_links == "other" ~ paste0(article_has_links_other, ". ",
                                            notes)
  )) %>% 
  mutate(article_has_links_other = ifelse(
    article_id %in% c("a000518", "a000532", "a001186")
    & article_has_links == "other"
    & is.na(article_has_links_other) == FALSE,
    NA_character_,
    article_has_links_other
  )) %>% 
  mutate(article_has_links = ifelse(
    article_id %in% c("a000518", "a000532", "a001186")
    & article_has_links == "other"
    & is.na(article_has_links_other) == TRUE,
    "no",
    article_has_links
  )) %>% 
  mutate(number_of_links = ifelse(
    article_id == "a001186"
    & article_has_links == "no"
    & number_of_links == 4,
    NA_integer_,
    number_of_links
  ))




# Cleaning number_of_links, is_repository, is_repository_other
# Remove redundant coding from variables for a record determined to have no
# links.
rows_to_update <- which(data_entry_temp$article_id %in% c("a001526"))
data_entry_temp[rows_to_update, "number_of_links"] <- NA_integer_
data_entry_temp[rows_to_update, "is_repository"] <- NA_character_
data_entry_temp[rows_to_update, "is_repository_other"] <- NA_character_
rm(list = c("rows_to_update"))



# Cleaning link_work
# Change value of link_work from "no" to "other", as comment in
# link_work_noother makes clear that "other" is more appropriate than "no" for
# article a000920.
data_entry_temp <- data_entry_temp %>% 
  mutate(link_work = ifelse(
    article_id == "a000920"
    & link_work == "no"
    & is.na(link_work_noother) == FALSE,
    "other",
    link_work
  ))

# Cleaning resolved_url - download_success_other for article intro10.
rows_to_update <- which(data_entry_temp$article_id %in% c("intro10"))
data_entry_temp[rows_to_update, "resolved_url"] <- NA_character_
data_entry_temp[rows_to_update, "is_repository"] <- NA_character_
data_entry_temp[rows_to_update, "is_repository_other"] <- NA_character_
data_entry_temp[rows_to_update, "repository_name"] <- NA_character_
data_entry_temp[rows_to_update, "files_exist"] <- NA_character_
data_entry_temp[rows_to_update, "download_success"] <- NA_character_
data_entry_temp[rows_to_update, "download_success_other"] <- NA_character_
rm(list = c("rows_to_update"))

# Add a value of "yes" to link_work to records for which the other coding
# indicates that the links clearly did work.
# Change link_work value of "other" to "yes" for a record which indicates that
# the link did work.
data_entry_temp <- data_entry_temp %>% 
  mutate(link_work = ifelse(
    article_id %in% c("a000063", "a000288", "a000769", "a000931", "a000390",
                      "a000580")
    & is.na(link_work) == TRUE,
    "yes",
    link_work
  )) %>% 
  mutate(link_work = ifelse(
    article_id %in% c("a000260")
    & link_work == "other",
    "yes",
    link_work
  ))



# Cleaning resolved_url for a000355 - redundant coding
data_entry_temp <- data_entry_temp %>% 
  mutate(resolved_url = ifelse(
    article_id == "a000355"
    & link_work == "no"
    & is.na(resolved_url) == FALSE,
    NA_character_,
    resolved_url
  ))



# Cleaning link_work_noother and resolved_url for two articles.
rows_to_update <- which(data_entry_temp$article_id %in% c("a001147"))
data_entry_temp[rows_to_update,
                "link_work_noother"] <- data_entry_temp[rows_to_update,
                                                        "resolved_url"]
data_entry_temp[rows_to_update, "resolved_url"] <- NA_character_
data_entry_temp[rows_to_update, "is_repository"] <- NA_character_
data_entry_temp[rows_to_update, "repository_name"] <- NA_character_
rm(list = c("rows_to_update"))

rows_to_update <- which(data_entry_temp$article_id %in% c("a000777"))
data_entry_temp[rows_to_update,
                "link_work_noother"] <- data_entry_temp[rows_to_update,
                                                        "resolved_url"]
data_entry_temp[rows_to_update, "resolved_url"] <- NA_character_
rm(list = c("rows_to_update"))



# Cleaning resolved_url for records that omitted the resolved url.
data_entry_temp <- data_entry_temp %>% 
  mutate(resolved_url = ifelse(
    article_id %in% c("a000581", "a000998", "a001394", "a000814")
    & is.na(link_url) == FALSE
    & is.na(resolved_url) == TRUE,
    "(resolved url not entered by coder)",
    resolved_url
  ))



# Cleaning files_exist
# Add values of "other" for three cases, add comments from link_work_noother,
# also add download_success value of "no".
# Add value of "no" for one case (comment in link_work_noother makes clear
# that "no" is appropriate).
data_entry_temp <- data_entry_temp %>% 
  mutate(files_exist = case_when(
    article_id %in% c("a000299", "a000581", "a000948") & link_work == "other"
    & is.na(files_exist) == TRUE ~ "other",
    article_id == "a001125" & link_work == "other"
    & is.na(files_exist) == TRUE ~ "no",
    TRUE ~ files_exist
  )) %>% 
  mutate(files_exist_other = ifelse(
    article_id %in% c("a000299", "a000581", "a000948")
    & files_exist == "other" & is.na(files_exist_other) == TRUE,
    link_work_noother,
    files_exist_other
  )) %>% 
  mutate(download_success = ifelse(
    article_id %in% c("a000299", "a000581", "a000948")
    & files_exist == "other" & is.na(download_success) == TRUE,
    "no",
    download_success
  ))



# Cleaning download_success
# Add missing values of "no" for three articles with files_exist status
# "other".
# Remove redundant comment from one record.
data_entry_temp <- data_entry_temp %>% 
  mutate(download_success = ifelse(
    article_id %in% c("a000831", "a000260", "a000334")
    & files_exist == "other" & is.na(download_success) == TRUE,
    "no",
    download_success
  )) %>% 
  mutate(download_success_other = ifelse(
    article_id == "a000629" & download_success == "yes"
    & is.na(download_success_other) == FALSE,
    NA_character_,
    download_success_other
  ))



# Cleaning is_repository and is_repository_other
data_entry_temp <- data_entry_temp %>% 
  mutate(is_repository = ifelse(
    article_id %in% c("a000299", "a001125")
    & link_work == "other"
    & is.na(is_repository) == TRUE,
    "other",
    is_repository
  )) %>% 
  mutate(is_repository_other = case_when(
    article_id %in% c("a000299", "a001125") & is_repository == "other"
    & is.na(is_repository_other) == TRUE ~ resolved_url,
    article_id %in% c("a000260", "a000533") & is_repository == "no"
    & is.na(is_repository_other) == FALSE ~ NA_character_,
    article_id == "a001611" & is_repository == "other"
    & is.na(is_repository_other) == TRUE ~ "(no comment from orginal coder)",
    TRUE ~ is_repository_other
  ))



# Cleaning notes
# Add note about one record having two URLs in the resolved_url field.
data_entry_temp <- data_entry_temp %>% 
  mutate(notes = ifelse(
    article_id == "a001611" & is.na(notes) == TRUE,
    "(resolved_url contains two distinct URLs, not clear where second URL comes from)",
    notes
  ))



# Cleaning number_of_links
# For article intro08, change number_of_links from 2 to 1, because only 1 link
# was included in coding. There is a note about the second link in notes field.
data_entry_temp <- data_entry_temp %>% 
  mutate(number_of_links = ifelse(
    article_id == "intro08" & number_of_links == 2 & link_number == 1,
    1,
    number_of_links
  ))



# Break up multi-link records into separate rows.
multilink_records <- data_entry_temp %>%
  filter(number_of_links > 1) %>%
  uncount(number_of_links, .remove = FALSE, .id = "temp_id") %>% 
  mutate(link_number = ifelse(
    number_of_links > 1,
    temp_id,
    number_of_links
  )) %>% 
  select(!(temp_id)) %>% 
  mutate(link_url = case_when(
    number_of_links == 2 & link_number == 1
    & link_url == "osf.io with /ucn6q/ and /wb2yv/" ~ "osf.io/ucn6q/",
    number_of_links == 2 & link_number == 2
    & link_url == "osf.io with /ucn6q/ and /wb2yv/" ~ "osf.io/wb2yv/",
    
    number_of_links == 2 & link_number == 1
    & link_url == "osf.io/6myfj/ and osf.io/tk67x/" ~ "osf.io/6myfj/",
    number_of_links == 2 & link_number == 2
    & link_url == "osf.io/6myfj/ and osf.io/tk67x/" ~ "osf.io/tk67x/",
    
    number_of_links == 3 & link_number == 1
    & link_url == "osf.io/dteh2 and osf.io/dp2u7 and osf.io/spqbw" ~ "osf.io/dteh2",
    number_of_links == 3 & link_number == 2
    & link_url == "osf.io/dteh2 and osf.io/dp2u7 and osf.io/spqbw" ~ "osf.io/dp2u7",
    number_of_links == 3 & link_number == 3
    & link_url == "osf.io/dteh2 and osf.io/dp2u7 and osf.io/spqbw" ~ "osf.io/spqbw",
    
    number_of_links == 3 & link_number == 1
    & link_url == "osf.io/tvyxz/files and five.dartmouth.edu/datasetsand and doi-org.ezp.lib.unimelb.edu.au/10.25540/3wd8-dffw" ~ "osf.io/tvyxz/files",
    number_of_links == 3 & link_number == 2
    & link_url == "osf.io/tvyxz/files and five.dartmouth.edu/datasetsand and doi-org.ezp.lib.unimelb.edu.au/10.25540/3wd8-dffw" ~ "five.dartmouth.edu/datasets",
    number_of_links == 3 & link_number == 3
    & link_url == "osf.io/tvyxz/files and five.dartmouth.edu/datasetsand and doi-org.ezp.lib.unimelb.edu.au/10.25540/3wd8-dffw" ~ "doi-org.ezp.lib.unimelb.edu.au/10.25540/3wd8-dffw",
    
    number_of_links == 2 & link_number == 1
    & link_url == "osf.io/d2ncs for exp1 and osf.io/473gq for exp2" ~ "osf.io/d2ncs",
    number_of_links == 2 & link_number == 2
    & link_url == "osf.io/d2ncs for exp1 and osf.io/473gq for exp2" ~ "osf.io/473gq",
    
    number_of_links == 2 & link_number == 1
    & link_url == "osf.io/2pnu6/ and osf.io/arphg/" ~ "osf.io/2pnu6/",
    number_of_links == 2 & link_number == 2
    & link_url == "osf.io/2pnu6/ and osf.io/arphg/" ~ "osf.io/arphg/",
    
    number_of_links == 4 & link_number == 1
    & link_url == "osf.io/cdyf9, osf.io/djxyg, Study 1: osf.io/df74g and Study 2: osf.io/v9b7j" ~ "osf.io/cdyf9",
    number_of_links == 4 & link_number == 2
    & link_url == "osf.io/cdyf9, osf.io/djxyg, Study 1: osf.io/df74g and Study 2: osf.io/v9b7j" ~ "osf.io/djxyg",
    number_of_links == 4 & link_number == 3
    & link_url == "osf.io/cdyf9, osf.io/djxyg, Study 1: osf.io/df74g and Study 2: osf.io/v9b7j" ~ "osf.io/df74g",
    number_of_links == 4 & link_number == 4
    & link_url == "osf.io/cdyf9, osf.io/djxyg, Study 1: osf.io/df74g and Study 2: osf.io/v9b7j" ~ "osf.io/v9b7j",
    
    number_of_links == 2 & link_number == 1
    & link_url == "Study 1 (https://osf.io/hd42c/) Study 2 (https://osf.io/hd42c/)" ~ "https://osf.io/hd42c/",
    number_of_links == 2 & link_number == 2
    & link_url == "Study 1 (https://osf.io/hd42c/) Study 2 (https://osf.io/hd42c/)" ~ "https://osf.io/hd42c/",
    
    number_of_links == 5 & link_number == 1
    & link_url == "https://osf.io/jckvq/, https://osf.io/6n3yq/, https://osf.io/5zuq7/, https://osf.io/g5cxj/ and https://osf.io/siqyz/" ~ "https://osf.io/jckvq/",
    number_of_links == 5 & link_number == 2
    & link_url == "https://osf.io/jckvq/, https://osf.io/6n3yq/, https://osf.io/5zuq7/, https://osf.io/g5cxj/ and https://osf.io/siqyz/" ~ "https://osf.io/6n3yq/",
    number_of_links == 5 & link_number == 3
    & link_url == "https://osf.io/jckvq/, https://osf.io/6n3yq/, https://osf.io/5zuq7/, https://osf.io/g5cxj/ and https://osf.io/siqyz/" ~ "https://osf.io/5zuq7/",
    number_of_links == 5 & link_number == 4
    & link_url == "https://osf.io/jckvq/, https://osf.io/6n3yq/, https://osf.io/5zuq7/, https://osf.io/g5cxj/ and https://osf.io/siqyz/" ~ "https://osf.io/g5cxj/",
    number_of_links == 5 & link_number == 5
    & link_url == "https://osf.io/jckvq/, https://osf.io/6n3yq/, https://osf.io/5zuq7/, https://osf.io/g5cxj/ and https://osf.io/siqyz/" ~ "https://osf.io/siqyz/",
    
    number_of_links == 2 & link_number == 1
    & link_url == "beta.ukdataservice.ac.uk/datacatalogue/series/series?id=200001 and beta.ukdataservice.ac.uk/datacatalogue/series/series?id=2000032" ~ "beta.ukdataservice.ac.uk/datacatalogue/series/series?id=200001",
    number_of_links == 2 & link_number == 2
    & link_url == "beta.ukdataservice.ac.uk/datacatalogue/series/series?id=200001 and beta.ukdataservice.ac.uk/datacatalogue/series/series?id=2000032" ~ "beta.ukdataservice.ac.uk/datacatalogue/series/series?id=2000032",
    
    number_of_links == 2 & link_number == 1
    & link_url == "osf.io/u3byh and osf.io/h49y7" ~ "osf.io/u3byh",
    number_of_links == 2 & link_number == 2
    & link_url == "osf.io/u3byh and osf.io/h49y7" ~ "osf.io/h49y7",
    
    TRUE ~ link_url
  )) %>% 
  mutate(resolved_url = case_when(
    number_of_links == 2 & link_number == 1
    & resolved_url == "https://osf.io/ucn6q/ and https://osf.io/wb2yv/" ~ "https://osf.io/ucn6q/",
    number_of_links == 2 & link_number == 2
    & resolved_url == "https://osf.io/ucn6q/ and https://osf.io/wb2yv/" ~ "https://osf.io/wb2yv/",
    
    number_of_links == 2 & link_number == 1
    & resolved_url == "osf.io/6myfj/ and osf.io/tk67x/" ~ "osf.io/6myfj/",
    number_of_links == 2 & link_number == 2
    & resolved_url == "osf.io/6myfj/ and osf.io/tk67x/" ~ "osf.io/tk67x/",
    
    number_of_links == 2 & link_number == 1
    & resolved_url == "osf.io/d2ncs and osf.io/473gq" ~ "osf.io/d2ncs",
    number_of_links == 2 & link_number == 2
    & resolved_url == "osf.io/d2ncs and osf.io/473gq" ~ "osf.io/473gq",
    
    number_of_links == 2 & link_number == 1
    & resolved_url == "osf.io/2pnu6/ and osf.io/arphg/" ~ "osf.io/2pnu6/",
    number_of_links == 2 & link_number == 2
    & resolved_url == "osf.io/2pnu6/ and osf.io/arphg/" ~ "osf.io/arphg/",
    
    number_of_links == 4 & link_number == 1
    & resolved_url == "http://osf.io/cdyf9 http://osf.io/djxyg, Study 1: http://osf.io/df74g and Study 2: http://osf.io/v9b7j." ~ "http://osf.io/cdyf9",
    number_of_links == 4 & link_number == 2
    & resolved_url == "http://osf.io/cdyf9 http://osf.io/djxyg, Study 1: http://osf.io/df74g and Study 2: http://osf.io/v9b7j." ~ "http://osf.io/djxyg",
    number_of_links == 4 & link_number == 3
    & resolved_url == "http://osf.io/cdyf9 http://osf.io/djxyg, Study 1: http://osf.io/df74g and Study 2: http://osf.io/v9b7j." ~ "http://osf.io/df74g",
    number_of_links == 4 & link_number == 4
    & resolved_url == "http://osf.io/cdyf9 http://osf.io/djxyg, Study 1: http://osf.io/df74g and Study 2: http://osf.io/v9b7j." ~ "http://osf.io/v9b7j",
    
    number_of_links == 2 & link_number == 1
    & resolved_url == "https://osf.io/hd42c/ and https://osf.io/hd42c/" ~ "https://osf.io/hd42c/",
    number_of_links == 2 & link_number == 2
    & resolved_url == "https://osf.io/hd42c/ and https://osf.io/hd42c/" ~ "https://osf.io/hd42c/",
    
    number_of_links == 5 & link_number == 1
    & resolved_url == "https://osf.io/jckvq/, https://osf.io/6n3yq/, https://osf.io/5zuq7/, https://osf.io/g5cxj/ and https://osf.io/siqyz/" ~ "https://osf.io/jckvq/",
    number_of_links == 5 & link_number == 2
    & resolved_url == "https://osf.io/jckvq/, https://osf.io/6n3yq/, https://osf.io/5zuq7/, https://osf.io/g5cxj/ and https://osf.io/siqyz/" ~ "https://osf.io/6n3yq/",
    number_of_links == 5 & link_number == 3
    & resolved_url == "https://osf.io/jckvq/, https://osf.io/6n3yq/, https://osf.io/5zuq7/, https://osf.io/g5cxj/ and https://osf.io/siqyz/" ~ "https://osf.io/5zuq7/",
    number_of_links == 5 & link_number == 4
    & resolved_url == "https://osf.io/jckvq/, https://osf.io/6n3yq/, https://osf.io/5zuq7/, https://osf.io/g5cxj/ and https://osf.io/siqyz/" ~ "https://osf.io/g5cxj/",
    number_of_links == 5 & link_number == 5
    & resolved_url == "https://osf.io/jckvq/, https://osf.io/6n3yq/, https://osf.io/5zuq7/, https://osf.io/g5cxj/ and https://osf.io/siqyz/" ~ "https://osf.io/siqyz/",
    
    number_of_links == 2 & link_number == 1
    & resolved_url == "osf.io/u3byh and osf.io/h49y7" ~ "osf.io/u3byh",
    number_of_links == 2 & link_number == 2
    & resolved_url == "osf.io/u3byh and osf.io/h49y7" ~ "osf.io/h49y7",
    
    TRUE ~ resolved_url
  ))



# Remove the records with multiple links from the temporary data entry set,
# and then append the corrected records.
data_entry_temp <- data_entry_temp %>% 
  filter(number_of_links < 2 | is.na(number_of_links) == TRUE) %>% 
  bind_rows(multilink_records)

# Clean up.
rm(list = c("multilink_records"))



# Set the clean data as a separate object.
data_entry_oliver_clean <- data_entry_temp

# Clean up (to avoid confusion when the code for other coders is run).
rm(list = c("data_entry_temp"))

# Export the cleaned coding data set to file (CSV and RDS).
readr::write_rds(x = data_entry_oliver_clean,
                 path = here::here("data", "output", "stage1",
                                   "after_cleaning",
                                   "data_entry_clean_oliver.rds"))

readr::write_csv(x = data_entry_oliver_clean,
                 path = here::here("data", "output", "stage1",
                                   "after_cleaning",
                                   "data_entry_clean_oliver.csv"))