################################################################################
# stage1_04_check_invalid_coding_data.R
#
# This script checks each coder's cleaned data to ensure that the columns
# contain valid responses. Any invalid responses are output to a specially
# formatted Excel spreadsheet file.

# Load required packages.
library(tidyverse)
library(here)
library(openxlsx)

# Function: create an Excel report of the invalid data.
create_invalid_rpt <- function(invalid_data_list, output_file_name) {
  
  # Create a helper function for creating Excel worksheets.
  create_rpt_sheet <- function(df, sheet_name, hl_cols, add_style = TRUE) {
    
    # Only create a worksheet if the number of rows in the data frame/tibble
    # object has one or more rows.
    if (nrow(df) > 0) {
      
      # Create cell styles for the sheet headers and highlighted cells.
      my_header_style <- openxlsx::createStyle(border = "TopBottomLeftRight",
                                               textDecoration = "bold")
      hlight_style <- openxlsx::createStyle(border = "TopBottomLeftRight",
                                            fgFill = "#FFFF00")
      
      # Add a worksheet.
      openxlsx::addWorksheet(wb, sheet_name)
      
      # Write data to the worksheet.
      openxlsx::writeData(wb, sheet = sheet_name, x = df,
                          borders = "all", headerStyle = my_header_style)
      
      # If specified, add a style to the worksheet.
      if (add_style == TRUE) {
        openxlsx::addStyle(wb = wb,
                           sheet = sheet_name,
                           style = hlight_style,
                           rows = 1:(nrow(df) + 1),
                           cols = hl_cols,
                           gridExpand = TRUE, stack = TRUE)
      }
    }
    
    # Return nothing.
    invisible(NULL)
  }
  
  # Create an Excel file.
  wb <- openxlsx::createWorkbook()
  
  # check_articleid
  create_rpt_sheet(df = invalid_data_list[[1]], sheet_name = "article_id",
                   hl_cols = c(1))
  
  # check_initials
  create_rpt_sheet(df = invalid_data_list[[2]], sheet_name = "coder_initials",
                   hl_cols = c(2))
  
  # check_dates
  create_rpt_sheet(df = invalid_data_list[[3]], sheet_name = "date_coded",
                   hl_cols = c(3))
  
  # check_article_n
  create_rpt_sheet(df = invalid_data_list[[4]], sheet_name = "count_mismatch",
                   hl_cols = c(0), add_style = FALSE)
  
  # check_has_data_badge
  create_rpt_sheet(df = invalid_data_list[[5]], sheet_name = "has_badge",
                   hl_cols = c(4, 5))
  
  # check_data_avail_state
  create_rpt_sheet(df = invalid_data_list[[6]], sheet_name = "avail_statement",
                   hl_cols = c(6, 7))
  
  # check_art_has_link
  create_rpt_sheet(df = invalid_data_list[[7]], sheet_name = "has_links",
                   hl_cols = c(8, 9))
  
  # check_no_link
  create_rpt_sheet(df = invalid_data_list[[8]], sheet_name = "has_link_no",
                   hl_cols = 10:22)
  
  # check_link_numbers
  create_rpt_sheet(df = invalid_data_list[[9]], sheet_name = "number_links",
                   hl_cols = c(10, 11))
  
  # check_link_url
  create_rpt_sheet(df = invalid_data_list[[10]], sheet_name = "link_url",
                   hl_cols = c(12))
  
  # check_link_work
  create_rpt_sheet(df = invalid_data_list[[11]], sheet_name = "link_work",
                   hl_cols = c(13, 14))
  
  # check_link_no_work
  create_rpt_sheet(df = invalid_data_list[[12]], sheet_name = "link_work_no",
                   hl_cols = 15:22)
  
  # check_resolved_url
  create_rpt_sheet(df = invalid_data_list[[13]], sheet_name = "resolved_url",
                   hl_cols = c(15))
  
  # check_is_repo
  create_rpt_sheet(df = invalid_data_list[[14]], sheet_name = "is_repository",
                   hl_cols = c(16, 17))
  
  # check_repo_name
  create_rpt_sheet(df = invalid_data_list[[15]], sheet_name = "repository_name",
                   hl_cols = c(18))
  
  # check_files_exist
  create_rpt_sheet(df = invalid_data_list[[16]], sheet_name = "files_exist",
                   hl_cols = c(19, 20))
  
  # check_no_files_exist
  create_rpt_sheet(df = invalid_data_list[[17]], sheet_name = "files_exist_no",
                   hl_cols = c(21, 22))
  
  # check_dl_success
  create_rpt_sheet(df = invalid_data_list[[18]], sheet_name = "download_success",
                   hl_cols = c(21, 22))

  # Save the Excel workbook to file.
  openxlsx::saveWorkbook(wb, file = output_file_name, overwrite = TRUE)
  
  # End function and return the original data argument invisibly.
  invisible(invalid_data_list)
}


# Function: check the data for invalid values.
check_invalid_data <- function(data_to_check, coder_name, output_path) {
  
  # Create an output filename for the Excel report.
  excel_file_name <- paste0(output_path, "/invalid_data_entry_", coder_name,
                            ".xlsx")
  
  
  # 1. Check for missing article numbers in data.
  check_articleid <- data_to_check %>%
    filter(grepl(pattern = "^a[0-9]{6}$", x = article_id) == FALSE) %>% 
    filter(grepl(pattern = "^intro[0-9]{2}$", x = article_id) == FALSE)
  
  
  # 2. Check for missing coder initials in data.
  check_initials <- data_to_check %>% filter(coder_initials != coder_name)
  #print(table(data_to_check$coder_initials, useNA = "always"))
  
  
  # 3. Check for missing dates in data (not critical).
  check_dates <- data_to_check %>% filter(is.na(date_coded))
  
  
  # 4. Check for article id count/link number count mismatches.
  article_n <- data_to_check %>% 
    group_by(article_id) %>% count()
  
  article_max_links <- data_to_check %>% 
    group_by(article_id) %>% mutate(n_should_appear = max(number_of_links)) %>% 
    mutate(n_should_appear = case_when(
      is.na(n_should_appear) == TRUE ~ 1,
      n_should_appear == 0 ~ 1,
      TRUE ~ n_should_appear)
    ) %>% 
    distinct(article_id, n_should_appear)
  
  check_article_n <- article_n %>% 
    full_join(article_max_links, by = "article_id") %>% 
    filter(n != n_should_appear)
  
  
  # 5. Check the responses for has_data_badge.
  check_has_data_badge <- data_to_check %>% 
    filter(!(has_data_badge %in% c("yes", "no", "other")) |
             (has_data_badge == "other" & is.na(has_data_badge_other)) |
             (has_data_badge != "other" & !is.na(has_data_badge_other)))
  
  
  # 6. Check the responses for data_availability_statement.
  check_data_avail_state <- data_to_check %>% 
    filter(!(data_availability_statement %in% c("yes", "no", "other")) |
             (data_availability_statement == "other" & is.na(data_availability_statement_other)) |
             (data_availability_statement != "other" & !is.na(data_availability_statement_other)))
  
  
  # 7. Check the responses for article_has_links.
  check_art_has_link <- data_to_check %>% 
    filter(!(article_has_links %in% c("yes", "no", "other")) |
             (article_has_links == "other" & is.na(article_has_links_other)) |
             (article_has_links != "other" & !is.na(article_has_links_other)))
  
  
  # 8. Check that if an article is down as having no links, then the remaining
  # fields are blank.
  check_no_link <- data_to_check %>% 
    filter(article_has_links == "no" & (
      (is.na(number_of_links) == FALSE & number_of_links != 0) |
        (!is.na(link_number)) |
        (!is.na(link_url)) |
        (!is.na(link_work)) |
        (!is.na(link_work_noother)) |
        (!is.na(resolved_url)) |
        (!is.na(is_repository)) |
        (!is.na(is_repository_other)) |
        (!is.na(repository_name)) |
        (!is.na(files_exist)) |
        (!is.na(files_exist_other)) |
        (!is.na(download_success)) |
        (!is.na(download_success_other))
    )
    )
  
  
  # 9. Check that if the answer to article_has_links is yes or other, then
  # there is valid data in the number_of_links and link_number fields.
  check_link_numbers <- data_to_check %>% 
    filter(article_has_links != "no") %>% 
    filter(is.na(number_of_links) | is.na(link_number) |
             link_number > number_of_links)
  
  
  # 10. Check that if an article has links, then the link_url field is
  # populated. Don't worry about trying to check the contents right now.
  check_link_url <- data_to_check %>% 
    filter(article_has_links != "no") %>% 
    filter(is.na(link_url))
  
  
  # 11. Check the link_work field (only for those records where 
  # article_has_links is not "no").
  check_link_work <- data_to_check %>% 
    filter(article_has_links != "no") %>% 
    filter(!(link_work %in% c("yes", "no", "other")) |
             (link_work != "yes" & is.na(link_work_noother)) |
             (link_work == "yes" & !is.na(link_work_noother)))
  
  
  # 12. Check that if an article is down as having no links, then the remaining
  # fields are blank.
  check_link_no_work <- data_to_check %>% 
    filter(link_work == "no" & (
      (!is.na(resolved_url)) |
        (!is.na(is_repository)) |
        (!is.na(is_repository_other)) |
        (!is.na(repository_name)) |
        (!is.na(files_exist)) |
        (!is.na(files_exist_other)) |
        (!is.na(download_success)) |
        (!is.na(download_success_other))
    )
    )
  
  
  # 13. Check that if the link_work is not "no", then resolved_url is not blank.
  # Don't worry about trying to check the contents right now.
  check_resolved_url <- data_to_check %>% 
    filter(link_work != "no") %>% 
    filter(is.na(resolved_url))
  
  
  # 14. Check that if the link_work is not "no", then is_repository has a valid
  # value.
  check_is_repo <- data_to_check %>% 
    filter(link_work != "no") %>% 
    filter(!(is_repository %in% c("yes", "no", "other")) |
             (is_repository == "other" & is.na(is_repository_other)) |
             (is_repository != "other" & !is.na(is_repository_other)))
  
  
  # 15. Check that all records for which is_repository is "yes" have something
  # in the repository_name field.
  check_repo_name <- data_to_check %>% 
    filter(link_work != "no") %>% 
    filter(is_repository == "yes" & is.na(repository_name))
  
  
  # 16. Check that all records for which link_work is not "no" have a valid
  # value for files_exist.
  check_files_exist <- data_to_check %>% 
    filter(link_work != "no") %>% 
    filter(!(files_exist %in% c("yes", "no", "other")) |
             (files_exist == "other" & is.na(files_exist_other)) |
             (files_exist != "other" & !is.na(files_exist_other)))
  
  
  # 17. Check that if files_exist is "no", then the remaining fields are blank.
  check_no_files_exist <- data_to_check %>% 
    filter(files_exist == "no" & (
      (!is.na(download_success)) |
        (!is.na(download_success_other))
    )
    )
  
  
  # 18. Check that if files_exist is not "no", then download_success has valid
  # responses.
  check_dl_success <- data_to_check %>% 
    filter(files_exist != "no")  %>% 
    filter(!(download_success %in% c("yes", "no", "other", "partial")) |
             (download_success %in% c("other", "partial") & is.na(download_success_other)) |
             (download_success %in% c("yes", "no") & !is.na(download_success_other)))
  
  
  # Combine all the checking data objects into a list to pass to the report
  # creating function.
  list_for_report <- list(check_articleid, check_initials,
                          check_dates, check_article_n,
                          check_has_data_badge, check_data_avail_state,
                          check_art_has_link, check_no_link,
                          check_link_numbers, check_link_url,
                          check_link_work, check_link_no_work,
                          check_resolved_url, check_is_repo,
                          check_repo_name, check_files_exist,
                          check_no_files_exist, check_dl_success)
  
  
  # Create an Excel report of the invalid data.
  create_invalid_rpt(invalid_data_list = list_for_report,
                    output_file_name = excel_file_name)
  
  
  # End function and return the original data argument invisibly.
  #invisible(data_to_check)
  invisible(list_for_report)
}



################################################################################
#                                                                              #
# Checking raw data entry files (before any cleaning)                          #
#                                                                              #
################################################################################

# Check coder adrian
check_invalid_data(data_to_check = data_entry_adrian_raw,
                   coder_name = "adrian",
                   output_path = here("data", "output", "stage1", "before_cleaning"))


# Check coder aniruddah
check_invalid_data(data_to_check = data_entry_aniruddah_raw,
                   coder_name = "aniruddah",
                   output_path = here("data", "output", "stage1", "before_cleaning"))


# Check coder evelyn
# Note that evelyn's "raw" data check is based on a minimally cleaned version
# of the raw data, due to the insertion of four columns which disrupts the
# formatting of the Excel report.
data_entry_evelyn_raw3 <- data_entry_evelyn_raw %>% 
  select(-c("X2", "X3", "X4", "X5"))
check_invalid_data(data_to_check = data_entry_evelyn_raw3,
                   coder_name = "evelyn",
                   output_path = here("data", "output", "stage1", "before_cleaning"))


# Check coder kongmanas
check_invalid_data(data_to_check = data_entry_kongmanas_raw,
                   coder_name = "kongmanas",
                   output_path = here("data", "output", "stage1", "before_cleaning"))


# Check coder matthew
check_invalid_data(data_to_check = data_entry_matthew_raw,
                   coder_name = "matthew",
                   output_path = here("data", "output", "stage1", "before_cleaning"))


# Check coder michelle
check_invalid_data(data_to_check = data_entry_michelle_raw,
                   coder_name = "michelle",
                   output_path = here("data", "output", "stage1", "before_cleaning"))


# Check coder nguyenlam
check_invalid_data(data_to_check = data_entry_nguyenlam_raw,
                   coder_name = "nguyenlam",
                   output_path = here("data", "output", "stage1", "before_cleaning"))


# Check coder oliver
check_invalid_data(data_to_check = data_entry_oliver_raw,
                   coder_name = "oliver",
                   output_path = here("data", "output", "stage1", "before_cleaning"))


# Check coder phuong
check_invalid_data(data_to_check = data_entry_phuong_raw,
                   coder_name = "phuong",
                   output_path = here("data", "output", "stage1", "before_cleaning"))


# Check coder sophia
check_invalid_data(data_to_check = data_entry_sophia_raw,
                   coder_name = "sophia",
                   output_path = here("data", "output", "stage1", "before_cleaning"))


# Check coder steve
check_invalid_data(data_to_check = data_entry_steve_raw,
                   coder_name = "steve",
                   output_path = here("data", "output", "stage1", "before_cleaning"))


# Check coder andy
check_invalid_data(data_to_check = data_entry_andy_raw,
                   coder_name = "andy",
                   output_path = here("data", "output", "stage1", "before_cleaning"))


################################################################################
#                                                                              #
# Checking cleaned data entry files                                            #
#                                                                              #
################################################################################

# Check coder adrian
check_invalid_data(data_to_check = data_entry_adrian_clean,
                   coder_name = "adrian",
                   output_path = here("data", "output", "stage1",
                                      "after_cleaning"))


# Check coder aniruddah
check_invalid_data(data_to_check = data_entry_aniruddah_clean,
                   coder_name = "aniruddah",
                   output_path = here("data", "output", "stage1",
                                      "after_cleaning"))


# Check coder sophia
check_invalid_data(data_to_check = data_entry_sophia_clean,
                   coder_name = "sophia",
                   output_path = here("data", "output", "stage1",
                                      "after_cleaning"))


# Check coder michelle
check_invalid_data(data_to_check = data_entry_michelle_clean,
                   coder_name = "michelle",
                   output_path = here("data", "output", "stage1",
                                      "after_cleaning"))


# Check coder nguyenlam
check_invalid_data(data_to_check = data_entry_nguyenlam_clean,
                   coder_name = "nguyenlam",
                   output_path = here("data", "output", "stage1",
                                      "after_cleaning"))


# Check coder phuong
check_invalid_data(data_to_check = data_entry_phuong_clean,
                   coder_name = "phuong",
                   output_path = here("data", "output", "stage1",
                                      "after_cleaning"))


# Check coder evelyn
check_invalid_data(data_to_check = data_entry_evelyn_clean,
                   coder_name = "evelyn",
                   output_path = here("data", "output", "stage1",
                                      "after_cleaning"))


# Check coder matthew
check_invalid_data(data_to_check = data_entry_matthew_clean,
                   coder_name = "matthew",
                   output_path = here("data", "output", "stage1",
                                      "after_cleaning"))


# Check coder kongmanas
check_invalid_data(data_to_check = data_entry_kongmanas_clean,
                   coder_name = "kongmanas",
                   output_path = here("data", "output", "stage1",
                                      "after_cleaning"))


# Check coder oliver
check_invalid_data(data_to_check = data_entry_oliver_clean,
                   coder_name = "oliver",
                   output_path = here("data", "output", "stage1",
                                      "after_cleaning"))


# Check coder steve
check_invalid_data(data_to_check = data_entry_steve_clean,
                   coder_name = "steve",
                   output_path = here("data", "output", "stage1",
                                      "after_cleaning"))


# Check coder andy
check_invalid_data(data_to_check = data_entry_andy_clean,
                   coder_name = "andy",
                   output_path = here("data", "output", "stage1",
                                      "after_cleaning"))