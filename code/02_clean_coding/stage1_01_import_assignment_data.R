# stage1_01_import_assignment_data.R
#
# This script imports the batch assignment records in order to construct a
# database of which coders were assigned which articles. The coding results
# from each coder can then be compared with their assigned articles, to check
# for uncoded articles.

library(tidyverse)
library(here)

# Import the batch register.
batch_reg_raw <- readr::read_csv(file = here::here("data", "raw", "stage1_batches",
                                                   "batch_register - batch_register.csv"))



# Import data about which coders were assigned which batches.
batch_summary_filenames <- dir(path = here::here("data", "raw",
                                                 "stage1_batches"),
                               pattern = "^batch\\d\\d\\d\\d_summary\\.csv",
                               full.names = TRUE)
all_bs <- lapply(X = batch_summary_filenames,
                 FUN = readr::read_csv)
all_bs_appended <- dplyr::bind_rows(all_bs)



# Merge the batch register and appended batch summaries by batch id.
# Use a full join and then check for missing values which indicates mismatches.
all_assigned_articles <- batch_reg_raw %>% 
  full_join(all_bs_appended, by = "batch_id")

check_na <- all_assigned_articles %>% filter_all(any_vars(is.na(.)))



# Check: are there duplicate coder/article matches in this data?
count_coder_article_x <- all_assigned_articles %>% 
  group_by(coder, study_id) %>% count()
print(table(count_coder_article_x$n))


# Export the assigned articles data to file.
readr::write_rds(x = all_assigned_articles,
                 path = here::here("data", "output", "stage1",
                                   "all_assigned_articles.rds"))

readr::write_csv(x = all_assigned_articles,
                 path = here::here("data", "output", "stage1",
                                   "all_assigned_articles.csv"))
