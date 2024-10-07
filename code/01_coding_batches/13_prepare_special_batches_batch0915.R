# 13_prepare_special_batches_batch0915.R ---------------------------------------
#
# This script prepares the files required for stage 1 checking of the Psych
# Science data badge articles 2020-2023.



# Load required libraries ------------------------------------------------------
library(tidyverse)
library(here)



# Import the latest library ----------------------------------------------------
osb_lib_latest <- read_rds(here("data", "output",
                                "osb_lib_with_study_id_latest.rds"))



# Restrict to Psych Science articles 2020-23 with data badges ------------------
osb_lib_for_batch <- osb_lib_latest |> 
  filter(`Publication Title` == "Psychological Science",
         `Publication Year` %in% c(2020, 2021, 2022, 2023),
         OpenDataBadge == TRUE)



# Rename PDFs of articles ------------------------------------------------------
#
# Note that this only needs to be done once.

pdf_rename_list <- osb_lib_for_batch |> 
  filter(rowid > 1663) |> 
  mutate(pdf_filename = paste0(str_replace(DOI, "10.1177/", "pssa_"), ".pdf")) |> 
  mutate(new_pdf_filename = paste0(study_id, ".pdf"))

for (i in 1:nrow(pdf_rename_list)) {
  old_filename <- pull(pdf_rename_list[i, "pdf_filename"])
  new_filename <- pull(pdf_rename_list[i, "new_pdf_filename"])
  if (file.exists(here("data", "temp", old_filename)) == TRUE) {
    file.rename(here("data", "temp", old_filename),
                here("data", "temp", new_filename))
  } else {
    warning(paste0("File ", old_filename, " for article ",
                   pull(pdf_rename_list[i, "study_id"]),
                   " not found."))
  }
}




# Create a batch summary -------------------------------------------------------
batch0915_summary <- osb_lib_for_batch |> 
  mutate(batch_id = "batch0915",
         batch_date = "2024-04-05",
         pdf_filename = paste0(study_id, ".pdf")) |> 
  select(batch_id, batch_date, study_id, Author, `Publication Year`,
         Title, `Publication Title`, DOI, pdf_filename)

write_excel_csv(batch0915_summary, here("data", "raw", "stage1_batches",
                                  "batch0915_summary.csv"),
                na = "NA",
                eol = "\r\n",
                quote = "all")


# Update the batch register ----------------------------------------------------
new_reg_entry <- tibble(batch_id = "batch0915",
                        coder = "elliot",
                        date_assigned = as.Date("2024-04-05"))

old_reg <- read_csv(here("data", "raw", "stage1_batches",
                         "batch_register - batch_register.csv"))

updated_reg <- bind_rows(old_reg, new_reg_entry)

write_excel_csv(updated_reg, here("data", "raw", "stage1_batches",
                                  "batch_register - batch_register.csv"),
                na = "NA",
                eol = "\r\n",
                quote = "all")