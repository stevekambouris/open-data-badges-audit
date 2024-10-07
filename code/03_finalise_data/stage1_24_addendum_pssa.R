# stage1_24_addendum_pssa.R ----------------------------------------------------
#
# This script imports the late data for Psychological Science



# Load required packages -------------------------------------------------------
library(tidyverse)
library(here)
library(ggpubr)



# Import and format Scopus extracts --------------------------------------------
scopus_raw_0408 <- read_csv(here("data", "raw", "scopus_pssa_2004-2008.csv")) |> 
  mutate(`Page start` = as.character(`Page start`))
scopus_raw_0913 <- read_csv(here("data", "raw", "scopus_pssa_2009-2013.csv")) |> 
  mutate(`Page start` = as.character(`Page start`))
scopus_raw_1418 <- read_csv(here("data", "raw", "scopus_pssa_2014-2018.csv")) |> 
  mutate(`Page start` = as.character(`Page start`))
scopus_raw_1923 <- read_csv(here("data", "raw", "scopus_pssa_2019-2023.csv")) |> 
  mutate(`Page start` = as.character(`Page start`))

scopus_fmt_all <- bind_rows(scopus_raw_0408,
                            scopus_raw_0913,
                            scopus_raw_1418,
                            scopus_raw_1923)

table(scopus_fmt_all$`Document Type`, useNA = "always")
table(scopus_fmt_all$Year, useNA = "always")



# Check the Scopus data --------------------------------------------------------
check_scopus_naDOI <- scopus_fmt_all |> 
  filter(is.na(DOI) == TRUE)

check_scopus_dupDOI <- scopus_fmt_all |> 
  group_by(DOI) |> 
  count() |> 
  ungroup() |> 
  filter(n > 1)

check_scopus_dupDOI2 <- scopus_fmt_all |> 
  filter(DOI %in% c("10.1177/0956797614531550",
                    "10.1177/0956797614550327",
                    "10.1177/0956797617697952"))

check_scopus_retracted <- scopus_fmt_all |> 
  filter(`Document Type` == "Retracted")



# Calculate total articles by year ---------------------------------------------
#
# Include all articles in the total article count, even errata, editorials, etc.

pssa_totaln_0413 <- scopus_fmt_all |> 
  filter(Year < 2014) |> 
  group_by(Year) |> 
  count(name = "total_n")

pssa_totaln_1423 <- scopus_fmt_all |> 
  filter(Year > 2013) |> 
  group_by(Year) |> 
  count(name = "total_n")



# Import the latest dataset with open data badge information -------------------
osb_lib <- read_rds(here("data", "output", "osb_lib_with_study_id_latest.rds"))

# Keep only those cases where the article has at least one "real" badge, this
# will remove one case which is just an erratum notice about an article which
# has badges.
data_pssa_badges <- osb_lib |> 
  filter(`Publication Title` == "Psychological Science") |> 
  filter(OpenDataBadge == TRUE |
           OpenMaterialsBadge == TRUE |
           PreregisteredBadge == TRUE |
           PreregisteredPlus == TRUE)

# Show the breakdown of PS articles by year
table(data_pssa_badges$`Publication Year`, useNA = "always")



# Check the merge by DOI -------------------------------------------------------

# Check all articles from the OSB library can be found by DOI in the Scopus
# extracts
check_merge1 <- data_pssa_badges |> 
  anti_join(scopus_fmt_all, by = "DOI")

check_merge2 <- data_pssa_badges |> 
  semi_join(scopus_fmt_all, by = "DOI")

# Attach the document type variable to the OSB library
data_formerge <- scopus_fmt_all |> 
  select(DOI, `Document Type`)

badgearticles_withtype <- data_pssa_badges |> 
  left_join(data_formerge, by = "DOI")

# Check which types of document were awarded a badge of some kind
table(badgearticles_withtype$`Document Type`, useNA = "always")

check_badge1_note <- badgearticles_withtype |> 
  filter(`Document Type` == "Note")

check_badge2_other <- badgearticles_withtype |> 
  filter(! `Document Type` %in% c("Article", "Note"))



# Calculate proportion of PS articles with any badge, then by badge ------------

# Num articles by year with any badge
pssa_badgeany_1423 <- data_pssa_badges |> 
  group_by(`Publication Year`) |> 
  count(name = "n_badge_any") |> 
  ungroup() |> 
  rename("Year" = "Publication Year")

# Num articles by year with data badge
pssa_badgedat_1423 <- data_pssa_badges |> 
  filter(OpenDataBadge == TRUE) |> 
  group_by(`Publication Year`) |> 
  count(name = "n_badge_dat") |> 
  ungroup() |> 
  rename("Year" = "Publication Year")

# Num articles by year with materials badge
pssa_badgemat_1423 <- data_pssa_badges |> 
  filter(OpenMaterialsBadge == TRUE) |> 
  group_by(`Publication Year`) |> 
  count(name = "n_badge_mat") |> 
  ungroup() |> 
  rename("Year" = "Publication Year")

# Num articles by year with preregistered badge
pssa_badgepre_1423 <- data_pssa_badges |> 
  filter(PreregisteredBadge == TRUE | PreregisteredPlus == TRUE) |> 
  group_by(`Publication Year`) |> 
  count(name = "n_badge_pre") |> 
  ungroup() |> 
  rename("Year" = "Publication Year")

# Merge the article counts together
data_pssa_allbadgecounts <- pssa_totaln_1423 |> 
  left_join(pssa_badgeany_1423, by = "Year") |>
  left_join(pssa_badgedat_1423, by = "Year") |> 
  left_join(pssa_badgemat_1423, by = "Year") |> 
  left_join(pssa_badgepre_1423, by = "Year") |> 
  mutate(n_badge_any = replace_na(n_badge_any, 0L),
         n_badge_dat = replace_na(n_badge_dat, 0L),
         n_badge_mat = replace_na(n_badge_mat, 0L),
         n_badge_pre = replace_na(n_badge_pre, 0L)) |> 
  mutate(pct_badge_any = n_badge_any/total_n,
         pct_badge_dat = n_badge_dat/total_n,
         pct_badge_mat = n_badge_mat/total_n,
         pct_badge_pre = n_badge_pre/total_n)



# Plot percentages of published articles with badges ---------------------------
plot_awardrate_byyear <- function(indata, intitle, incolour = "#1387c9ff") {
  outplot <- ggplot(data = indata,
                    aes(x = `Year`,
                        y = `pct_awarded`)) +
    geom_col(colour = "black", fill = incolour) +
    labs(title = intitle,
         x = "Publication Year",
         y = "% published articles") +
    scale_x_continuous(breaks = seq(2014, 2023, by = 1)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.2),
                       labels = seq(0, 100, by = 20),
                       limits = c(0, 1)) +
    theme_bw() +
    theme(axis.title = element_text(colour = "black", size = 10),
          axis.text.x = element_text(colour = "black", size = 9),
          axis.text.y = element_text(colour = "black", size = 9),
          plot.title = element_text(colour = "black", size = 10, face = "bold"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    geom_label(aes(label = paste0(sprintf("%.1f%%", 100*`pct_awarded`),
                                  "\n(n = ", `total_n`, ")")),
               vjust = 0,
               nudge_y = +0.02,
               size = 2.5)
}

plot_badges_awarded_all <- plot_awardrate_byyear(dplyr::rename(data_pssa_allbadgecounts, "pct_awarded" = "pct_badge_any"),
                                                 "(a) Articles awarded any badges",
                                                 "beige")

plot_badges_awarded_dat <- plot_awardrate_byyear(dplyr::rename(data_pssa_allbadgecounts, "pct_awarded" = "pct_badge_dat"),
                                                 "(b) Articles awarded an Open Data badge",
                                                 "#1387c9ff")

plot_badges_awarded_mat <- plot_awardrate_byyear(dplyr::rename(data_pssa_allbadgecounts, "pct_awarded" = "pct_badge_mat"),
                                                 "(c) Articles awarded an Open Materials badge",
                                                 "#F99B1Cff")

plot_badges_awarded_pre <- plot_awardrate_byyear(dplyr::rename(data_pssa_allbadgecounts, "pct_awarded" = "pct_badge_pre"),
                                                 "(d) Articles awarded a Preregistered badge",
                                                 "#F04C3Eff")

plot(plot_badges_awarded_all)
plot(plot_badges_awarded_dat)
plot(plot_badges_awarded_mat)
plot(plot_badges_awarded_pre)

fig_badgesawarded_byyear_combined <- ggarrange(plot_badges_awarded_all,
                                               plot_badges_awarded_dat,
                                               plot_badges_awarded_mat,
                                               plot_badges_awarded_pre,
                                               ncol = 1)

plot(fig_badgesawarded_byyear_combined)

# Export to PNG
ggsave(filename = "databadge_ps_badgesawarded_byyear_combined.png",
       plot = fig_badgesawarded_byyear_combined,
       device = "png",
       path = here("results"),
       width = 15,
       height = 25,
       units = "cm")

# Export to PDF
ggsave(filename = "databadge_ps_badgesawarded_byyear_combined.pdf",
       plot = fig_badgesawarded_byyear_combined,
       device = "pdf",
       path = here("results"),
       width = 15,
       height = 25,
       units = "cm")



# Plot the total number of published articles in PS over time ------------------
pssa_totaln_0423 <- bind_rows(pssa_totaln_0413, pssa_totaln_1423) |> 
  ungroup() |> 
  mutate(`Policy` = case_when(
    `Year` < 2014 ~ "Before Eich (2014)",
    `Year` > 2013 ~ "Eich (2014) inc. badges",
    TRUE ~ NA_character_
  ))

plot_pubcount_all <- ggplot(data = pssa_totaln_0423,
                            aes(x = as.character(`Year`),
                                y = `total_n`,
                                fill = `Policy`)) +
  #geom_col(colour = "black", fill = "#00a083ff") +
  geom_col(colour = "black") +
  labs(title = "Total annual number of articles published in Psychological Science, 2004-2023",
       x = "Publication Year",
       y = "Published articles") +
  #scale_x_continuous(breaks = seq(2004, 2023, by = 1), limits = rev) +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(breaks = seq(0, 350, by = 50),
                     #labels = seq(0, 400, by = 50),
                     limits = c(0, 350)) +
  theme_bw() +
  theme(axis.title = element_text(colour = "black", size = 9),
        axis.text.x = element_text(colour = "black", size = 9),
        axis.text.y = element_text(colour = "black", size = 9),
        plot.title = element_text(colour = "black", size = 9, face = "bold"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(colour = "black", size = 9)) +
  geom_label(aes(label = paste0(`total_n`)),
             hjust = 0,
             vjust = 0.5,
             nudge_y = +2,
             size = 2.5,
             fill = "white") +
  coord_flip() +
  scale_fill_manual(values = c("#00a083ff", "beige"))

plot(plot_pubcount_all)

# Export to PNG
ggsave(filename = "databadge_ps_artcount_byyear_combined.png",
       plot = plot_pubcount_all,
       device = "png",
       path = here("results"),
       width = 15,
       height = 12.5,
       units = "cm")

# Export to PDF
ggsave(filename = "databadge_ps_artcount_byyear_combined.pdf",
       plot = plot_pubcount_all,
       device = "pdf",
       path = here("results"),
       width = 15,
       height = 12.5,
       units = "cm")



# Import and clean the PS 2020-23 coding ---------------------------------------

# Import the batch file for this round of coding
psslate_coding_batch <- read_csv(here("data", "raw", "stage1_batches",
                                      "batch0915_summary.csv"))

# Import the raw coding data file
psslate_coding_raw <- read_csv(here("data", "raw", "stage1_coding",
                                    "EG_coding.csv")) |> 
  rename("study_id" = "article_id")



# Check if all batch records are found in the coding
psslate_check_batch1 <- psslate_coding_batch |> 
  anti_join(psslate_coding_raw, by = "study_id")

psslate_check_batch2 <- psslate_coding_raw |> 
  anti_join(psslate_coding_batch, by = "study_id")



# Check the coding of each variable
table(psslate_coding_raw$has_data_badge, useNA = "always")
table(psslate_coding_raw$article_has_links, useNA = "always")
table(psslate_coding_raw$number_of_links, useNA = "always")
table(psslate_coding_raw$link_work, useNA = "always")
table(psslate_coding_raw$is_repository, useNA = "always")
table(psslate_coding_raw$files_exist, useNA = "always")
table(psslate_coding_raw$download_success, useNA = "always")



# Check has_data_badge
check_psslate_hdb1 <- psslate_coding_raw |> 
  filter(is.na(has_data_badge) == TRUE)



# Check the missing coding issue
psslate_coding_sorted <- psslate_coding_raw |> 
  arrange(study_id, link_number)


# Clean the coding data
psslate_coding_clean <- psslate_coding_raw |> 
  mutate(
    has_data_badge = case_when(
      study_id == "a002209" ~ "yes",
      TRUE ~ has_data_badge),
    data_availability_statement = case_when(
      study_id == "a002209" ~ "yes",
      TRUE ~ data_availability_statement
    ),
    number_of_links = case_when(
      study_id == "a002207" ~ 4L,
      study_id == "a002209" ~ 4L,
      TRUE ~ number_of_links
    ),
    link_number = case_when(
      study_id == "a002209" ~ 4L,
      TRUE ~ link_number
    )
  ) |> 
  mutate(
    study_id = case_when(
      study_id == "a002209" ~ "a002207",
      TRUE ~ study_id
    )
  ) |> 
  mutate(
    files_exist = case_when(
      files_exist == "y" ~ "yes",
      TRUE ~ files_exist
    ),
    download_success = case_when(
      download_success == "yse" ~ "yes",
      TRUE ~ download_success
    )
  ) |> 
  mutate(
    has_data_badge = case_when(
      study_id == "a001736" ~ "no",
      TRUE ~ has_data_badge
    )
  )

# Check for the missing studies again, after cleaning
psslate_check_batch1b <- psslate_coding_batch |> 
  anti_join(psslate_coding_clean, by = "study_id")



# Import the additional late coding done by me ---------------------------------
psslate2_coding_raw <- read_csv(here("data", "raw", "stage1_coding",
                                     "late_data_collection_steve.csv")) |> 
  mutate(date_coded = as.character(date_coded))



# Combine the EG and SK coding
psslate_coding_clean2 <- bind_rows(psslate_coding_clean,
                                   psslate2_coding_raw)

# Check for the missing studies again, after cleaning
psslate_check_batch1c <- psslate_coding_batch |> 
  anti_join(psslate_coding_clean2, by = "study_id")

# Check for duplicates
psslate_check_dups1 <- psslate_coding_clean2 |> 
  group_by(study_id, link_url) |> 
  count() |> 
  filter(n > 1)

# Check the coding of each variable
table(psslate_coding_clean2$has_data_badge, useNA = "always")
table(psslate_coding_clean2$data_availability_statement, useNA = "always")
table(psslate_coding_clean2$article_has_links, useNA = "always")
table(psslate_coding_clean2$number_of_links, useNA = "always")
table(psslate_coding_clean2$link_work, useNA = "always")
table(psslate_coding_clean2$is_repository, useNA = "always")
table(psslate_coding_clean2$files_exist, useNA = "always")
table(psslate_coding_clean2$download_success, useNA = "always")

# Perform finalisation of variables --------------------------------------------

# Check 
check_psslate_das1 <- psslate_coding_clean2 |> 
  filter(has_data_badge == "yes") |> 
  filter(data_availability_statement != "yes")

check_psslate_ahl1 <- psslate_coding_clean2 |> 
  filter(has_data_badge == "yes") |> 
  filter(article_has_links != "yes")

check_psslate_lwk1 <- psslate_coding_clean2 |> 
  filter(has_data_badge == "yes") |> 
  filter(link_work != "yes")

check_psslate_fex1 <- psslate_coding_clean2 |> 
  filter(has_data_badge == "yes") |> 
  filter(files_exist != "yes") |> 
  relocate(coder_initials:article_has_links_other, .after = notes) |> 
  relocate(resolved_url, files_exist, files_exist_other, .after = study_id)
check_psslate_fex2 <- psslate_coding_clean2 |> 
  filter(has_data_badge == "yes") |> 
  filter(link_work == "yes") |> 
  filter(files_exist != "yes")

check_psslate_dls1 <- psslate_coding_clean2 |> 
  filter(has_data_badge == "yes") |> 
  filter(download_success != "yes") |> 
  relocate(files_exist, files_exist_other,
           download_success, download_success_other, 
           link_number, number_of_links, .after = study_id)



psslate_coding_final <- psslate_coding_clean2 |> 
  
  # Filter out the one case which doesn't actually have an open data badge
  filter(has_data_badge == "yes") |> 
  
  # Final coding of data availability statement
  mutate(final_data_avail_statement = data_availability_statement) |> 
  
  # Final coding of article has links
  # - recode the three "other" cases as "yes" - they have some links
  mutate(final_art_has_links = case_when(
    article_has_links == "other" ~ "yes",
    TRUE ~ article_has_links
  )) |> 
  
  # Final coding of link_work
  # - recode the four cases of "other" as "yes" - they "worked".
  # - keep the "no" case as "no".
  mutate(final_link_works = case_when(
    final_art_has_links == "no" ~ NA_character_,
    link_work == "other" ~ "yes",
    TRUE ~ link_work
  )) |> 
  
  # Final coding of files_exist
  mutate(final_files_exist = case_when(
    
    final_link_works == "no" ~ NA_character_,
    is.na(final_link_works) ~ NA_character_,
    
    # These are the cases of link_work "other"
    study_id == "a001780" & link_number == 3 ~ "yes",
    study_id %in% c("a002072", "a001733") &
      files_exist == "other" ~ "no", # Can't access the OSF pages at all
    study_id == "a001692" & link_url == "https://subscribe.billboard.com" ~ "no", # main page of "repo"
    
    # These are the cases of files_exist "other"
    study_id == "a001780" & link_number == 1 ~ "no", # main page of repo
    study_id == "a002145" ~ "no", # link to form to ask permission
    study_id == "a002204" ~ "no", # outdated link no longer hosted on ICPSR
    study_id == "a001807" & files_exist == "other" ~ "no", # main page of repo
    study_id == "a002236" & files_exist == "other" ~ "no", # main page of repo
    study_id == "a002207" & link_number == 1 ~ "yes", # Archived web page with data
    study_id == "a002207" & link_number == 2 ~ "yes", # it's a website with data on it
    study_id == "a002166" & files_exist == "other" ~ "yes", # seems to list files, just need account
    study_id == "a001810" & link_number == 1 ~ "no", # Just a supp. doc with further links to files
    study_id == "a001772" & files_exist == "other" ~ "yes", # Study 1 had no data to share?
    study_id == "a002146" & files_exist == "other" ~ "yes", # There are data files
    study_id == "a001699" & files_exist == "other" ~ "yes", # There are data files
    study_id == "a001169" & files_exist == "other" ~ "no", # main page of repo

    study_id == "a001935" & is.na(files_exist) ~ "yes", # files do exist
    
    TRUE ~ files_exist
  )) |> 
  
  # Final coding of download_success
  mutate(final_download_success = case_when(
    final_files_exist == "no" ~ NA_character_,
    is.na(final_files_exist) ~ NA_character_,
    
    # These are the cases of link_work "other" which had files
    study_id == "a001780" & link_number == 3 ~ "account",
    
    # These are the cases of files_exist "other"
    study_id == "a002207" & link_number == 1 ~ "yes", # Archived web page with data
    study_id == "a002207" & link_number == 2 ~ "yes", # it's a website with data on it
    study_id == "a002166" & files_exist == "other" ~ "account", # seems to list files, just need account
    study_id == "a001772" & files_exist == "other" ~ "yes", # Study 1 had no data to share?
    study_id == "a002146" & files_exist == "other" ~ "yes", # There are data files
    study_id == "a001699" & files_exist == "other" ~ "yes", # There are data files
    
    # These are the cases of download_success "other"
    study_id == "a002179" & download_success == "other" ~ "partial",
    study_id == "a001780" & link_number == 2 ~ "account",
    study_id == "a002168" & download_success == "other" ~ "account",
    study_id == "a001757" & download_success == "other" ~ "account",
    study_id == "a002017" & download_success == "other" ~ "account",
    study_id == "a002193" & download_success == "other" ~ "account",
    
    study_id == "a001935" & is.na(download_success) ~ "yes", # files downloaded
    study_id == "a001891" &
      is.na(download_success) &
      download_success_other == "yes" ~ "yes", # files downloaded
    
    
    TRUE ~ download_success
  )) |> 
  
  # Recoding download_success into version 2
  mutate(final_download_success2 = case_when(
    final_download_success %in% c("account", "partial") ~ "yes",
    TRUE ~ final_download_success
  ))
  

table(psslate_coding_final$final_data_avail_statement, useNA = "always")
table(psslate_coding_final$final_art_has_links, useNA = "always")
table(psslate_coding_final$final_link_works, useNA = "always")
table(psslate_coding_final$final_files_exist, useNA = "always")
table(psslate_coding_final$final_download_success, useNA = "always")
table(psslate_coding_final$final_download_success2, useNA = "always")

temp_check1 <- psslate_coding_final |> 
  filter(is.na(final_download_success))



# Bibliographic details of articles to attach to the coding --------------------

# Import the latest version of the OSB library
osb_lib_raw <- read_rds(here("data", "output",
                             "osb_lib_with_study_id_latest.rds"))

osb_lib_formerge <- osb_lib_raw |> 
  select(study_id, `Publication Year`:`Publication Title`,
         ISSN, DOI, Pages, Issue, Volume,
         OpenDataBadge:PreregisteredPlus)

check_merge3 <- psslate_coding_final |> 
  anti_join(osb_lib_formerge, by = "study_id")

psslate_coding_merged <- psslate_coding_final |> 
  left_join(osb_lib_formerge, by = "study_id")



# Export the finalised late coding to file -------------------------------------

write_rds(psslate_coding_merged,
          here("data", "output", "stage1",
               "late_ps_coding_forsummary.rds"))

write_excel_csv(psslate_coding_merged,
                here("data", "output", "stage1",
                     "late_ps_coding_forsummary.csv"),
                na = "",
                quote = "all",
                eol = "\r\n")



# Import the original coding, keep PS only -------------------------------------

pssmain_coding_final <- read_rds(here("data", "output", "stage1",
                                   "all_final_coding_final.rds")) |> 
  filter(`Publication Title` == "Psychological Science") |> 
  # Remove the records which had been removed from analysis
  filter(! study_id %in% c("a000063", "a001347"))

psslate_coding_final <- read_rds(here("data", "output", "stage1",
                                      "late_ps_coding_forsummary.rds"))



# Combine coding for 2014-23 ---------------------------------------------------
pssall_coding_final <- bind_rows(pssmain_coding_final,
                                 psslate_coding_final) |> 
  arrange(study_id)



# Calculate success rates for articles with multiple links ---------------------
pssall_coding_withrates <- pssall_coding_final |> 
  
  # Summarise final_link_works
  mutate(article_has_links_logical = case_when(
    final_art_has_links == "yes" ~ TRUE,
    final_art_has_links == "no" ~ FALSE,
    TRUE ~ FALSE
  )) |> 
  mutate(link_works_logical = case_when(
    final_link_works == "yes" ~ TRUE,
    final_link_works == "no" ~ FALSE,
    TRUE ~ FALSE
  )) |> 
  group_by(study_id) |> 
  mutate(link_success = mean(link_works_logical),
         n_links = n()) |> 
  ungroup() |> 
  mutate(link_category = case_when(
    article_has_links_logical == FALSE ~ "0",
    n_links == 1 ~ "1",
    n_links == 2 ~ "2",
    n_links >= 3 ~ "3+"
  )) |> 
  mutate(n_links = ifelse(link_category == "0", 0, n_links)) |> 
  
  # Summarise final_files_exist
  mutate(files_exist_logical = case_when(
    final_files_exist == "yes" ~ TRUE,
    final_files_exist == "no" ~ FALSE,
    TRUE ~ FALSE
  )) |> 
  group_by(study_id) |> 
  mutate(files_exist_success = mean(files_exist_logical),
         n_links = n()) |> 
  ungroup() |> 
  
  # Summarise final_download_success
  mutate(dlsuccess2_logical = case_when(
    final_download_success2 == "yes" ~ TRUE,
    final_download_success2 == "no" ~ FALSE,
    TRUE ~ FALSE
  )) |> 
  group_by(study_id) |> 
  mutate(dl_success2 = mean(dlsuccess2_logical),
         n_links = n()) |> 
  ungroup()



# Collapse down to the article level -------------------------------------------

pssall_coding_articlelevel <- pssall_coding_withrates |> 
  group_by(study_id) |> 
  slice_head() |> 
  ungroup()

# Get the response frequencies for each variable
table(pssall_coding_articlelevel$final_data_avail_statement, useNA = "always")
table(pssall_coding_articlelevel$final_art_has_links, useNA = "always")
table(pssall_coding_articlelevel$link_success, useNA = "always")
table(pssall_coding_articlelevel$files_exist_success, useNA = "always")
table(pssall_coding_articlelevel$dl_success2, useNA = "always")




# Break down article success by year -------------------------------------------

byyear_total <- pssall_coding_articlelevel |> 
  group_by(`Publication Year`) |> 
  count(name = "n_total_articles") |> 
  ungroup()

byyear_1das <- pssall_coding_articlelevel |> 
  filter(final_data_avail_statement == "yes") |> 
  group_by(`Publication Year`) |> 
  count(name = "n_success") |> 
  ungroup() |> 
  right_join(byyear_total, by = "Publication Year") |> 
  rowwise() |> 
  mutate(pct_success = n_success/n_total_articles) |> 
  ungroup()

byyear_2ahl <- pssall_coding_articlelevel |> 
  filter(final_art_has_links == "yes") |> 
  group_by(`Publication Year`) |> 
  count(name = "n_success") |> 
  ungroup() |> 
  right_join(byyear_total, by = "Publication Year") |> 
  rowwise() |> 
  mutate(pct_success = n_success/n_total_articles) |> 
  ungroup()

byyear_3lwk <- pssall_coding_articlelevel |> 
  filter(link_success == 1) |> 
  group_by(`Publication Year`) |> 
  count(name = "n_success") |> 
  ungroup() |> 
  right_join(byyear_total, by = "Publication Year") |> 
  rowwise() |> 
  mutate(pct_success = n_success/n_total_articles) |> 
  ungroup()

byyear_4fex <- pssall_coding_articlelevel |> 
  filter(files_exist_success == 1) |> 
  group_by(`Publication Year`) |> 
  count(name = "n_success") |> 
  ungroup() |> 
  right_join(byyear_total, by = "Publication Year") |> 
  rowwise() |> 
  mutate(pct_success = n_success/n_total_articles) |> 
  ungroup()

byyear_5dls <- pssall_coding_articlelevel |> 
  filter(dl_success2 == 1) |> 
  group_by(`Publication Year`) |> 
  count(name = "n_success") |> 
  ungroup() |> 
  right_join(byyear_total, by = "Publication Year") |> 
  rowwise() |> 
  mutate(pct_success = n_success/n_total_articles) |> 
  ungroup()

plot_success_byyear <- function(indata, intitle, incolour = "#1387c9ff") {
  outplot <- ggplot(data = indata,
                    aes(x = `Publication Year`,
                        y = `pct_success`)) +
    geom_col(colour = "black", fill = incolour) +
    labs(title = intitle,
         x = "Publication Year",
         y = "Success rate (%)") +
    scale_x_continuous(breaks = seq(2014, 2023, by = 1)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.2),
                       labels = scales::percent) +
    theme_bw() +
    theme(axis.title = element_text(colour = "black", size = 10),
          axis.text.x = element_text(colour = "black", size = 9),
          axis.text.y = element_text(colour = "black", size = 9),
          plot.title = element_text(colour = "black", size = 10, face = "bold"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    geom_label(aes(label = paste0(sprintf("%.1f%%", 100*`pct_success`),
                                  "\n(n = ", `n_total_articles`, ")")),
               vjust = 1,
               nudge_y = -0.02,
               size = 2.5)
}

fig_success_byyear_1das <- plot_success_byyear(byyear_1das,
                                               "(a) Has a data availability statement",
                                               "#331E36ff")
fig_success_byyear_2ahl <- plot_success_byyear(byyear_2ahl,
                                               "(b) Has link(s)",
                                               "#41337Aff")
fig_success_byyear_3lwk <- plot_success_byyear(byyear_3lwk,
                                               "(c) Has working link(s)",
                                               "#6EA4BFff")
fig_success_byyear_4fex <- plot_success_byyear(byyear_4fex,
                                               "(d) Has working link(s) that lead to files",
                                               "#C2EFEBff")
fig_success_byyear_5dls <- plot_success_byyear(byyear_5dls,
                                               "(e) Has working link(s) that lead to files which can be downloaded",
                                               "#ECFEE8ff")


plot(fig_success_byyear_1das)
plot(fig_success_byyear_2ahl)
plot(fig_success_byyear_3lwk)
plot(fig_success_byyear_4fex)
plot(fig_success_byyear_5dls)

fig_success_byyear_combined <- ggarrange(fig_success_byyear_1das,
                                         fig_success_byyear_2ahl,
                                         fig_success_byyear_3lwk,
                                         fig_success_byyear_4fex,
                                         fig_success_byyear_5dls,
                                         ncol = 1)

plot(fig_success_byyear_combined)

ggsave(filename = "databadge_ps_byyear_combined.png",
       plot = fig_success_byyear_combined,
       device = "png",
       path = here("results"),
       width = 15,
       height = 25,
       units = "cm")

ggsave(filename = "databadge_ps_byyear_combined.pdf",
       plot = fig_success_byyear_combined,
       device = "pdf",
       path = here("results"),
       width = 15,
       height = 25,
       units = "cm")