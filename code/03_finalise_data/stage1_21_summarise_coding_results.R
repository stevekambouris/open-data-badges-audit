# stage1_21_summarise_coding_results.R -----------------------------------------
#
# This script takes the final cleaned data set and summarises the results.



# Load required packages -------------------------------------------------------
library(tidyverse)
library(here)
library(ggpubr)
library(alluvial)

# Import the final cleaned coding data -----------------------------------------
data_coding_final <- read_rds(here("data", "output", "stage1",
                                   "all_final_coding_final.rds"))



# Import the latest version of the OSB library ---------------------------------
osb_lib_raw <- read_rds(here("data", "output",
                             "osb_lib_with_study_id_latest.rds"))

# Special step until it is cleaned up in 12_update_zotero_lib_2024-08-09.R:
# fix up the effective year of three Internet Archaeology articles published
# in 2014.
osb_lib_fmt <- osb_lib_raw |> 
  mutate(effective_year = case_when(
    `Publication Title` == "Internet Archaeology" & 
      effective_year == 2014 ~ 2015,
    TRUE ~ effective_year
  ))


# Check if all studies can be found in the OSB library
check_study_id1 <- osb_lib_fmt |> 
  semi_join(data_coding_final, by = "study_id")

check_study_id2 <- data_coding_final |> 
  anti_join(osb_lib_fmt, by = "study_id")

check_study_id3 <- osb_lib_fmt |> 
  anti_join(data_coding_final, by = "study_id") |> 
  filter(OpenDataBadge == TRUE, `Publication Year` < 2020) |> 
  arrange(`Publication Year`)

table(check_study_id3$`Publication Year`, useNA = "always")
table(check_study_id3$effective_year, useNA = "always")

data_bib_info <- check_study_id1



# Summarise the bibliographic data ---------------------------------------------

# Break down the number of articles by publication year
result_bib_byyear <- data_bib_info |> 
  group_by(effective_year) |> 
  count() |> 
  ungroup()

fig_bib_byyear <- ggplot(data = result_bib_byyear,
                             aes(x = effective_year, y = `n`)) +
  geom_col(colour = "black", fill = "#1387c9ff") +
  labs(x = "Publication Year", y = "Number of articles") +
  scale_x_continuous(breaks = seq(2014, 2019, by = 1)) +
  scale_y_continuous(breaks = seq(0, 500, by = 50)) +
  theme_bw() +
  theme(axis.title = element_text(colour = "black", size = 12),
        axis.text.x = element_text(colour = "black", size = 10),
        axis.text.y = element_text(colour = "black", size = 10),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())


# Break down the number of articles by journal title
result_bib_byjnl <- data_bib_info |> 
  group_by(`Publication Title`) |> 
  count() |> 
  ungroup()

fig_bib_byjnl <- ggplot(data = result_bib_byjnl,
                         aes(x = reorder(`Publication Title`, `n`),
                             y = `n`)) +
  geom_col(colour = "black", fill = "#1387c9ff") +
  labs(y = "Number of articles") +
  scale_y_continuous(breaks = seq(0, 500, by = 50)) +
  theme_bw() +
  theme(axis.title = element_text(colour = "black", size = 12),
        axis.text.x = element_text(colour = "black", size = 8),
        axis.text.y = element_text(colour = "black", size = 8),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.y = element_blank()) +
  coord_flip()


# Break down the proportions of articles by badge combination
result_bib_badgecombo <- data_bib_info |> 
  # Add badge combination flags
  mutate(badge_combination = case_when(
    OpenDataBadge == TRUE & OpenMaterialsBadge == FALSE & PreregisteredBadge == FALSE ~ "D",
    OpenDataBadge == FALSE & OpenMaterialsBadge == TRUE & PreregisteredBadge == FALSE ~ "M",
    OpenDataBadge == FALSE & OpenMaterialsBadge == FALSE & PreregisteredBadge == TRUE ~ "P",
    OpenDataBadge == TRUE & OpenMaterialsBadge == TRUE & PreregisteredBadge == FALSE ~ "DM",
    OpenDataBadge == TRUE & OpenMaterialsBadge == FALSE & PreregisteredBadge == TRUE ~ "DP",
    OpenDataBadge == FALSE & OpenMaterialsBadge == TRUE & PreregisteredBadge == TRUE ~ "MP",
    OpenDataBadge == TRUE & OpenMaterialsBadge == TRUE & PreregisteredBadge == TRUE ~ "DMP",
    TRUE ~ NA_character_
  )) |> 
  group_by(badge_combination) |> 
  count() |> 
  ungroup() |> 
  mutate(badge_combination_label = case_when(
    badge_combination == "D" ~ "Data only",
    badge_combination == "DM" ~ "Data + Materials",
    badge_combination == "DMP" ~ "Data + Materials + Preregistered",
    badge_combination == "DP" ~ "Data + Preregistered",
    badge_combination == "M" ~ "Materials",
    badge_combination == "MP" ~ "Materials + Preregistered",
    badge_combination == "P" ~ "Preregistered",
    TRUE ~ NA_character_
  )) |> 
  arrange(desc(badge_combination_label)) |> 
  mutate(pct = n/sum(n)) |> 
  mutate(cum_pct = cumsum(pct)) |> 
  mutate(cum_pct_mid = cum_pct - 0.5*pct) |> 
  arrange(badge_combination_label)

# Create vectors for annotations
anno_badge_combo_label <- result_bib_badgecombo$badge_combination_label
anno_badge_combo_label[2] <- "Data +\nMaterials +\nPreregistered"
anno_badge_combo_label[3] <- "Data +\nPreregistered"
anno_badge_combo_y <- result_bib_badgecombo$cum_pct_mid

# Create the badge combination figure
fig_bib_bycombo <- ggplot(data = result_bib_badgecombo,
                           aes(x = "1",
                               y = `n`,
                               fill = `badge_combination_label`)) +
  geom_col(position = "fill", colour = "black", width = 0.333) + 
  annotate("text",
           x = "1",
           y = anno_badge_combo_y[c(1, 4)],
           label = anno_badge_combo_label[c(1, 4)],
           vjust = 0, size = 2.8) +
  annotate("text",
           x = "1",
           y = anno_badge_combo_y[c(2)],
           label = anno_badge_combo_label[c(2)],
           vjust = 0.5, size = 2.8) +
  annotate("text",
           x = "1",
           y = anno_badge_combo_y[c(3)],
           label = anno_badge_combo_label[c(3)],
           vjust = -2.0, size = 2.8) +
  labs(y = "Percentage of articles with badges",
       x = NULL) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = NULL) +
  theme_bw() +
  theme(axis.title = element_text(colour = "black", size = 12),
        axis.text.x = element_text(colour = "black", size = 10),
        axis.text.y = element_text(colour = "black", size = 10),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "bottom",
        axis.ticks.y = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(colour = "black", size = 6)) +
  coord_flip()

# Export the figures to file
ggsave(filename = "databadge_bib_summary_byyear.png",
       plot = fig_bib_byyear,
       device = "png",
       path = here("results"),
       width = 15,
       height = 10,
       units = "cm")

ggsave(filename = "databadge_bib_summary_bycombo.png",
       plot = fig_bib_bycombo,
       device = "png",
       path = here("results"),
       width = 15,
       height = 10,
       units = "cm")

ggsave(filename = "databadge_bib_summary_byjnl.png",
       plot = fig_bib_byjnl,
       device = "png",
       path = here("results"),
       width = 15,
       height = 25,
       units = "cm")



# 1. Does the article have an Open Data badge? ---------------------------------

# Summarise the count of responses to the has_data_badge variable
results_step1 <- data_coding_final |> 
  group_by(study_id) |> 
  slice_head() |> 
  ungroup() |> 
  group_by(has_data_badge) |> 
  count()

# Check cases of "other"
results_step1_check1 <- data_coding_final |> 
  filter(has_data_badge == "other")

# Decide to remove two articles which don't count as having badges (one with a
# badge in error, one which was retracted).
to_remove_nobadge <- c("a000063", "a001347")

# Create a data frame which removes the two articles which ought not count as
# articles with badges.
data_coding_forresults <- data_coding_final |> 
  filter(! study_id %in% to_remove_nobadge)



# 2. Does the article have a data availability statement? ----------------------
# 3. Does the article have a link to data? -------------------------------------

# Calculate the percentage of articles with at least one link to data,
# broken down by whether or not the article has a data availability statement.
#
# This will be the first result to appear in a table in the Results section.

results_haslinks_bydas <- data_coding_forresults |> 
  group_by(study_id) |> 
  slice_head() |> 
  ungroup() |> 
  group_by(final_data_avail_statement, final_art_has_links) |> 
  count() |> 
  ungroup() |> 
  pivot_wider(names_from = final_art_has_links, names_prefix = "has_link_",
              values_from = n) |> 
  rowwise() |> 
  mutate(`N` = sum(has_link_yes, has_link_no)) |> 
  mutate(`pct_with_links` = round(100*has_link_yes/N, 1)) |> 
  ungroup() |> 
  select(final_data_avail_statement, N, pct_with_links) |> 
  arrange(desc(final_data_avail_statement))

# Check the details of the cases which don't have a data availability statement
check_nodas1 <- data_coding_forresults |> 
  filter(final_data_avail_statement == "no") |> 
  group_by(study_id) |> 
  slice_head() |> 
  ungroup()

# Check the details of the cases which don't have a data link
check_nolink1 <- data_coding_forresults |> 
  filter(final_art_has_links == "no") |> 
  group_by(study_id) |> 
  slice_head() |> 
  ungroup()

# Get the count of journal titles without links to data
result_jnl_nolink <- check_nolink1 |> 
  group_by(`Publication Title`) |> 
  count() |> 
  arrange(desc(n), `Publication Title`) |> 
  ungroup()

# Break down the articles by how many (nominal) links to data they contain
results_numlinks <- data_coding_forresults |> 
  filter(is.na(link_url_cleaned) == FALSE) |> 
  group_by(study_id) |> 
  count(name = "num_of_links") |> 
  group_by(num_of_links) |> 
  count()

# Calculate the total number of (nominal) links to data were captured
results_totallinks <- results_numlinks |> 
  rowwise() |> 
  mutate(total_link = num_of_links * n) |> 
  ungroup() |> 
  summarise(total_number_of_links = sum(total_link))



# 4. Do the link(s) work? ------------------------------------------------------

# Determine the number of non-hyperlink cases
results_linktypes <- data_coding_forresults |> 
  filter(final_art_has_links == "yes") |> 
  filter(link_work == "other")

# Get the number of cases which required a "trivial" edit to the URL
results_links_needing_fix <- data_coding_forresults |> 
  filter(final_art_has_links == "yes") |> 
  filter(link_work == "other") |> 
  filter(link_work_noother %in% c("the encoding of the hyperlink in the pdf is garbled but it is still unambiguous what the url to data is",
                                  "the encoding of the hyperlink in the pdf is garbled (line break) but it is still unambiguous what the url to data is",
                                  "Embedded link had a problem so clicking directly resulted in an error but url as written in text was OK",
                                  "Embedded link had a problem so clicking directly resulted in an error but url as written in text was OK - minor edit required to replace the hyphen in the url text",
                                  "The link as provided is malformed, but is simple and straightforward to fix (add the two forward slashes). After fixed url text, link resolves correctly",
                                  "This URL is malformed; the doi.org/ part is missing, it worked once corrected",
                                  "The link as provide is slightly awkward to work with (split over multiple line breaks, hyperlink encoding incomplete) but the text of the url is complete and correct.",
                                  "The embedded link in the PDF is broken and includes a lot of the paragraph text following the actual url. The link as it appears in the text is correct, though.",
                                  "The embedded url and the in-text url have issues that renders a straight-up attempt to follow the link a failure. However, it is straightforward to fix the url (do not include the invalid protocol at the start and omit the invalid characters at the end) so that it works.",
                                  "There was a typo in the URL as supplied, easily fixed",
                                  "Encoding issue with hyphens/dashes when copied and pasted directly from PDF",
                                  "the encoding of the hyperlink in the pdf is garbled (line break) but it is still unambiguous what the url to data is",
                                  "there is a typo in the document, https://osf.io/3wz2j/ works and leads to the correct repository page",
                                  "link is garbled version of two separate links pointing to the same place; easy to determine the intended link",
                                  "URL as written led to a \"page not found\" error. Omitting the view_only slug resulted in a valid destination",
                                  "The link for this DOI is malformed, but can fairly easily be corrected. Once corrected, it works",
                                  "Embedded link had a problem so clicking directly resulted in an error but url as written in text was OK",
                                  "Embedded url has an invalid character - it should be a hyphen. Fixing the hyphen makes the url behave as expected",
                                  "This link unsuccessfully attempts to combine the dataverse url with a doi-based url and doesn't work as printed - using the final part of the url (just the doi url) works though - https://doi.org/10.7910/DVN/GSQG1K",
                                  "Had to edit URL to remove non-alpha characters around the \"&\" in the string. Also, this is a link to the upload page for the authors - it is not for public use!!!",
                                  "The link as copied contains a bit of extraneous text at the end (\";the\"), removing this makes the url work fine. Clicking on the link directly from PDF results in an error",
                                  "The embedded link does not work as it is a combining of two separate urls, one of which is irrelevant, also spaces are introduced. After correcting the obvious problems with the url, the link works as expected",
                                  "hyphen introduced into url by line break. Clicking link directly results in an error. Once removed, link works as expected",
                                  "The url as provided is malformed, fixing the url was easy (\":\" becomes \".org/\"). Once fixed, the url resolved."))

# Get the cases where the links did not work
results_links_notworking <- data_coding_forresults |> 
  filter(final_link_works == "no")

# Get the success rates of working links, by article

# Calculate data which calculates a success rate for each individual study ID
# (to collapse multiple-link studies into a single record).
data_linkwork <- data_coding_forresults |> 
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
  mutate(n_links = ifelse(link_category == "0", 0, n_links))

# Calculate the link-work success rates for all links consolidated by study ID.
results_linkwork_summary <- data_linkwork |> 
  select(study_id,
         article_has_links_logical,
         link_success,
         link_category) |> 
  distinct() |> 
  group_by(link_category) |> 
  summarise(n = n(),
            all_passed = round(100*mean(link_success == 1), 1),
            all_failed = round(100*mean(link_success == 0), 1),
            mixed = round(100*mean(link_success > 0 & link_success < 1), 1))



# 5. Do the (working) links lead to data files? --------------------------------

# Calculate data which calculates a success rate for each individual study ID
# (to collapse multiple-link studies into a single record).
data_hasfiles <- data_linkwork |> 
  mutate(files_exist_logical = case_when(
    final_files_exist == "yes" ~ TRUE,
    final_files_exist == "no" ~ FALSE,
    TRUE ~ FALSE
  )) |> 
  group_by(study_id) |> 
  mutate(files_exist_success = mean(files_exist_logical),
         n_links = n()) |> 
  ungroup()

# Calculate the files-exist success rates for all links consolidated by
# study ID.
results_files_exist_summary <- data_hasfiles |> 
  select(study_id,
         files_exist_success,
         link_category) |> 
  distinct() |> 
  group_by(link_category) |> 
  summarise(n = n(),
            all_passed = round(100*mean(files_exist_success == 1), 1),
            all_failed = round(100*mean(files_exist_success == 0), 1),
            mixed = round(100*mean(files_exist_success > 0 & files_exist_success < 1), 1))

# Check the cases which did not lead to files
table(data_hasfiles$final_files_exist, useNA = "always")

check_nofiles1 <- data_hasfiles |> 
  filter((final_files_exist == "no" | is.na(final_files_exist)),
         final_art_has_links == "yes") |> 
  arrange(final_files_exist, study_id) |> 
  mutate(final_files_exist_failure_note = case_when(
    is.na(final_files_exist) == TRUE ~ "Link coded as not working",
    study_id %in% c("a000295", "a000501",
                    "a000518", "a000688",
                    "a000785", "a000845",
                    "a001121", "a001215",
                    "a001247", "a001285",
                    "a001296") ~ "Link led to main page of repository (IRIS)",
    study_id %in% c("a000011", "a000160",
                    "a001490", "a000031",
                    "a000280", "a000948") ~ "Link led to main page of repository (other)",
    study_id %in% c("a000655", "a000935",
                    "a000965", "a000581") ~ "Link led to opaque login page",
    link_url_cleaned == "midus.colectica.org" ~ "Link led to opaque login page",
    link_url_cleaned == "hrsonline.isr.umich.edu" ~ "Link led to main page of repository (other)",
    study_id %in% c("a000132",
                    "a000260") ~ "Link led to page detailing how to request data",
    study_id %in% c("a000071", "a000117") ~ "Link led to main page of non-data repository",
    link_url_cleaned %in% c("doi.org/10.3886/ICPSR21600.v17",
                            "doi.org/10.3886/ICPSR04652.v6") ~ "Link led to no longer publicly available data due to update",
    study_id %in% c("a000300") ~ "Link led to notice that data has been deleted from repository",
    study_id %in% c("a000330") ~ "Link led to data explicitly for a different article",
    study_id %in% c("a000334") ~ "Link led to DOI metadata",
    study_id %in% c("a000417", "a000793") ~ "Link led to a document of supplemental material on journal website",
    link_url_cleaned %in% c("citsci.geog.mcgill.ca") ~ "Link led to main page of research program unrelated to data",
    study_id %in% c("a001430") ~ "Link led to a page for file submission to repository",
    TRUE ~ NA_character_
  )) |> 
  relocate(final_files_exist,
           final_files_exist_failure_note,
           link_url_cleaned,
           resolved_url_cleaned,
           files_exist,
           files_exist_other,
           all_notes,
           .after = study_id)

# Summarise the count of links without files by failure type
check_nofiles2 <- check_nofiles1 |> 
  group_by(final_files_exist_failure_note) |> 
  count() |> 
  arrange(desc(n))

# Check the cases of the articles that linked to the IRIS repository
check_nofiles3 <- check_nofiles1 |> 
  filter(final_files_exist_failure_note == "Link led to main page of repository (IRIS)")



# 6. Can the data files be downloaded? -----------------------------------------

# Calculate data which calculates a success rate for each individual study ID
# (to collapse multiple-link studies into a single record).

# Use Definition 2 of download success, which recodes "account" and "partial"
# to "yes" - this is the optimistic interpretation
data_dlsuccess2 <- data_hasfiles |> 
  mutate(dlsuccess2_logical = case_when(
    final_download_success2 == "yes" ~ TRUE,
    final_download_success2 == "no" ~ FALSE,
    TRUE ~ FALSE
  )) |> 
  group_by(study_id) |> 
  mutate(dl_success2 = mean(dlsuccess2_logical),
         n_links = n()) |> 
  ungroup()

results_dlsuccess2_summary <- data_dlsuccess2 |> 
  select(study_id,
         dl_success2,
         link_category) |> 
  distinct() |> 
  group_by(link_category) |> 
  summarise(n = n(),
            all_passed = round(100*mean(dl_success2 == 1), 1),
            all_failed = round(100*mean(dl_success2 == 0), 1),
            mixed = round(100*mean(dl_success2 > 0 & dl_success2 < 1), 1))

# Do a separate breakdown of the links using the richer variable
results_dlsuccess_summary_rich <- data_dlsuccess2 |> 
  group_by(final_download_success) |> 
  count()

# Check the three cases of download failure
temp_check_dl1 <- data_dlsuccess2 |> 
  filter(final_download_success == "no")



# Summarise the repositories used ----------------------------------------------

# Check cases where the repository flag is blank
check_norepoflag <- data_coding_forresults |> 
  filter(is.na(is_repository))

# Check repository flag and name for cases which have been coded as having
# no links, or non-working links
check_norepoflag2 <- data_coding_forresults |> 
  filter(final_art_has_links == "no" | final_link_works == "no")

# Summarise the recognised repositories linked to for all records of working
# links. Calculate the proportion that each repo makes up of the total.
results_repo_names <- data_coding_forresults |> 
  filter(! (final_art_has_links == "no" | final_link_works == "no")) |> 
  group_by(repository_name) |> 
  count() |> 
  arrange(desc(n)) |> 
  ungroup() |> 
  mutate(total_n = sum(n)) |> 
  rowwise() |> 
  mutate(pct = round(100*n/total_n, 1)) |> 
  ungroup() |> 
  mutate(repository_name = case_when(
    is.na(repository_name) == TRUE ~ "(Not recognised as a data repository)",
    TRUE ~ repository_name
  ))

# Export the full table of recognised repositories to file.
write_excel_csv(x = results_repo_names,
                file = here("results",
                            "databadge_repo_summary_allworkinglinks.csv"),
                eol = "\r\n",
                na = "",
                quote = "all")

# Alternative: summarise the repositories in terms of the number of articles
# in the data set (n = 1,208) which have a link to that repository. Clean the
# dataset to include only the distinct repository/study_id combinations.
# Still need to remove the non-working and no-link articles, since they won't
# count, and will only inflate the repository name = NA group
results_repo_byarticle <- data_coding_forresults |> 
  filter(! (final_art_has_links == "no" | final_link_works == "no")) |> 
  mutate(repository_name = case_when(
    repository_name == "ReShare (UK Data Service)" ~ "UK Data Service",
    TRUE ~ repository_name
  )) |> 
  distinct(study_id, repository_name) |> 
  group_by(repository_name) |> 
  count() |> 
  arrange(desc(n), repository_name) |> 
  ungroup() |> 
  rowwise() |> 
  mutate(pct = round(100*n/1208, 1)) |> 
  ungroup() |> 
  mutate(repository_name = case_when(
    is.na(repository_name) == TRUE ~ "(Not recognised as a data repository)",
    TRUE ~ repository_name
  ))

# Check that there are 859 unique articles which linked to (at least one) OSF
# repo
check_repo_osf1 <- data_coding_forresults |> 
  filter(! (final_art_has_links == "no" | final_link_works == "no")) |> 
  filter(repository_name == "Open Science Framework") |> 
  distinct(study_id)

# Check that 32 unique articles linked to (at least one) non-repository
# resource
check_repo_na1 <- data_coding_forresults |> 
  filter(! (final_art_has_links == "no" | final_link_works == "no")) |> 
  filter(is.na(repository_name) == TRUE) |> 
  distinct(study_id)

# Export the full table of recognised repository counts by number of unique
# articles to file.
write_excel_csv(x = results_repo_byarticle,
                file = here("results",
                            "databadge_repo_summary_by_num_articles.csv"),
                eol = "\r\n",
                na = "",
                quote = "all")

# Break down the journal titles which linked to OSF
results_repo_osf_byjnl <- data_coding_forresults |> 
  filter(! (final_art_has_links == "no" | final_link_works == "no")) |> 
  filter(repository_name == "Open Science Framework") |> 
  distinct(study_id, `Publication Title`) |> 
  group_by(`Publication Title`) |> 
  count() |> 
  arrange(desc(n))

# Break down the journal titles which linked to Harvard Dataverse
results_repo_hdv_byjnl <- data_coding_forresults |> 
  filter(! (final_art_has_links == "no" | final_link_works == "no")) |> 
  filter(repository_name == "Harvard Dataverse") |> 
  distinct(study_id, `Publication Title`) |> 
  group_by(`Publication Title`) |> 
  count() |> 
  arrange(desc(n))

# Break down the repositories linked to in Psychological Science articles
results_repo_ps_repos <- data_coding_forresults |> 
  filter(! (final_art_has_links == "no" | final_link_works == "no")) |> 
  filter(`Publication Title` == "Psychological Science") |> 
  distinct(study_id, repository_name) |> 
  group_by(repository_name) |> 
  count() |> 
  arrange(desc(n))

# Break down the repositories linked to in AJPS articles
results_repo_ajps_repos <- data_coding_forresults |> 
  filter(! (final_art_has_links == "no" | final_link_works == "no")) |> 
  filter(`Publication Title` == "American Journal of Political Science") |> 
  distinct(study_id, repository_name) |> 
  group_by(repository_name) |> 
  count() |> 
  arrange(desc(n))

# Break down the repositories linked to in JESP articles
results_repo_jesp_repos <- data_coding_forresults |> 
  filter(! (final_art_has_links == "no" | final_link_works == "no")) |> 
  filter(`Publication Title` == "Journal of Experimental Social Psychology") |> 
  distinct(study_id, repository_name) |> 
  group_by(repository_name) |> 
  count() |> 
  arrange(desc(n))

# List the frequency of journal titles found in the coding dataset
check_articles_by_jnl <- data_coding_forresults |> 
  distinct(study_id, `Publication Title`) |> 
  group_by(`Publication Title`) |> 
  count() |> 
  ungroup() |> 
  arrange(desc(n))

# Show the frequency of links found to be to repositories vs non-repos
results_repo_freq <- data_coding_forresults |> 
  filter(! (final_art_has_links == "no" | final_link_works == "no")) |> 
  group_by(is_repository) |> 
  count()

# Look at the cases of non-repository links, for descriptive purposes
check_repoflag3 <- data_coding_forresults |> 
  filter(is_repository == "no") |> 
  mutate(non_repo_description = case_when(
    study_id %in% c("a000071", "a000696") ~ "Website not relevant for hosting research data",
    study_id %in% c("a000219", "a000400",
                    "a000417", "a000532",
                    "a000597", "a000793",
                    "a000952", "a000996",
                    "a001265", "a001356",
                    "a001394", "a001420",
                    "a001545") ~ "Supplementary material section for article on journal publisher website",
    study_id %in% c("a000051", "a000356",
                    "a000484", "a000575",
                    "a000784", "a001111",
                    "a000269") ~ "University/Institutional repository not listed on re3data",
    study_id %in% c("a000389", "a001274") ~ "Website for research lab hosted on university server",
    study_id %in% c("a000011", "a000031",
                    "a001027", "a001116",
                    "a001186", "a001425",
                    "a001634") ~ "Website for a specific research project, not listed on re3data",
    study_id %in% c("a001104") ~ "A shared folder hosted on a cloud storage account",
    TRUE ~ NA_character_
  )) |> 
  relocate(resolved_url_cleaned, non_repo_description, .after = study_id)

check_repoflag3a <- check_repoflag3 |> 
  group_by(non_repo_description) |> 
  count() |> 
  arrange(desc(n))



# Collapse success down to a single result for each article (n = 1,208) --------

# Get a single record per article
data_coding_articlelevel <- data_dlsuccess2 |> 
  group_by(study_id) |> 
  slice_head() |> 
  ungroup() |> 
  mutate(effective_year = case_when(
    `Publication Title` == "Internet Archaeology" &
      effective_year < 2015 ~ 2015,
    TRUE ~ effective_year
  ))

# Get the response frequencies for each variable
table(data_coding_articlelevel$final_data_avail_statement, useNA = "always")
table(data_coding_articlelevel$final_art_has_links, useNA = "always")
table(data_coding_articlelevel$link_success, useNA = "always")
table(data_coding_articlelevel$files_exist_success, useNA = "always")
table(data_coding_articlelevel$dl_success2, useNA = "always")

# Calculate the number of successful articles for each variable
n_success_1das <- data_coding_articlelevel |> 
  filter(final_data_avail_statement == "yes") |> 
  nrow()

n_success_2ahl <- data_coding_articlelevel |> 
  filter(final_art_has_links == "yes") |> 
  nrow()

n_success_3lwk <- data_coding_articlelevel |> 
  filter(link_success == 1) |> 
  nrow()

n_success_4fex <- data_coding_articlelevel |> 
  filter(files_exist_success == 1) |> 
  nrow()

n_success_5dls <- data_coding_articlelevel |> 
  filter(dl_success2 == 1) |> 
  nrow()

# Compile all overall, article-level success counts into a data frame
results_success_overall <- tibble(
  success_category = c("Article has a data availability statement",
                       "Article has link(s)",
                       "Article has working link(s)",
                       "Article has working link(s) that lead to files",
                       "Article has working link(s) that lead to files which can be downloaded"),
  n_success = c(n_success_1das,
                n_success_2ahl,
                n_success_3lwk,
                n_success_4fex,
                n_success_5dls)) |> 
  mutate(pct_success = round(100*n_success/nrow(data_coding_articlelevel), 1))

# Export the overall, article-level success results to file
write_excel_csv(x = results_success_overall,
                file = here("results",
                            "databadge_summary_overall_articlelevel.csv"),
                eol = "\r\n",
                na = "",
                quote = "all")



# Break down article success by year -------------------------------------------

table(data_coding_articlelevel$`Publication Year`, useNA = "always")

temp_check1 <- data_coding_articlelevel |> 
  filter(effective_year == 2014) |> 
  group_by(`Publication Title`) |> 
  count()

byyear_total <- data_coding_articlelevel |> 
  group_by(effective_year) |> 
  count(name = "n_total_articles") |> 
  ungroup()

byyear_1das <- data_coding_articlelevel |> 
  filter(final_data_avail_statement == "yes") |> 
  group_by(effective_year) |> 
  count(name = "n_success") |> 
  ungroup() |> 
  right_join(byyear_total, by = "effective_year") |> 
  rowwise() |> 
  mutate(pct_success = n_success/n_total_articles) |> 
  ungroup()

byyear_2ahl <- data_coding_articlelevel |> 
  filter(final_art_has_links == "yes") |> 
  group_by(effective_year) |> 
  count(name = "n_success") |> 
  ungroup() |> 
  right_join(byyear_total, by = "effective_year") |> 
  rowwise() |> 
  mutate(pct_success = n_success/n_total_articles) |> 
  ungroup()

byyear_3lwk <- data_coding_articlelevel |> 
  filter(link_success == 1) |> 
  group_by(effective_year) |> 
  count(name = "n_success") |> 
  ungroup() |> 
  right_join(byyear_total, by = "effective_year") |> 
  rowwise() |> 
  mutate(pct_success = n_success/n_total_articles) |> 
  ungroup()

byyear_4fex <- data_coding_articlelevel |> 
  filter(files_exist_success == 1) |> 
  group_by(effective_year) |> 
  count(name = "n_success") |> 
  ungroup() |> 
  right_join(byyear_total, by = "effective_year") |> 
  rowwise() |> 
  mutate(pct_success = n_success/n_total_articles) |> 
  ungroup()

byyear_5dls <- data_coding_articlelevel |> 
  filter(dl_success2 == 1) |> 
  group_by(effective_year) |> 
  count(name = "n_success") |> 
  ungroup() |> 
  right_join(byyear_total, by = "effective_year") |> 
  rowwise() |> 
  mutate(pct_success = n_success/n_total_articles) |> 
  ungroup()

plot_success_byyear <- function(indata, intitle, incolour = "#1387c9ff") {
  outplot <- ggplot(data = indata,
                    aes(x = `effective_year`,
                        y = `pct_success`)) +
    geom_col(colour = "black", fill = incolour) +
    labs(title = intitle,
         x = "Publication Year",
         y = "Success rate (%)") +
    scale_x_continuous(breaks = seq(2014, 2019, by = 1)) +
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
               size = 3)
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

ggsave(filename = "databadge_success_byyear_combined.png",
       plot = fig_success_byyear_combined,
       device = "png",
       path = here("results"),
       width = 15,
       height = 25,
       units = "cm")



# Break down article success by journal ----------------------------------------

temp_check2 <- data_coding_articlelevel |> 
  group_by(`Publication Title`) |> 
  count() |> 
  ungroup() |> 
  arrange(desc(n))

byjnl_total <- data_coding_articlelevel |> 
  group_by(`Publication Title`) |> 
  count(name = "n_total_articles") |> 
  ungroup()

byjnl_1das <- data_coding_articlelevel |> 
  filter(final_data_avail_statement == "yes") |> 
  group_by(`Publication Title`) |> 
  count(name = "n_success") |> 
  ungroup() |> 
  right_join(byjnl_total, by = "Publication Title") |> 
  mutate(n_success = tidyr::replace_na(n_success, 0L)) |> 
  rowwise() |> 
  mutate(pct_success = n_success/n_total_articles) |> 
  ungroup()

byjnl_2ahl <- data_coding_articlelevel |> 
  filter(final_art_has_links == "yes") |> 
  group_by(`Publication Title`) |> 
  count(name = "n_success") |> 
  ungroup() |> 
  right_join(byjnl_total, by = "Publication Title") |> 
  mutate(n_success = tidyr::replace_na(n_success, 0L)) |> 
  rowwise() |> 
  mutate(pct_success = n_success/n_total_articles) |> 
  ungroup()

byjnl_3lwk <- data_coding_articlelevel |> 
  filter(link_success == 1) |> 
  group_by(`Publication Title`) |> 
  count(name = "n_success") |> 
  ungroup() |> 
  right_join(byjnl_total, by = "Publication Title") |> 
  mutate(n_success = tidyr::replace_na(n_success, 0L)) |> 
  rowwise() |> 
  mutate(pct_success = n_success/n_total_articles) |> 
  ungroup()

byjnl_4fex <- data_coding_articlelevel |> 
  filter(files_exist_success == 1) |> 
  group_by(`Publication Title`) |> 
  count(name = "n_success") |> 
  ungroup() |> 
  right_join(byjnl_total, by = "Publication Title") |> 
  mutate(n_success = tidyr::replace_na(n_success, 0L)) |> 
  rowwise() |> 
  mutate(pct_success = n_success/n_total_articles) |> 
  ungroup()

byjnl_5dls <- data_coding_articlelevel |> 
  filter(dl_success2 == 1) |> 
  group_by(`Publication Title`) |> 
  count(name = "n_success") |> 
  ungroup() |> 
  right_join(byjnl_total, by = "Publication Title") |> 
  mutate(n_success = tidyr::replace_na(n_success, 0L)) |> 
  rowwise() |> 
  mutate(pct_success = n_success/n_total_articles) |> 
  ungroup()

plot_success_byjnl <- function(indata, intitle, incolour = "#1387c9ff") {
  outplot <- ggplot(data = indata,
                    aes(x = `Publication Title`,
                        y = `pct_success`)) +
    geom_col(colour = "black", fill = incolour) +
    labs(title = intitle,
         y = "Success rate (%)") +
    scale_y_continuous(breaks = seq(0, 1, by = 0.2),
                       labels = scales::percent) +
    theme_bw() +
    theme(axis.title = element_text(colour = "black", size = 10),
          axis.title.y = element_blank(),
          axis.text.x = element_text(colour = "black", size = 8),
          axis.text.y = element_text(colour = "black", size = 8),
          plot.title = element_text(colour = "black", size = 10, face = "bold",
                                    hjust = 1),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    geom_label(aes(label = paste0(sprintf("%.1f%%", 100*`pct_success`),
                                  " (n = ", n_total_articles, ")"),
                   y = 0.01),
               hjust = 0,
               nudge_x = -0.02,
               size = 2.5) +
    scale_x_discrete(limits = rev) +
    coord_flip()
}

fig_success_byjnl_1das <- plot_success_byjnl(byjnl_1das,
                                             "Has a data availability statement",
                                             "#331E36ff")
fig_success_byjnl_2ahl <- plot_success_byjnl(byjnl_2ahl,
                                             "Has link(s)",
                                             "#41337Aff")
fig_success_byjnl_3lwk <- plot_success_byjnl(byjnl_3lwk,
                                             "Has working link(s)",
                                             "#6EA4BFff")
fig_success_byjnl_4fex <- plot_success_byjnl(byjnl_4fex,
                                             "Has working link(s) that lead to files",
                                             "#C2EFEBff")
fig_success_byjnl_5dls <- plot_success_byjnl(byjnl_5dls,
                                             "Has working link(s) that lead to files which can be downloaded",
                                             "#ECFEE8ff")


# plot(fig_success_byjnl_1das)
# plot(fig_success_byjnl_2ahl)
# plot(fig_success_byjnl_3lwk)
# plot(fig_success_byjnl_4fex)
# plot(fig_success_byjnl_5dls)

ggsave(filename = "databadge_success_byjnl_all_1das.png",
       plot = fig_success_byjnl_1das,
       device = "png",
       path = here("results"),
       width = 15,
       height = 25,
       units = "cm")

ggsave(filename = "databadge_success_byjnl_all_2ahl.png",
       plot = fig_success_byjnl_2ahl,
       device = "png",
       path = here("results"),
       width = 15,
       height = 25,
       units = "cm")

ggsave(filename = "databadge_success_byjnl_all_3lwk.png",
       plot = fig_success_byjnl_3lwk,
       device = "png",
       path = here("results"),
       width = 15,
       height = 25,
       units = "cm")

ggsave(filename = "databadge_success_byjnl_all_4fex.png",
       plot = fig_success_byjnl_4fex,
       device = "png",
       path = here("results"),
       width = 15,
       height = 25,
       units = "cm")

ggsave(filename = "databadge_success_byjnl_all_5dls.png",
       plot = fig_success_byjnl_5dls,
       device = "png",
       path = here("results"),
       width = 15,
       height = 25,
       units = "cm")

# Create a combined plot just for the top 5 articles by count
#
# Specify the top 5 journals to include, by article count
top5_journals <- c("Psychological Science",
                   "American Journal of Political Science",
                   "Journal of Experimental Social Psychology",
                   "Cortex",
                   "The Journal of Social Psychology")

byjnl_top5_total <- byjnl_total |> 
  filter(`Publication Title` %in% top5_journals)

byjnl_top5_1das <- byjnl_1das |> 
  filter(`Publication Title` %in% top5_journals)

byjnl_top5_2ahl <- byjnl_2ahl |> 
  filter(`Publication Title` %in% top5_journals)

byjnl_top5_3lwk <- byjnl_3lwk |> 
  filter(`Publication Title` %in% top5_journals)

byjnl_top5_4fex <- byjnl_4fex |> 
  filter(`Publication Title` %in% top5_journals)

byjnl_top5_5dls <- byjnl_5dls |> 
  filter(`Publication Title` %in% top5_journals)

fig_success_byjnl_top5_1das <- plot_success_byjnl(byjnl_top5_1das,
                                             "(a) Has a data availability statement",
                                             "#331E36ff")
fig_success_byjnl_top5_2ahl <- plot_success_byjnl(byjnl_top5_2ahl,
                                             "(b) Has link(s)",
                                             "#41337Aff")
fig_success_byjnl_top5_3lwk <- plot_success_byjnl(byjnl_top5_3lwk,
                                             "(c) Has working link(s)",
                                             "#6EA4BFff")
fig_success_byjnl_top5_4fex <- plot_success_byjnl(byjnl_top5_4fex,
                                             "(d) Has working link(s) that lead to files",
                                             "#C2EFEBff")
fig_success_byjnl_top5_5dls <- plot_success_byjnl(byjnl_top5_5dls,
                                             "(e) Has working link(s) that lead to files which can be downloaded",
                                             "#ECFEE8ff")

fig_success_byjnl_top5_combined <- ggarrange(fig_success_byjnl_top5_1das,
                                             fig_success_byjnl_top5_2ahl,
                                             fig_success_byjnl_top5_3lwk,
                                             fig_success_byjnl_top5_4fex,
                                             fig_success_byjnl_top5_5dls,
                                             ncol = 1)

plot(fig_success_byjnl_top5_combined)

ggsave(filename = "databadge_success_byjnl_top5_combined.png",
       plot = fig_success_byjnl_top5_combined,
       device = "png",
       path = here("results"),
       width = 15,
       height = 25,
       units = "cm")



# Create an alluvial diagram for the final success rates by article ------------
#
# Abandon this, it doesn't look good at all

# data_foralluvial <- data_coding_articlelevel |> 
#   mutate(
#     das = case_when(
#       final_data_avail_statement == "yes" ~ "success",
#       final_data_avail_statement == "no" ~ "failure",
#       TRUE ~ NA_character_
#     ),
#     ahl = case_when(
#       final_art_has_links == "yes" ~ "success",
#       final_art_has_links == "no" ~ "failure",
#       TRUE ~ NA_character_
#     ),
#     lwk = case_when(
#       final_art_has_links == "no" ~ "(not applicable)",
#       link_success == 1 ~ "success",
#       link_success < 1 ~ "failure",
#       TRUE ~ NA_character_
#     ),
#     fex = case_when(
#       link_success < 1 ~ "(not applicable)",
#       files_exist_success == 1 ~ "success",
#       files_exist_success < 1 ~ "failure",
#       TRUE ~ NA_character_
#     ),
#     dls = case_when(
#       files_exist_success < 1 ~ "(not applicable)",
#       dl_success2 == 1 ~ "success",
#       dl_success2 < 1 ~ "failure",
#       TRUE ~ NA_character_
#     )
#   ) |> 
#   group_by(das, ahl, lwk, fex, dls) |> 
#   count() |> 
#   ungroup()
# 
# plot_test1 <- alluvial(select(data_foralluvial,
#                               das, ahl, lwk, fex, dls),
#                       freq = data_foralluvial$n,
#                       blocks = FALSE)



# Review failures --------------------------------------------------------------

# Look at the cases with no links to data
temp_check3 <- data_coding_forresults |> 
  filter(final_art_has_links == "no")

# Look at the cases with no working links
temp_check4 <- data_coding_forresults |> 
  filter(is.na(final_link_works) == TRUE | final_link_works == "no")

# Look at the cases with no files
all_failures <- data_coding_forresults |> 
  filter(final_files_exist == "no" | is.na(final_files_exist) == TRUE)

write_excel_csv(x = all_failures,
                file = here("results",
                            "databadge_forreview_allfailures.csv"),
                eol = "\r\n",
                na = "",
                quote = "all")

write_excel_csv(x = check_nofiles1,
                file = here("results",
                            "databadge_forreview_file_exists_failure_cats.csv"),
                eol = "\r\n",
                na = "",
                quote = "all")

# Create an updated dataset with reviewed variables for some
data_coding_reviewed_all <- data_coding_forresults |> 
  mutate(review_art_has_links = case_when(
    study_id == "a000831" & final_art_has_links == "no" ~ "yes",
    study_id == "a001051" & final_art_has_links == "no" ~ "yes",
    study_id == "a000773" & final_art_has_links == "no" ~ "yes",
    TRUE ~ final_art_has_links
  )) |> 
  mutate(review_link_url = case_when(
    study_id == "a000831" & final_art_has_links == "no" ~ "data.mendeley.com/datasets/bfgy9bk7n3/2",
    study_id == "a001051" & final_art_has_links == "no" ~ "osf.io/hszyq",
    study_id == "a000773" & final_art_has_links == "no" ~ "osf.io/ehmwp",
    
    study_id == "a000367" & final_link_works == "no" ~ "dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/BRRZSK",
    study_id == "a000777" & final_link_works == "no" ~ "dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/DSDNX6",
    study_id == "a000849" & final_link_works == "no" ~ "dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/KL7J6Z",
    study_id == "a000880" & final_link_works == "no" ~ "https://www.oecd.org/en/data/datasets/pisa-2006-database.html",
    study_id == "a001221" & final_link_works == "no" ~ "dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/HIQ4BV",
    study_id == "a001250" & final_link_works == "no" ~ "search.gesis.org/research_data/ZA6701",
    study_id == "a001293" & final_link_works == "no" ~ "https://www.vliz.be/en/imis?dasid=4687&doiid=329",
    study_id == "a001429" & final_link_works == "no" ~ "osf.io/a42yg",
    study_id == "a001476" & final_link_works == "no" ~ "forsbase.unil.ch/project/study-public-overview/16970/0",
    study_id == "a001501" & final_link_works == "no" ~ "dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/NMJ4W9",
    
    study_id == "a001430" & final_files_exist == "no" ~ "data.mendeley.com/datasets/rh22h25sw7/1",
    study_id == "a000330" & final_files_exist == "no" ~ "dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/XSRNRB",
    study_id == "a000334" & final_files_exist == "no" ~ "dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/KUWA5X",
    study_id == "a001215" & final_files_exist == "no" ~ "www.iris-database.org/iris/app/home/detail?id=york:932759, https://www.iris-database.org/iris/app/home/detail?id=york:932760, https://www.iris-database.org/iris/app/home/detail?id=york:932690",
    study_id == "a000295" & final_files_exist == "no" ~ "www.iris-database.org/iris/app/home/detail?id=york:935345",
    study_id == "a001285" & final_files_exist == "no" ~ "www.iris-database.org/iris/app/home/detail?id=york:935317, https://github.com/izeh/i, https://github.com/rnorouzian/i/blob/master/i.r",
    study_id == "a000845" & final_files_exist == "no" ~ "iris-database.org/iris/app/home/detail?id=york:935657",
    study_id == "a000501" & final_files_exist == "no" ~ "www.iris-database.org/iris/app/home/detail?id=york:935306, https://www.iris-database.org/iris/app/home/detail?id=york:935307, https://www.iris-database.org/iris/app/home/detail?id=york:935308, https://www.iris-database.org/iris/app/home/detail?id=york:935309, https://www.iris-database.org/iris/app/home/detail?id=york:935310, https://www.iris-database.org/iris/app/home/detail?id=york:935311, https://www.iris-database.org/iris/app/home/detail?id=york:935313",
    study_id == "a001121" & final_files_exist == "no" ~ "www.iris-database.org/iris/app/home/detail?id=york:936152",
    study_id == "a000518" & final_files_exist == "no" ~ "iris-database.org/iris/app/home/detail?id=york:934567",
    study_id == "a000688" & final_files_exist == "no" ~ "iris-database.org/iris/app/home/detail?id=york:936142",
    study_id == "a000785" & final_files_exist == "no" ~ "iris-database.org/iris/app/home/detail?id=york:936215, https://github.com/izeh/l/blob/master/d.r",
    study_id == "a001247" & final_files_exist == "no" ~ "iris-database.org/iris/app/home/detail?id=york:936419, https://www.dropbox.com/s/59czjr4u0nm1gsn/data.xlsx?dl=0, https://www.dropbox.com/s/7gqxg9spe2dx9n4/Grammar.exe?dl=0, https://www.dropbox.com/s/uasmo5werd5ebl1/Delayed.exe?dl=0",
    study_id == "a001296" & final_files_exist == "no" ~ "iris-database.org/iris/app/home/detail?id=york:936622, https://iris-database.org/iris/app/home/detail?id=york:936623, https://iris-database.org/iris/app/home/detail?id=york:936624, https://iris-database.org/iris/app/home/detail?id=york:936625, https://iris-database.org/iris/app/home/detail?id=york:936626",
    study_id == "a000948" & final_files_exist == "no" ~ "osf.io/37g64/",
    study_id == "a000160" & final_files_exist == "no" ~ "childes.talkbank.org/access/Eng-UK/Manchester.html; https://childes.talkbank.org/access/Eng-UK/Thomas.html; https://childes.talkbank.org/access/Eng-NA/Brown.html; https://childes.talkbank.org/access/Eng-NA/Bloom.html; https://childes.talkbank.org/access/Eng-NA/Suppes.html; https://childes.talkbank.org/access/Eng-NA/Sachs.html; https://childes.talkbank.org/access/Eng-NA/Kuczaj.html; https://phonbank.talkbank.org/access/Eng-NA/Providence.html",
    study_id == "a001490" & final_files_exist == "no" ~ "osf.io/m75da",
    study_id == "a000300" & final_files_exist == "no" ~ "osf.io/4rs5x/",
    study_id == "a000965" & final_files_exist == "no" & link_url_cleaned == "osf.io/b6n8t" ~ "osf.io/v7amd/",
    
    TRUE ~ NA_character_
  )) |> 
  mutate(review_link_works = case_when(
    study_id == "a000831" & final_art_has_links == "no" ~ "yes",
    study_id == "a001051" & final_art_has_links == "no" ~ "yes",
    study_id == "a000773" & final_art_has_links == "no" ~ "yes",
    
    study_id == "a000367" & final_link_works == "no" ~ "yes",
    study_id == "a000777" & final_link_works == "no" ~ "yes",
    study_id == "a000849" & final_link_works == "no" ~ "yes",
    study_id == "a000880" & final_link_works == "no" ~ "yes",
    study_id == "a001221" & final_link_works == "no" ~ "yes",
    study_id == "a001250" & final_link_works == "no" ~ "yes",
    study_id == "a001293" & final_link_works == "no" ~ "yes",
    study_id == "a001429" & final_link_works == "no" ~ "yes",
    study_id == "a001476" & final_link_works == "no" ~ "yes",
    study_id == "a001501" & final_link_works == "no" ~ "yes",
    
    study_id == "a001430" & final_files_exist == "no" ~ "yes",
    study_id == "a000330" & final_files_exist == "no" ~ "yes",
    study_id == "a000334" & final_files_exist == "no" ~ "yes",
    study_id == "a001215" & final_files_exist == "no" ~ "yes",
    study_id == "a000295" & final_files_exist == "no" ~ "yes",
    study_id == "a001285" & final_files_exist == "no" ~ "yes",
    study_id == "a000845" & final_files_exist == "no" ~ "yes",
    study_id == "a000501" & final_files_exist == "no" ~ "yes",
    study_id == "a001121" & final_files_exist == "no" ~ "yes",
    study_id == "a000518" & final_files_exist == "no" ~ "yes",
    study_id == "a000688" & final_files_exist == "no" ~ "yes",
    study_id == "a000785" & final_files_exist == "no" ~ "yes",
    study_id == "a001247" & final_files_exist == "no" ~ "yes",
    study_id == "a001296" & final_files_exist == "no" ~ "yes",
    study_id == "a000948" & final_files_exist == "no" ~ "yes",
    study_id == "a000160" & final_files_exist == "no" ~ "yes",
    study_id == "a001490" & final_files_exist == "no" ~ "yes",
    study_id == "a000300" & final_files_exist == "no" ~ "yes",
    study_id == "a000965" & final_files_exist == "no" & link_url_cleaned == "osf.io/b6n8t" ~ "yes",
    
    TRUE ~ final_link_works
  )) |> 
  mutate(review_files_exist = case_when(
    study_id == "a000831" & final_art_has_links == "no" ~ "yes",
    study_id == "a001051" & final_art_has_links == "no" ~ "yes",
    study_id == "a000773" & final_art_has_links == "no" ~ "yes",
    
    study_id == "a000367" & final_link_works == "no" ~ "yes",
    study_id == "a000777" & final_link_works == "no" ~ "yes",
    study_id == "a000849" & final_link_works == "no" ~ "yes",
    study_id == "a000880" & final_link_works == "no" ~ "yes",
    study_id == "a001221" & final_link_works == "no" ~ "yes",
    study_id == "a001250" & final_link_works == "no" ~ "yes",
    study_id == "a001293" & final_link_works == "no" ~ "yes",
    study_id == "a001429" & final_link_works == "no" ~ "yes",
    study_id == "a001476" & final_link_works == "no" ~ "yes",
    study_id == "a001501" & final_link_works == "no" ~ "yes",
    
    study_id == "a001430" & final_files_exist == "no" ~ "yes",
    study_id == "a000330" & final_files_exist == "no" ~ "yes",
    study_id == "a000334" & final_files_exist == "no" ~ "yes",
    study_id == "a001215" & final_files_exist == "no" ~ "yes",
    study_id == "a000295" & final_files_exist == "no" ~ "yes",
    study_id == "a001285" & final_files_exist == "no" ~ "yes",
    study_id == "a000845" & final_files_exist == "no" ~ "yes",
    study_id == "a000501" & final_files_exist == "no" ~ "yes",
    study_id == "a001121" & final_files_exist == "no" ~ "yes",
    study_id == "a000518" & final_files_exist == "no" ~ "yes",
    study_id == "a000688" & final_files_exist == "no" ~ "yes",
    study_id == "a000785" & final_files_exist == "no" ~ "yes",
    study_id == "a001247" & final_files_exist == "no" ~ "yes",
    study_id == "a001296" & final_files_exist == "no" ~ "yes",
    study_id == "a000948" & final_files_exist == "no" ~ "yes",
    study_id == "a000160" & final_files_exist == "no" ~ "yes",
    study_id == "a001490" & final_files_exist == "no" ~ "yes",
    study_id == "a000300" & final_files_exist == "no" ~ "yes",
    study_id == "a000965" & final_files_exist == "no" & link_url_cleaned == "osf.io/b6n8t" ~ "yes",
    
    TRUE ~ final_files_exist
  )) |> 
  mutate(review_download_success = case_when(
    study_id == "a000831" & final_art_has_links == "no" ~ "yes",
    study_id == "a001051" & final_art_has_links == "no" ~ "yes",
    study_id == "a000773" & final_art_has_links == "no" ~ "yes",
    
    study_id == "a000367" & final_link_works == "no" ~ "yes",
    study_id == "a000777" & final_link_works == "no" ~ "yes",
    study_id == "a000849" & final_link_works == "no" ~ "yes",
    study_id == "a000880" & final_link_works == "no" ~ "yes",
    study_id == "a001221" & final_link_works == "no" ~ "yes",
    study_id == "a001250" & final_link_works == "no" ~ "account",
    study_id == "a001293" & final_link_works == "no" ~ "yes",
    study_id == "a001429" & final_link_works == "no" ~ "yes",
    study_id == "a001476" & final_link_works == "no" ~ "account",
    study_id == "a001501" & final_link_works == "no" ~ "yes",
    
    study_id == "a001430" & final_files_exist == "no" ~ "yes",
    study_id == "a000330" & final_files_exist == "no" ~ "yes",
    study_id == "a000334" & final_files_exist == "no" ~ "yes",
    study_id == "a001215" & final_files_exist == "no" ~ "yes",
    study_id == "a000295" & final_files_exist == "no" ~ "yes",
    study_id == "a001285" & final_files_exist == "no" ~ "yes",
    study_id == "a000845" & final_files_exist == "no" ~ "yes",
    study_id == "a000501" & final_files_exist == "no" ~ "yes",
    study_id == "a001121" & final_files_exist == "no" ~ "yes",
    study_id == "a000518" & final_files_exist == "no" ~ "yes",
    study_id == "a000688" & final_files_exist == "no" ~ "yes",
    study_id == "a000785" & final_files_exist == "no" ~ "yes",
    study_id == "a001247" & final_files_exist == "no" ~ "yes",
    study_id == "a001296" & final_files_exist == "no" ~ "yes",
    study_id == "a000948" & final_files_exist == "no" ~ "yes",
    study_id == "a000160" & final_files_exist == "no" ~ "yes",
    study_id == "a001490" & final_files_exist == "no" ~ "yes",
    study_id == "a000300" & final_files_exist == "no" ~ "yes",
    study_id == "a000965" & final_files_exist == "no" & link_url_cleaned == "osf.io/b6n8t" ~ "yes",
    
    TRUE ~ final_download_success
  )) |> 
  mutate(review_download_success2 = case_when(
    study_id == "a000831" & final_art_has_links == "no" ~ "yes",
    study_id == "a001051" & final_art_has_links == "no" ~ "yes",
    study_id == "a000773" & final_art_has_links == "no" ~ "yes",
    
    study_id == "a000367" & final_link_works == "no" ~ "yes",
    study_id == "a000777" & final_link_works == "no" ~ "yes",
    study_id == "a000849" & final_link_works == "no" ~ "yes",
    study_id == "a000880" & final_link_works == "no" ~ "yes",
    study_id == "a001221" & final_link_works == "no" ~ "yes",
    study_id == "a001250" & final_link_works == "no" ~ "yes", # was account
    study_id == "a001293" & final_link_works == "no" ~ "yes",
    study_id == "a001429" & final_link_works == "no" ~ "yes",
    study_id == "a001476" & final_link_works == "no" ~ "yes", # was account
    study_id == "a001501" & final_link_works == "no" ~ "yes",
    
    study_id == "a001430" & final_files_exist == "no" ~ "yes",
    study_id == "a000330" & final_files_exist == "no" ~ "yes",
    study_id == "a000334" & final_files_exist == "no" ~ "yes",
    study_id == "a001215" & final_files_exist == "no" ~ "yes",
    study_id == "a000295" & final_files_exist == "no" ~ "yes",
    study_id == "a001285" & final_files_exist == "no" ~ "yes",
    study_id == "a000845" & final_files_exist == "no" ~ "yes",
    study_id == "a000501" & final_files_exist == "no" ~ "yes",
    study_id == "a001121" & final_files_exist == "no" ~ "yes",
    study_id == "a000518" & final_files_exist == "no" ~ "yes",
    study_id == "a000688" & final_files_exist == "no" ~ "yes",
    study_id == "a000785" & final_files_exist == "no" ~ "yes",
    study_id == "a001247" & final_files_exist == "no" ~ "yes",
    study_id == "a001296" & final_files_exist == "no" ~ "yes",
    study_id == "a000948" & final_files_exist == "no" ~ "yes",
    study_id == "a000160" & final_files_exist == "no" ~ "yes",
    study_id == "a001490" & final_files_exist == "no" ~ "yes",
    study_id == "a000300" & final_files_exist == "no" ~ "yes",
    study_id == "a000965" & final_files_exist == "no" & link_url_cleaned == "osf.io/b6n8t" ~ "yes",
    TRUE ~ final_download_success2
  ))

# temp_check7 <- data_coding_reviewed_all |> 
#   filter(is.na(review_art_has_links) == FALSE |
#            is.na(review_link_works) == FALSE |
#            is.na(review_link_url) == FALSE |
#            is.na(review_files_exist) == FALSE |
#            is.na(review_download_success) == FALSE)

table(data_coding_reviewed_all$review_art_has_links, useNA = "always")
table(data_coding_reviewed_all$review_link_works, useNA = "always")
table(data_coding_reviewed_all$review_files_exist, useNA = "always")
table(data_coding_reviewed_all$review_download_success, useNA = "always")
table(data_coding_reviewed_all$review_download_success2, useNA = "always")

# Calculate data which calculates a success rate for each individual study ID
# (to collapse multiple-link studies into a single record).
review_data_linkwork <- data_coding_reviewed_all |> 
  mutate(article_has_links_logical = case_when(
    review_art_has_links == "yes" ~ TRUE,
    review_art_has_links == "no" ~ FALSE,
    TRUE ~ FALSE
  )) |> 
  mutate(link_works_logical = case_when(
    review_link_works == "yes" ~ TRUE,
    review_link_works == "no" ~ FALSE,
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
  mutate(n_links = ifelse(link_category == "0", 0, n_links))

# Calculate the link-work success rates for all links consolidated by study ID.
review_linkwork_summary <- review_data_linkwork |> 
  select(study_id,
         article_has_links_logical,
         link_success,
         link_category) |> 
  distinct() |> 
  group_by(link_category) |> 
  summarise(n = n(),
            all_passed = round(100*mean(link_success == 1), 1),
            all_failed = round(100*mean(link_success == 0), 1),
            mixed = round(100*mean(link_success > 0 & link_success < 1), 1))

# Calculate data which calculates a success rate for each individual study ID
# (to collapse multiple-link studies into a single record).
review_data_hasfiles <- review_data_linkwork |> 
  mutate(files_exist_logical = case_when(
    review_files_exist == "yes" ~ TRUE,
    review_files_exist == "no" ~ FALSE,
    TRUE ~ FALSE
  )) |> 
  group_by(study_id) |> 
  mutate(files_exist_success = mean(files_exist_logical),
         n_links = n()) |> 
  ungroup()

# Calculate the files-exist success rates for all links consolidated by
# study ID.
review_files_exist_summary <- review_data_hasfiles |> 
  select(study_id,
         files_exist_success,
         link_category) |> 
  distinct() |> 
  group_by(link_category) |> 
  summarise(n = n(),
            all_passed = round(100*mean(files_exist_success == 1), 1),
            all_failed = round(100*mean(files_exist_success == 0), 1),
            mixed = round(100*mean(files_exist_success > 0 & files_exist_success < 1), 1))

# Use Definition 2 of download success, which recodes "account" and "partial"
# to "yes" - this is the optimistic interpretation
review_data_dlsuccess2 <- review_data_hasfiles |> 
  mutate(dlsuccess2_logical = case_when(
    review_download_success2 == "yes" ~ TRUE,
    review_download_success2 == "no" ~ FALSE,
    TRUE ~ FALSE
  )) |> 
  group_by(study_id) |> 
  mutate(dl_success2 = mean(dlsuccess2_logical),
         n_links = n()) |> 
  ungroup()

review_dlsuccess2_summary <- review_data_dlsuccess2 |> 
  select(study_id,
         dl_success2,
         link_category) |> 
  distinct() |> 
  group_by(link_category) |> 
  summarise(n = n(),
            all_passed = round(100*mean(dl_success2 == 1), 1),
            all_failed = round(100*mean(dl_success2 == 0), 1),
            mixed = round(100*mean(dl_success2 > 0 & dl_success2 < 1), 1))

# Get a single record per article
data_coding_reviewed_art <- review_data_dlsuccess2 |> 
  group_by(study_id) |> 
  slice_head() |> 
  ungroup() |> 
  mutate(effective_year = case_when(
    `Publication Title` == "Internet Archaeology" &
      effective_year < 2015 ~ 2015,
    TRUE ~ effective_year
  ))

# Get the response frequencies for each variable
table(data_coding_reviewed_art$final_data_avail_statement, useNA = "always")
table(data_coding_reviewed_art$final_art_has_links, useNA = "always")
table(data_coding_reviewed_art$link_success, useNA = "always")
table(data_coding_reviewed_art$files_exist_success, useNA = "always")
table(data_coding_reviewed_art$dl_success2, useNA = "always")

# Calculate the number of successful articles for each variable
n_review_1das <- data_coding_reviewed_art |> 
  filter(final_data_avail_statement == "yes") |> 
  nrow()

n_review_2ahl <- data_coding_reviewed_art |> 
  filter(review_art_has_links == "yes") |> 
  nrow()

n_review_3lwk <- data_coding_reviewed_art |> 
  filter(link_success == 1) |> 
  nrow()

n_review_4fex <- data_coding_reviewed_art |> 
  filter(files_exist_success == 1) |> 
  nrow()

n_review_5dls <- data_coding_reviewed_art |> 
  filter(dl_success2 == 1) |> 
  nrow()

# Compile all overall, article-level success counts into a data frame
review_success_overall <- tibble(
  success_category = c("Article has a data availability statement",
                       "Article has link(s)",
                       "Article has working link(s)",
                       "Article has working link(s) that lead to files",
                       "Article has working link(s) that lead to files which can be downloaded"),
  n_success = c(n_review_1das,
                n_review_2ahl,
                n_review_3lwk,
                n_review_4fex,
                n_review_5dls)) |> 
  mutate(pct_success = round(100*n_success/nrow(data_coding_articlelevel), 1))

# Export the overall, article-level success results to file
write_excel_csv(x = review_success_overall,
                file = here("results",
                            "databadge_postreview_summary_overall_articlelevel.csv"),
                eol = "\r\n",
                na = "",
                quote = "all")
