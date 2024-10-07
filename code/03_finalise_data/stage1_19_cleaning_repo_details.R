# stage1_19_cleaning_repo_details.R --------------------------------------------
#
# This script takes the stage 1 coding data after the step 6 finalisation, and
# then adds clean repository variables to the data set.



# Load required packages -------------------------------------------------------
library(tidyverse)
library(here)



# Import the step 6 finalised coding -------------------------------------------
data_coding_step6 <- read_rds(here("data", "output", "stage1",
                                   "all_final_coding_step6.rds"))



# Import the latest version of the OSB library ---------------------------------
osb_lib_raw <- read_rds(here("data", "output",
                             "osb_lib_with_study_id_latest.rds"))

# Check the numbers of articles by journal for data badge articles published
# 2020 or later.
check1 <- osb_lib_raw |> 
  filter(OpenDataBadge == TRUE) |> 
  filter(`Publication Year` >= 2020)

check2 <- check1 |> 
  group_by(`Publication Title`) |> 
  count() |> 
  arrange(desc(n))



# Create repository variables --------------------------------------------------
repo_cleaned <- data_coding_step6 |> 
  mutate(repository_name = case_when(
    str_sub(resolved_url_cleaned, 1, 6) == "osf.io" ~ "Open Science Framework",
    str_sub(resolved_url_cleaned, 1, 15) == "accounts.osf.io" ~ "Open Science Framework",
    str_sub(resolved_url_cleaned, 1, 21) == "dataverse.harvard.edu" ~ "Harvard Dataverse",
    str_sub(resolved_url_cleaned, 1, 13) == "datadryad.org" ~ "Dryad",
    str_sub(resolved_url_cleaned, 1, 10) == "zenodo.org" ~ "Zenodo",
    str_sub(resolved_url_cleaned, 1, 17) == "data.mendeley.com" ~ "Mendeley",
    str_sub(resolved_url_cleaned, 1, 12) == "figshare.com" ~ "Figshare",
    str_sub(resolved_url_cleaned, 1, 19) == "www.icpsr.umich.edu" ~ "ICPSR",
    str_sub(resolved_url_cleaned, 1, 17) == "www.openicpsr.org" ~ "openICPSR",
    str_sub(resolved_url_cleaned, 1, 21) == "www.iris-database.org" ~ "IRIS",
    str_sub(resolved_url_cleaned, 1, 17) == "iris-database.org" ~ "IRIS",
    str_sub(resolved_url_cleaned, 1, 10) == "github.com" ~ "Github",
    str_sub(resolved_url_cleaned, 1, 18) == "crumplab.github.io" ~ "Github",
    str_sub(resolved_url_cleaned, 1, 27) == "reshare.ukdataservice.ac.uk" ~ "ReShare (UK Data Service)",
    str_sub(resolved_url_cleaned, 1, 24) == "beta.ukdataservice.ac.uk" ~ "UK Data Service",
    str_sub(resolved_url_cleaned, 1, 20) == "catalogue.ceda.ac.uk" ~ "CEDA Archive",
    str_sub(resolved_url_cleaned, 1, 19) == "catalogue.ceh.ac.uk" ~ "Environmental Information Data Centre (UKCEH)",
    str_sub(resolved_url_cleaned, 1, 20) == "childes.talkbank.org" ~ "CHILDES/TalkBank",
    str_sub(resolved_url_cleaned, 1, 14) == "doi.pangaea.de" ~ "PANGAEA",
    #str_sub(resolved_url_cleaned, 1, 19) == "midus.colectica.org" ~ "Colectica.org",
    str_sub(resolved_url_cleaned, 1, 24) == "www.repository.cam.ac.uk" ~ "Apollo (University of Cambridge)",
    str_sub(resolved_url_cleaned, 1, 21) == "www.psycharchives.org" ~ "PsychArchives",
    str_sub(resolved_url_cleaned, 1, 16) == "www.psychdata.de" ~ "PsychData",
    str_sub(resolved_url_cleaned, 1, 19) == "snap.ogs.trieste.it" ~ "Seismic data Network Access Point (SNAP)",
    str_sub(resolved_url_cleaned, 1, 14) == "open.canada.ca" ~ "Open Government Portal (Government of Canada)",
    str_sub(resolved_url_cleaned, 1, 28) == "archaeologydataservice.ac.uk" ~ "Archaeology Data Service",
    
    str_sub(resolved_url_cleaned, 1, 11) == "nces.ed.gov" ~ "National Center for Education Statistics",
    str_sub(resolved_url_cleaned, 1, 12) == "dataverse.nl" ~ "DataverseNL",
    str_sub(resolved_url_cleaned, 1, 13) == "openneuro.org" ~ "OpenNeuro",
    str_sub(resolved_url_cleaned, 1, 14) == "ddbj.nig.ac.jp" ~ "DNA Data Bank of Japan",
    #str_sub(resolved_url_cleaned, 1, 14) == "ckan-rdm.up.pt" ~ "University of Porto",
    str_sub(resolved_url_cleaned, 1, 14) == "neurovault.org" ~ "NeuroVault",
    str_sub(resolved_url_cleaned, 1, 14) == "www.seanoe.org" ~ "SEANOE (SEA scieNtific Open data Edition)",
    str_sub(resolved_url_cleaned, 1, 15) == "opencontext.org" ~ "Open Context",
    str_sub(resolved_url_cleaned, 1, 15) == "www.usap-dc.org" ~ "U.S. Antarctic Program Data Center",
    str_sub(resolved_url_cleaned, 1, 16) == "forsbase.unil.ch" ~ "FORSbase",
    str_sub(resolved_url_cleaned, 1, 16) == "ore.exeter.ac.uk" ~ "Open Research Exeter (University of Exeter)",
    str_sub(resolved_url_cleaned, 1, 16) == "search.gesis.org" ~ "GESIS (Leibniz Institute for the Social Sciences)",
    #str_sub(resolved_url_cleaned, 1, 16) == "sro.sussex.ac.uk" ~ "Sussex Research Online (University of Sussex)",
    #str_sub(resolved_url_cleaned, 1, 16) == "www.astro.oma.be" ~ "Royal Observatory of Belgium",
    str_sub(resolved_url_cleaned, 1, 17) == "dataverse.unc.edu" ~ "UNC Dataverse",
    str_sub(resolved_url_cleaned, 1, 17) == "hrs.isr.umich.edu" ~ "Health and Retirement Study Data Portal (University of Michigan)",
    str_sub(resolved_url_cleaned, 1, 17) == "merritt.cdlib.org" ~ "Merritt (University of California)",
    str_sub(resolved_url_cleaned, 1, 17) == "mistrals.sedoo.fr" ~ "SEDOO (Observatoire Midi-Pyrénées)",
    str_sub(resolved_url_cleaned, 1, 17) == "nyu.databrary.org" ~ "Databrary",
    str_sub(resolved_url_cleaned, 1, 17) == "www.ebi.ac.uk/ena" ~ "European Nucleotide Archive",
    str_sub(resolved_url_cleaned, 1, 18) == "edmond.mpdl.mpg.de" ~ "Edmond (Max Planck Society)",
    str_sub(resolved_url_cleaned, 1, 18) == "data.nodc.noaa.gov" ~ "National Oceanographic Data Center",
    str_sub(resolved_url_cleaned, 1, 18) == "data.donders.ru.nl" ~ "Radboud Data Repository (Radboud University)",
    #str_sub(resolved_url_cleaned, 1, 18) == "sc.lib.miamioh.edu" ~ "Scholarly Commons (Miami University)",
    str_sub(resolved_url_cleaned, 1, 19) == "www.vliz.be/en/imis" ~ "Integrated Marine Information System (Flanders Marine Institute)",
    #str_sub(resolved_url_cleaned, 1, 21) == "nova.newcastle.edu.au" ~ "Nova (University of Newcastle)",
    #str_sub(resolved_url_cleaned, 1, 21) == "www.oa.uni-hamburg.de" ~ "Open Access Portal (Universität Hamburg)",
    #str_sub(resolved_url_cleaned, 1, 22) == "epub.uni-regensburg.de" ~ "University of Regensburg Publication Server",
    str_sub(resolved_url_cleaned, 1, 22) == "db.humanconnectome.org" ~ "ConnectomeDB, CCF",
    str_sub(resolved_url_cleaned, 1, 22) == "scholarbank.nus.edu.sg" ~ "ScholarBank@NUS (National University of Singapore)",
    str_sub(resolved_url_cleaned, 1, 22) == "www.fdr.uni-hamburg.de" ~ "Research Data Repository (Universität Hamburg)",
    str_sub(resolved_url_cleaned, 1, 23) == "hrsonline.isr.umich.edu" ~ "Health and Retirement Study Data Portal (University of Michigan)",
    str_sub(resolved_url_cleaned, 1, 23) == "www.datacommons.psu.edu" ~ "Data Commons (The Pennsylvania State University)",
    str_sub(resolved_url_cleaned, 1, 24) == "espace.library.uq.edu.au" ~ "eSpace (The University of Queensland)",
    str_sub(resolved_url_cleaned, 1, 26) == "madata.bib.uni-mannheim.de" ~ "MADATA (University of Mannheim)",
    str_sub(resolved_url_cleaned, 1, 28) == "fcon_1000.projects.nitrc.org" ~ "NeuroImaging Tools & Resources Collaboratory",
    str_sub(resolved_url_cleaned, 1, 29) == "dataverse.scholarsportal.info" ~ "Scholars Portal Dataverse",
    str_sub(resolved_url_cleaned, 1, 29) == "arch.library.northwestern.edu" ~ "Arch (Northwestern University)",
    str_sub(resolved_url_cleaned, 1, 31) == "www.ncbi.nlm.nih.gov/bioproject" ~ "BioProject (National Center for Biotechnology Information)",
    #str_sub(resolved_url_cleaned, 1, 35) == "www.oecd.org/pisa/data/2015database" ~ "OECD PISA Database",
    is.na(resolved_url_cleaned) == TRUE & str_sub(link_url_cleaned, 1, 21) == "dataverse.harvard.edu" ~ "Harvard Dataverse",
    str_sub(resolved_url_cleaned, 1, 14) == "www.pairfam.de" ~ "Pairfam",
    str_sub(resolved_url_cleaned, 1, 25) == "www.worldvaluessurvey.org" ~ "World Values Survey",
    
    # str_sub(resolved_url_cleaned, 1, 1) == "" ~ "",
    # str_sub(resolved_url_cleaned, 1, 1) == "" ~ "",
    is.na(resolved_url_cleaned) == TRUE ~ NA_character_,
    TRUE ~ NA_character_
  )) |> 
  mutate(is_repository = case_when(
    is.na(repository_name) == FALSE ~ "yes",
    is.na(resolved_url_cleaned) == FALSE ~ "no",
    is.na(resolved_url_cleaned) == TRUE ~ NA_character_,
    TRUE ~ NA_character_
  ))
  

table(repo_cleaned$repository_name, useNA = "always")
table(repo_cleaned$is_repository, useNA = "always")

check_unprocessed <- repo_cleaned |> 
  filter(is.na(repository_name)) |> 
  arrange(resolved_url_cleaned) |> 
  relocate(resolved_url_cleaned, .after = study_id)

repo_data_final <- repo_cleaned |> 
  select(study_id, link_url_cleaned, resolved_url_cleaned,
         is_repository, repository_name)



# Export the repository details ------------------------------------------------
write_rds(repo_data_final,
          here("data", "output", "stage1",
               "all_final_repository_coding.rds"))

write_excel_csv(repo_data_final,
                here("data", "output", "stage1",
                     "all_final_repository_coding.csv"),
                eol = "\r\n",
                quote = "all",
                na = "")
