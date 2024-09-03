# this script gets data from another directory 

library(tidyverse)
library(magrittr)


# rename regulations.gov
namingthings <- function(d){
  names(d)  <- names(d) %>%
    str_replace_all("([A-Z])", "_\\1") %>%
    str_to_lower() %>%
    # rename old data for new API results
    str_replace("agency_acronym", "agency_id") %>%
    str_replace("document_id", "id")
  
  return(d)
}

# the old data
data_dir <- "dissertation"

path <- here::here("data",
                   "ej_data_clean.Rdata") |>
  str_replace("University of Michigan Dropbox/Devin Judge-Lord/ej", data_dir) 

load(path)

# new data 
# all docs 

data_dir <- "regulationsdotgov"

path <- here::here("data", 
                   "all_documents.rda") |>
  str_replace("ej", data_dir)

load(path)

exclude <- c('Advance Notice of Proposed Rulemaking (ANPRM)',
             'Other',
             'Request for Comment',
             #'Direct Final Rule',
             'Termination',
             #'Proposed Rule',
             'Withdrawal',
             'Public Hearing',
             'Extension of Comment Period',
             'Amendment',
             'Hearings',
             'Request for Information',
             #'Policy Statement',
             'Notice of Extension',
             #'Final Rule with Request for Comments',
             #'Final Order',
             'Advance Notice of Proposed Rulemaking',
             'Affirmation of an Interim Rule',
             'Reopening of Comment Period',
             'Notice of Interim Rule',
             'Confirmation of Effective Date',
             'Delay of Effective Date',
             'Public Meeting','Final',
             #'Interim Final',
             #'Interim Final w/ Comment Period',
             'Corrections to a Final Rule or Final Rule with Comment Period',
             'Federal Register Correction',
             'Prepublication Display',
             'Corrections to Proposed Rules',
             #'Final Rule with Comment Period',
             'Corrections to a Proposed Rule',
             #'Published Proposed Rule',
             #'Interim Final Rule with Comment Period',
             'Withdrawal of Proposed Rule',
             #'NOPR',
             'ANOPR',
             #'Federal Register Publication',
             #'Supplemental Notice of Proposed Rulemaking (SNPRM)',
             'Approval',
             #'Temporary Rule',
             #'Petition','Denial',
             #'SNOPR','Federal Register Document',
             'Foreign Airworthiness Directive',
             'Emergency Airworthiness Directive',
             #'Negotiated Rulemaking (NEG REG)',
             #'NFR-Notice of Final Rule','NPR-Notice of Proposed Rule-Making',
             'Commissioners Decision',
             'Notice of Confirmation of Effective Date',
             'Background Material','Notice of Hearing','Certificate Extending Patent Term',
             'CEP-Certificate Extending Patent Term','NCR-Notice of Correction',
             'NEC-Notice of Extension','General Notice','NAP-Advanced Notice of Proposed Rulemaking',
             'NCD-Notice of Confirmation of Effective Date','SET-Settlement Agreement',
             'IDF-Initial Decision by Admin Law Judge',
             'CMD-Commissioner.s Decision',
             #'Notice of Final Rule',
             'CFR-Correction',
             'ND-Notice of Denial',
             #'NFR-Notice of Final Order',
             'NWL-Notice of Withdrawl',
             'GDL Guidance',
             #'NIR-Notice of Interim Rule',
'N-Notice',
'GDL-Guidance',
'Notice of Availability',
'Technical Amendment',
#'Federal Register','Proposed rule',
#''Extension of comment period',
#'Advance notice of proposed rulemaking',
#'Interim','Proposed Rules',
'Advanced Notice',
'Settlement Agreement',
'Petition for Rulemaking',
'Withdrawal of Rule',
'Denial of PRM',
'Resolution of PRM',
#'Draft Policy Statement',
'ANPR Withdrawal',
'Withdrawal of PRM')

documents %<>%
  namingthings() %>%
  filter(document_type %in% c("Rule", "Proposed Rule"),
         !subtype %in% exclude) %>%
  mutate(year = str_sub(posted_date, 1, 4) ) %>% 
  group_by(docket_id, document_type) %>% 
  slice_max(n = 1, order_by = posted_date, with_ties = F) %>% 
  rename(agency = agency_id) %>% #, number_of_comments_received = comments) %>% 
  select(agency, document_type, fr_doc_num, 
         title, 
         posted_date, 
         docket_id, subtype,
         year) 

# inspect duplicates 
documents %>% add_count(docket_id, document_type, subtype) |> 
  filter(n > 1) |> arrange(docket_id) |>
  view()

rules %<>% 
  full_join(documents) %>% 
  distinct() %>% 
  group_by(docket_id, document_type) %>% 
  slice_max(n = 1, year, with_ties = FALSE)


# ej docs
data_dir <- "regulationsdotgov"

path <- here::here("data", "metadata", 
                   "documents", 
                   "environmental justice.rda") |>
  str_replace("ej", data_dir)

load(path)

#FIXME should be called documents, not d
ej_documents <- d %>%
  namingthings() %>%
  filter(document_type %in% c("Rule", "Proposed Rule"),
         !subtype %in% exclude) %>%
  mutate(posted_date = as.Date(posted_date),
         year = str_sub(posted_date, 1, 4) ) %>% 
  group_by(docket_id, document_type) %>% 
  slice_max(n = 1, order_by = posted_date, with_ties = F) %>% 
  mutate(agency = agency_id) %>% 
  select(agency, agency_id, document_type, fr_doc_num, 
         title, 
         posted_date, 
         docket_id, 
         subtype,
         year)%>% 
  distinct()


# comments 
path <- here::here("data", "metadata", 
                   "comments", 
                   "environmental justice.rda") |>
  str_replace("ej", data_dir)

load(path)

# fix new data before merging 
ej_comments <- d %>% 
  namingthings() %>% 
  mutate(docket_id = str_remove(id, "-[0-9]*$"),
         posted_date = as.Date(posted_date),
         year = str_sub(posted_date, 1, 4),
         agency = agency_id) %>% 
  select(-last_modified_date, -lastpage, -search_term, -withdrawn, -document_type) 



# merge 
ejcomments %<>% full_join(ej_comments )

# fix old data 
ej_documents %<>% 
  group_by(docket_id) %>%
  mutate(ej_comments_on_docket = docket_id %in% ejcomments$docket_id) %>% 
  ungroup() 

count(ej_documents, ej_comments_on_docket)

# agencies that mention ej for data collection 
ej_documents$agency |> unique() |> paste(collapse = "', '")


ej_FR <- ej_documents |> filter(document_type == "Rule")

ejFR %<>% full_join(ej_FR)

ej_PR <-  ej_documents |> filter(document_type == "Proposed Rule")

ejPR %<>% full_join(ej_PR)

ej_FRejPR <- ejFR |> filter(docket_id %in% ejPR$docket_id)

ejFRejPR %<>% full_join(ej_FRejPR)

ej_FR_PR <- ejFR |> filter(!docket_id %in% ejPR$docket_id)

ejFR_PR %<>% full_join(ej_FR_PR)

# FIXME the new api is returning mostly blanks for highlighted content 
# ejFRejPRchanged <- ejFRejPR %>% filter(change)

save(rules,
     ejFR_PR, ejFRejPR, ejFR, ejPR, ej_comments, 
     file = here::here("data", "ej_data_raw.Rdata"))


