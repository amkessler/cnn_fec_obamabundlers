#first we'll run script step 00 to connect to db
source("00_connecttodb.R")

library(tidyverse)
library(lubridate)
library(janitor)
library(dbplyr)
library(readxl)

#list the tables in the database
src_dbi(con)

#pull in the schedule A table from postgres db
contribs_db <- tbl(con, "cycle_2020_schedulea")

glimpse(contribs_db)

#filter out only individual contributions, and active ones
#create zip5 field by pulling out just first five digits
contribs_db <- contribs_db %>%
  mutate(form_type = str_to_upper(form_type)) %>%
  filter(active==TRUE,
         form_type %in% c("SA17A", #individuals other than cmtes
                          "SA18", #transfers from other cmtes
                          "SB28A")) %>% #refunds to individuals
  mutate(
    zip5 = str_sub(str_trim(contributor_zip), 1, 5)
  )

# contribs_db <- contribs_db %>%
#   filter(active==TRUE)


#pull candidate table from postgres db
cand_db <- tbl(con, "cycle_2020_candidate")

#filter only for presidential and DEMOCRATS
candnames <- cand_db %>% 
  filter(district == "US",
         party == "D") %>% 
  select(name, 
         fec_committee_id) %>% 
  collect()


#grab prez committee ids
prez_cmte_ids <- candnames %>% pull(fec_committee_id)

#filter the contribs table by them
prez_contribs <- contribs_db %>% 
  filter(filer_committee_id_number %in% prez_cmte_ids)


### INDIVIDUAL NAME OR VARIABLE VALUE SEARCHES #### -------------------------

glimpse(prez_contribs)

#look for actress
occup_actress <- prez_contribs %>% 
  mutate(
    contributor_occupation = str_trim(str_to_upper(contributor_occupation))
  ) %>% 
  filter(
    str_detect(contributor_occupation, "ACTRESS")
  ) %>% 
  collect()

occup_actress <- occup_actress %>% 
  mutate(
    contribution_date = ymd(contribution_date)
  ) %>% 
  filter(
    contribution_date >= as_date("2019-04-01")
  )

## add candidate name to the mix
occup_actress_withcandname <- inner_join(occup_actress, candnames, by = c("filer_committee_id_number" = "fec_committee_id")) %>% 
  select(candidate_name = name, everything())

writexl::write_xlsx(occup_actress_withcandname, "output/actress.xlsx")
