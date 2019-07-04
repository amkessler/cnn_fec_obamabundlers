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

## NOTE: CAN REPLACE ABOVE WITH THE MATERIALIZED VIEW IN POSTGRES


#pull candidate table from postgres db
cand_db <- tbl(con, "cycle_2020_candidate")

#filter only for presidential
candnames <- cand_db %>% 
  filter(district == "US") %>% 
  select(name, fec_committee_id) %>% 
  collect()

#grab prez committee ids
prez_cmte_ids <- candnames %>% pull(fec_committee_id)

#filter the contribs table by them
prez_contribs <- contribs_db %>% 
  filter(filer_committee_id_number %in% prez_cmte_ids)

#first letter of first name column for matching
#we'll also collect it locally to allow for map() function later to work
prez_contribs <- prez_contribs %>% 
  mutate(
    firstname_firstletter = str_sub(str_trim(contributor_first_name), 1, 1),
    matchstring = str_to_upper(str_trim(paste0(firstname_firstletter, contributor_last_name)))
  ) %>% 
  collect()

#save the result as RDS
saveRDS(prez_contribs, "holding/prez_contribs.rds")

#check to make sure it worked
glimpse(prez_contribs)



### PULLING OUT POSSIBLE OBAMA BUNDLERS ##### ---------------------------

bundlers_raw <- read_excel("processed_data/OBAMATOPBUNDLERS_CLEAN_STRINGCODE.xlsx")

bundlers <- bundlers_raw %>% 
  clean_names() %>% 
  remove_empty(c("rows", "cols"))  %>% 
  mutate(
    name_last = str_trim(str_to_upper(name_last)),
    firstname_firstletter = str_sub(str_trim(name_first), 1, 1),
    matchstring = str_to_upper(str_trim(paste0(firstname_firstletter, name_last)))
  )

#create a vector from matchstring field to feed to function
vector_matchstring <- bundlers %>% 
  pull(matchstring)

test_match <- vector_matchstring[1]


#FUNCTION TO PULL MATCHES ####
matchbundlers <- function(match_var) {
  result <- prez_contribs %>% 
    filter(
      (matchstring == match_var)
    ) 
  
  return(result)
}


#use function with one match
zzz <- matchbundlers(test_match) 

#works!

### USE MAP() to have FUNCTION run on all the matches
prez_contribs_bundler_matches <- map_df(vector_matchstring, matchbundlers) 
#works! (req change above to collect the large prez_contribs table locally)

#save results 
write_csv(prez_contribs_bundler_matches, "output/prez_contribs_bundler_matches.csv")
saveRDS(prez_contribs_bundler_matches, "output/prez_contribs_bundler_matches.rds")






####


#add candidate names
targetlist_wcandname <- left_join(targetlist, candnames, by = c("filer_committee_id_number" = "fec_committee_id")) %>% 
  select(cand_name = name, everything())  

#output
write_csv(targetlist_wcandname, "output/targetlist_wcandname.csv")
saveRDS(targetlist_wcandname, "processed_data/targetlist_wcandname.rds")


### FIND UNIQUE NAME/ADDRESS VARIATIONS ####

#load from rds file above to allow for local analysis 
#without having to hit db server if desired
targetlist_wcandname <- readRDS("processed_data/targetlist_wcandname.rds")

names(targetlist_wcandname)

# targetlist_uniques <- targetlist_wcandname %>% 
#   select(cand_name,
#          contributor_last_name,
#          contributor_first_name,
#          contributor_middle_name,
#          contributor_suffix,
#          contributor_street_1,
#          contributor_street_2,
#          contributor_city,
#          contributor_state,
#          zip5,
#          contributor_employer,
#          contributor_occupation
#         ) %>% 
#   unique()

#group by unique and contrib amounts
targetlist_uniques <- targetlist_wcandname %>% 
  group_by(cand_name,
         contributor_last_name,
         contributor_first_name,
         contributor_middle_name,
         contributor_suffix,
         contributor_street_1,
         contributor_street_2,
         contributor_city,
         contributor_state,
         zip5,
         contributor_employer,
         contributor_occupation
  ) %>% 
  summarise(
    sum_contribs_period = sum(contribution_amount),
    aggregate_contribs_cycle = max(contribution_aggregate))


# filter to just democrats
targetlist_uniques %>% 
  group_by(cand_name) %>% 
  summarise(n())



#group by states
targetlist_uniques %>% 
  count(contributor_state) %>% 
  arrange(desc(n))

#group by occupations
targetlist_uniques %>% 
  count(contributor_occupation) %>% 
  arrange(desc(n))
