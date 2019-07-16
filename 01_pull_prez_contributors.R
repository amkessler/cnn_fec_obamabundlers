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

#first letter of first name column for later use in filtering matches
#we'll also collect it locally to allow for map() function later to work
prez_contribs <- prez_contribs %>% 
  mutate(
    firstname_firstletter = str_sub(str_trim(contributor_first_name), 1, 1),
    matchstring = str_to_upper(str_trim(contributor_last_name)) #use matchstring for last name here
  ) %>% 
  collect()

#save the result as RDS
saveRDS(prez_contribs, "holding/prez_contribs.rds")

#check to make sure it worked
glimpse(prez_contribs)


## now let's filter for JUST Q2 CONTRIBUTIONS ####

#convert contrib date to date format
prez_contribs$contribution_date <- ymd(prez_contribs$contribution_date)
head(prez_contribs$contribution_date)
tail(prez_contribs$contribution_date)

#filter for Q2
prez_contribs <- prez_contribs %>% 
  filter(contribution_date > as_date("2019-04-01")) 






### PULLING OUT POSSIBLE OBAMA BUNDLERS ##### ---------------------------

bundlers_raw <- read_excel("processed_data/OBAMATOPBUNDLERS_CLEAN_STRINGCODE.xlsx")

bundlers <- bundlers_raw %>% 
  clean_names() %>% 
  remove_empty(c("rows", "cols"))  %>% 
  mutate(
    name_last = str_trim(str_to_upper(name_last)),
    firstname_firstletter = str_sub(str_trim(name_first), 1, 1),
    matchstring = name_last #renaming the last name field to matchstring here
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

# 
# #FUNCTION TO PULL MATCHES - OLD VERSION
# matchbundlers <- function(match_var) {
#   result <- prez_contribs %>% 
#     filter(
#       (matchstring == match_var)
#     ) 
#   
#   return(result)
# }


#use function with one match
zzz <- matchbundlers(test_match) 

#works!


### USE MAP() to have FUNCTION run on all the matches
prez_contribs_bundler_matches <- map_df(vector_matchstring, matchbundlers) 
#works! (req change above to collect the large prez_contribs table locally)


## add candidate name to the mix
head(candnames)

prez_contribs_bundler_matches

prez_contribs_bundler_matches <- inner_join(prez_contribs_bundler_matches, candnames, by = c("filer_committee_id_number" = "fec_committee_id")) %>% 
  select(candidate_name = name, everything())

#save results 
write_csv(prez_contribs_bundler_matches, "output/prez_contribs_bundler_matches.csv")
saveRDS(prez_contribs_bundler_matches, "output/prez_contribs_bundler_matches.rds")

