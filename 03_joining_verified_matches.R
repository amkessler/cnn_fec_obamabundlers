library(tidyverse)
library(lubridate)
library(janitor)
library(readxl)
library(writexl)
library(googlesheets)
options(scipen = 999)

#### LOAD IN DATA TABLES #### ----------------

#read in saved data file from previous step
prez_contribs_forjoin <- readRDS("processed_data/prez_contribs_bundler_matches_forjoin.rds")

glimpse(prez_contribs_forjoin)

#trigger google authentication - should only have to do this once per computer
# gs_ls()

#register google sheet using stored sheet id key 
mykey <- Sys.getenv("BUNDLEMATCH_KEY")
bundlematches <- gs_key(mykey)

#read in all the data tab
matches <- bundlematches %>% 
  gs_read(ws = "allrecords") %>% 
  clean_names() 

#pull out just the Y/YES matches
matches_yes <- matches %>% 
  filter(match_verdict == "Y")

#save yes matches to file
saveRDS(matches_yes, "processed_data/matches_yes.rds")



### JOIN #### -------------------------------

#join verified matches with original fec contribution records

joined <- left_join(matches_yes, prez_contribs_forjoin, by = c("donorhash"))

joined %>% 
  count(status)

joined %>% 
  filter(status == "MEMO") %>% 
  View()
  
joined %>% 
  count(candidate_name.y) %>% 
  arrange(desc(n))

joined %>% 
  group_by(bundler_last, bundler_first, candidate_name.y) %>% 
  summarise(n = n()) %>% 
  arrange(bundler_last, bundler_first, candidate_name.y, desc(n)) %>% 
  View()


joined %>% 
  group_by(candidate_name.y, bundler_last, bundler_first) %>% 
  summarise(n = n()) %>% 
  arrange(candidate_name.y, bundler_last, bundler_first, desc(n)) %>% 
  View()

glimpse(joined)




### bring in BIG ORIGINAL contribs table to check against #### 

#save the result as RDS
prez_contribs_orig <- readRDS("holding/prez_contribs.rds")

glimpse(prez_contribs_orig)

prez_contribs_orig <- prez_contribs_orig %>% 
  mutate(
    contributor_last_name = str_to_upper(contributor_last_name),
    contributor_first_name = str_to_upper(contributor_first_name)
  )

#add cand name - run top of step 01 first to grab candnames from postgres
prez_contribs_orig <- inner_join(prez_contribs_orig, candnames, by = c("filer_committee_id_number" = "fec_committee_id")) %>% 
  select(candidate_name = name, everything())



prez_contribs_orig %>% 
  filter(
    contributor_last_name == "CORZINE",
    contributor_first_name == "JON"
  ) %>% 
  View()
