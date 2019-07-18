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

#check to see match column variations
matches %>% 
  count(match_verdict)

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
  filter(status == "MEMO") 

#number of donations per candidate (not num of unique bundlers)  
joined %>% 
  count(candidate_name.y) %>% 
  arrange(desc(n)) 

joined %>% 
  count(bundler_last, bundler_first)

#by bundler
joined_bybundler <- joined %>% 
  group_by(bundler_last, bundler_first, candidate_name.y) %>% 
  summarise(n = n()) %>% 
  arrange(bundler_last, bundler_first, candidate_name.y, desc(n)) %>% 
  ungroup()

#by candidate
joined_bycandidate <- joined %>% 
  group_by(candidate_name.y, bundler_last, bundler_first) %>% 
  summarise(n = n()) %>% 
  arrange(candidate_name.y, bundler_last, bundler_first, desc(n)) %>% 
  ungroup()


#write results to files
write_xlsx(joined_bybundler, "output/joined_bybundler.xlsx")
write_xlsx(joined_bycandidate, "output/joined_bycandidate.xlsx")

temp <- joined_bycandidate %>% 
  mutate(
    bundlername = paste0(bundler_first, " ", bundler_last)
  ) %>% 
  select(candname = candidate_name.y, bundlername) 

temp

# temp %>% 
#   # filter(candname == "BENNET, MICHAEL F.") %>% 
#   rowid_to_column() %>% 
#   spread(candname, bundlername) %>% 
#   View()

#only giving to one cand?
faithful_bundlers <- temp %>% 
  count(bundlername) %>% 
  filter(n==1) %>% 
  pull(bundlername)

temp_faithfultots <- temp %>% 
  filter(bundlername %in% faithful_bundlers) %>% 
  count(candname) %>% 
  arrange(desc(n))

#num of unique bundlers per candidate
temp_uniquebundlers <- temp %>% 
  count(candname)

#join
temp_bothtogether <- left_join(temp_uniquebundlers, temp_faithfultots, by = "candname") %>% 
  arrange(desc(n.x))

write_xlsx(temp_bothtogether, "output/temp_bothtogether.xlsx")


#how many bundlers gave to more than one candidate
temp_bundlers_count_cands <- temp %>% 
  count(bundlername) %>% 
  arrange(desc(n))

write_xlsx(temp_bundlers_count_cands, "output/temp_bundlers_count_cands.xlsx")




#### Finding who Biden's donors gave to other than him??? ####
biden_bundlers_vector <- temp %>% 
  filter(candname == "BIDEN, JOSEPH R. JR.") %>% 
  pull(bundlername)
  
biden_shared_donors <- temp %>% 
  filter(bundlername %in% biden_bundlers_vector) %>% 
  arrange(bundlername)

biden_shared_donors %>% 
  write_xlsx("output/biden_shared_donors.xlsx")


#num of unique bundlers per candidate
temp_uniquebundlers <- temp %>% 
  count(candname)







#### bring in BIG ORIGINAL contribs table to check against #### 

#save the result as RDS
prez_contribs_orig <- readRDS("holding/prez_contribs.rds")

glimpse(prez_contribs_orig)

prez_contribs_orig <- prez_contribs_orig %>% 
  mutate(
    contributor_last_name = str_to_upper(contributor_last_name),
    contributor_first_name = str_to_upper(contributor_first_name),
    contribution_date = ymd(contribution_date)
  )

#filter for just Q2 only
prez_contribs_orig <- prez_contribs_orig %>% 
  filter(contribution_date >= as_date("2019-04-01"))

#add cand name - run top of step 01 first to grab candnames from postgres
prez_contribs_orig <- inner_join(prez_contribs_orig, candnames, by = c("filer_committee_id_number" = "fec_committee_id")) %>% 
  select(candidate_name = name, everything())



#pull out a donor names to check ####
prez_contribs_orig %>% 
  filter(
    contributor_last_name == "CORZINE",
    contributor_first_name == "JON"
  ) 


prez_contribs_orig %>% 
  filter(
    contributor_last_name == "AVANT",
    contributor_first_name == "NICOLE"
  ) 


prez_contribs_orig %>% 
  filter(
    contributor_last_name == "GALLOGLY",
    contributor_first_name == "MARK"
  ) %>% 
  arrange(candidate_name) 


prez_contribs_orig %>% 
  filter(
    contributor_last_name == "GOLDMAN",
    contributor_first_name == "LISA"
  ) %>% 
  arrange(candidate_name) 


prez_contribs_orig %>% 
  filter(
    contributor_last_name == "HALPERN",
    contributor_first_name == "DANIEL"
  ) %>% 
  arrange(candidate_name)


prez_contribs_orig %>% 
  filter(
    contributor_last_name == "KRAMER",
    contributor_first_name == "ORIN"
  ) %>% 
  arrange(candidate_name)


prez_contribs_orig %>% 
  filter(
    contributor_last_name == "PARKER",
    contributor_first_name == "YOLANDA"
  ) %>% 
  arrange(candidate_name)


prez_contribs_orig %>% 
  filter(
    contributor_last_name == "STETSON"
  ) %>% 
  arrange(candidate_name) %>% 
  write_xlsx("output/stetson_test.xlsx")

