library(tidyverse)
library(lubridate)
library(janitor)
library(readxl)

#read in saved data file from previous step
prez_contribs_bundler_matches <- readRDS("output/prez_contribs_bundler_matches.rds")

#place fec contributor info to the left of dataframe
prez_contribs_bundler_matches <- prez_contribs_bundler_matches %>% 
  select(contributor_first_name,
        contributor_middle_name,
        contributor_prefix,
        contributor_suffix,
        contributor_street_1,
        contributor_street_2,
        contributor_city,
        contributor_state,
        contributor_zip,
        contributor_occupation,
        contributor_employer,
        everything())


#pull in bundler file and add columns
bundlers_raw <- read_excel("processed_data/OBAMATOPBUNDLERS_CLEAN_STRINGCODE.xlsx")

bundlers <- bundlers_raw %>% 
  clean_names() %>% 
  remove_empty(c("rows", "cols"))  %>% 
  mutate(
    name_last = str_trim(str_to_upper(name_last)),
    firstname_firstletter = str_sub(str_trim(name_first), 1, 1),
    matchstring = str_to_upper(str_trim(paste0(firstname_firstletter, name_last)))
  ) %>% 
  select(
    -full_name,
    -code1,
    -code2
  )

#rename columns to signify bundler origin (to aid with reading joined table)
#also uppercase them and remove any extraneous spaces
bundlers <- bundlers %>% 
  select(
    bundler_last = name_last,
    bundler_first = name_first,
    bundler_middle = middle_name,
    bundler_suffix = suffix_name,
    bundler_city = city,
    bundler_state = state,
    bundler_employer = employer,
    matchstring
  ) %>% 
  mutate(
    bundler_last = str_squish(str_to_upper(bundler_last)),
    bundler_first = str_squish(str_to_upper(bundler_first)),
    bundler_middle = str_squish(str_to_upper(bundler_middle)),
    bundler_suffix = str_squish(str_to_upper(bundler_suffix)),
    bundler_city = str_squish(str_to_upper(bundler_city)),
    bundler_state = str_squish(str_to_upper(bundler_state)),
    bundler_employer = str_squish(str_to_upper(bundler_employer))
  )


joined <- inner_join(bundlers, prez_contribs_bundler_matches, by = "matchstring")






#### FORMER ANALYSIS SCRIPTS ####
# 
# 
# #add candidate names
# targetlist_wcandname <- left_join(targetlist, candnames, by = c("filer_committee_id_number" = "fec_committee_id")) %>% 
#   select(cand_name = name, everything())  
# 
# #output
# write_csv(targetlist_wcandname, "output/targetlist_wcandname.csv")
# saveRDS(targetlist_wcandname, "processed_data/targetlist_wcandname.rds")
# 
# 
# ### FIND UNIQUE NAME/ADDRESS VARIATIONS ####
# 
# #load from rds file above to allow for local analysis 
# #without having to hit db server if desired
# targetlist_wcandname <- readRDS("processed_data/targetlist_wcandname.rds")
# 
# names(targetlist_wcandname)
# 
# # targetlist_uniques <- targetlist_wcandname %>% 
# #   select(cand_name,
# #          contributor_last_name,
# #          contributor_first_name,
# #          contributor_middle_name,
# #          contributor_suffix,
# #          contributor_street_1,
# #          contributor_street_2,
# #          contributor_city,
# #          contributor_state,
# #          zip5,
# #          contributor_employer,
# #          contributor_occupation
# #         ) %>% 
# #   unique()
# 
# #group by unique and contrib amounts
# targetlist_uniques <- targetlist_wcandname %>% 
#   group_by(cand_name,
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
#   ) %>% 
#   summarise(
#     sum_contribs_period = sum(contribution_amount),
#     aggregate_contribs_cycle = max(contribution_aggregate))
# 
# 
# # filter to just democrats
# targetlist_uniques %>% 
#   group_by(cand_name) %>% 
#   summarise(n())
# 
# 
# 
# #group by states
# targetlist_uniques %>% 
#   count(contributor_state) %>% 
#   arrange(desc(n))
# 
# #group by occupations
# targetlist_uniques %>% 
#   count(contributor_occupation) %>% 
#   arrange(desc(n))
