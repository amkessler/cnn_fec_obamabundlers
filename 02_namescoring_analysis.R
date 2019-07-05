library(tidyverse)
library(lubridate)
library(janitor)
library(readxl)


prez_contribs_bundler_matches <- readRDS("output/prez_contribs_bundler_matches.rds")






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
