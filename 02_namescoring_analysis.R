library(tidyverse)
library(lubridate)
library(janitor)
library(readxl)

#read in saved data file from previous step
prez_contribs_bundler_matches <- readRDS("output/prez_contribs_bundler_matches.rds")

#limit to 500 aggregate or above
prez_contribs_bundler_matches <- prez_contribs_bundler_matches %>% 
  filter(contribution_aggregate >= 500)

#place fec contributor info to the left of dataframe
prez_contribs_bundler_matches <- prez_contribs_bundler_matches %>% 
  select(candidate_name,
         contributor_last_name,
        contributor_first_name,
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
        everything()) %>% 
  mutate(
    candidate_name = str_to_upper(str_squish(candidate_name)),
    contributor_last_name = str_to_upper(str_squish(contributor_last_name)),
    contributor_first_name = str_to_upper(str_squish(contributor_first_name)),
    contributor_middle_name = str_to_upper(str_squish(contributor_middle_name)),
    contributor_prefix = str_to_upper(str_squish(contributor_prefix)),
    contributor_suffix = str_to_upper(str_squish(contributor_suffix)),
    contributor_street_1 = str_to_upper(str_squish(contributor_street_1)),
    contributor_street_2 = str_to_upper(str_squish(contributor_street_2)),
    contributor_city = str_to_upper(str_squish(contributor_city)),
    contributor_state = str_to_upper(str_squish(contributor_state)),
    contributor_zip = str_to_upper(str_squish(contributor_zip)),
    contributor_occupation = str_to_upper(str_squish(contributor_occupation)),
    contributor_employer = str_to_upper(str_squish(contributor_employer))
  )


#add a unique id to each of the records
prez_contribs_bundler_matches <- prez_contribs_bundler_matches %>% 
  tibble::rowid_to_column("index_id")

# create a table for joining that includes only unique name/add combinations
unique_potential_matches <- prez_contribs_bundler_matches %>% 
  select(index_id,
         candidate_name,
         contributor_last_name,
         contributor_first_name,
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
         matchstring) %>% 
  distinct(candidate_name,
           contributor_last_name,
           contributor_first_name,
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
           .keep_all = TRUE)


#pull in bundler file and add columns
bundlers_raw <- read_excel("processed_data/OBAMATOPBUNDLERS_CLEAN_STRINGCODE.xlsx")

bundlers <- bundlers_raw %>% 
  clean_names() %>% 
  remove_empty(c("rows", "cols"))  %>% 
  mutate(
    name_last = str_trim(str_to_upper(name_last)),
    firstname_firstletter = str_sub(str_trim(name_first), 1, 1),
    matchstring = str_to_upper(str_trim(paste0(firstname_firstletter, name_last)))
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


joined <- inner_join(bundlers, unique_potential_matches, by = "matchstring")


# ASSIGNING SCORES TO POSSIBLE MATCHES ####
names(joined)

final_for_research <- joined %>% 
  mutate(
    match_type = case_when(
         bundler_last == contributor_last_name & 
           bundler_first == contributor_first_name &
           bundler_city == contributor_city &
           bundler_state == contributor_state ~ "1 - FirstLastCityState",
         bundler_last == contributor_last_name & 
           bundler_first == contributor_first_name &
           bundler_city != contributor_city &
           bundler_state == contributor_state ~ "2 - FirstLastState",
         bundler_last == contributor_last_name & 
           bundler_first == contributor_first_name &
          bundler_city != contributor_city &
           bundler_state != contributor_state ~ "3 - FirstLast"
         )
    ) %>% 
  select(match_type, everything()) 


final_for_research %>% 
  count(match_type)


#write results to file
write_csv(final_for_research, "output/final_for_research_500andabove.csv", na = "")