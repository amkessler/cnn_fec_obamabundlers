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
prez_contribs <- prez_contribs %>% 
  mutate(
    firstname_firstletter = str_sub(str_trim(contributor_first_name), 1, 1),
    matchstring = str_trim(paste0(firstname_firstletter, contributor_last_name))
  )

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
    matchstring = str_trim(paste0(firstname_firstletter, name_last))
  )

vector_matchstring <- bundlers %>% 
  pull(matchstring)

test_match <- vector_matchstring[1]

#WIP on functions
matchbundlers <- function(match_var) {
  result <- prez_contribs %>% 
    filter(
      (matchstring == match_var)
    ) 
  
  return(result)
}


#test out the function with one match
zzz <- matchbundlers(test_match) %>% 
  collect()


### ATTEMPT TO USE MAP() FUNCTION
#run all the matches?
zzz_mapped <- map_df(vector_matchstring, matchbundlers) %>% 
  collect()



# OLD MAKESHIFT QUICK-TURN CODE
#name matches and download to local dataframe
# targetlist <- prez_contribs %>% 
#   filter(
#     (str_detect(contributor_last_name, "ANDERSON") && firstname_firstletter == "R") |
#       (str_detect(contributor_last_name, "MINER") && firstname_firstletter == "N") |
#       (str_detect(contributor_last_name, "BREWSTER") && firstname_firstletter == "J") |
#       (str_detect(contributor_last_name, "BREWSTER") && firstname_firstletter == "W") |
#       (str_detect(contributor_last_name, "SATAWAKE") && firstname_firstletter == "B") |
#       (str_detect(contributor_last_name, "ROGERS") && firstname_firstletter == "J") |
#       (str_detect(contributor_last_name, "HOBSON") && firstname_firstletter == "M") |
#       (str_detect(contributor_last_name, "SMITH") && firstname_firstletter == "W") |
#       (str_detect(contributor_last_name, "PINKETT-SMITH") && firstname_firstletter == "J") |
#       (str_detect(contributor_last_name, "SCHAPIRO") && firstname_firstletter == "A") |
#       (str_detect(contributor_last_name, "NEWBERGER") && firstname_firstletter == "T") |
#       (str_detect(contributor_last_name, "CHESLER") && firstname_firstletter == "E") |
#       (str_detect(contributor_last_name, "MALLOW") && firstname_firstletter == "M") |
#       (str_detect(contributor_last_name, "ACKER") && firstname_firstletter == "G") |
#       (str_detect(contributor_last_name, "GOODMAN") && firstname_firstletter == "B") |
#       (str_detect(contributor_last_name, "SMITH") && firstname_firstletter == "M") |
#       (str_detect(contributor_last_name, "COSTOS") && firstname_firstletter == "J") |
#       (str_detect(contributor_last_name, "AVANT") && firstname_firstletter == "N") |
#       (str_detect(contributor_last_name, "SARANDOS") && firstname_firstletter == "T") |
#       (str_detect(contributor_last_name, "PERLMAN") && firstname_firstletter == "D") |
#       (str_detect(contributor_last_name, "KARAS") && firstname_firstletter == "B") |
#       (str_detect(contributor_last_name, "MILLS") && firstname_firstletter == "S") |
#       (str_detect(contributor_last_name, "HEINS") && firstname_firstletter == "S") |
#       (str_detect(contributor_last_name, "GILL") && firstname_firstletter == "T") |
#       (str_detect(contributor_last_name, "MILLER") && firstname_firstletter == "S") |
#       (str_detect(contributor_last_name, "KIREKER") && firstname_firstletter == "M") |
#       (str_detect(contributor_last_name, "KIREKER") && firstname_firstletter == "C") |
#       (str_detect(contributor_last_name, "KRUPP") && firstname_firstletter == "L") |
#       (str_detect(contributor_last_name, "KRUPP") && firstname_firstletter == "G") |
#       (str_detect(contributor_last_name, "STAFFORD") && firstname_firstletter == "E") |
#       (str_detect(contributor_last_name, "STAFFORD") && firstname_firstletter == "A") |
#       (str_detect(contributor_last_name, "CONNORS") && firstname_firstletter == "E") |
#       (str_detect(contributor_last_name, "CONNORS") && firstname_firstletter == "J") |
#       (str_detect(contributor_last_name, "RANDLETT") && firstname_firstletter == "L") |
#       (str_detect(contributor_last_name, "RANDLETT") && firstname_firstletter == "W") |
#       (str_detect(contributor_last_name, "PARKER") && firstname_firstletter == "Y") |
#       (str_detect(contributor_last_name, "PARKER") && firstname_firstletter == "C") |
#       (str_detect(contributor_last_name, "COHEN") && firstname_firstletter == "D") |
#       (str_detect(contributor_last_name, "COHEN") && firstname_firstletter == "R") |
#       (str_detect(contributor_last_name, "FIDLER") && firstname_firstletter == "J") |
#       (str_detect(contributor_last_name, "FIDLER") && firstname_firstletter == "G") |
#       (str_detect(contributor_last_name, "HEYMAN") && firstname_firstletter == "V") |
#       (str_detect(contributor_last_name, "HEYMAN") && firstname_firstletter == "B") |
#       (str_detect(contributor_last_name, "BERNSTEIN") && firstname_firstletter == "A") |
#       (str_detect(contributor_last_name, "BERNSTEIN") && firstname_firstletter == "T") |
#       (str_detect(contributor_last_name, "MCFADDEN-LAWSON") && firstname_firstletter == "M") |
#       (str_detect(contributor_last_name, "FRILLMAN") && firstname_firstletter == "C") |
#       (str_detect(contributor_last_name, "FRILLMAN") && firstname_firstletter == "L") |
#       (str_detect(contributor_last_name, "KRUPP") && firstname_firstletter == "J") |
#       (str_detect(contributor_last_name, "KRUPP") && firstname_firstletter == "D") |
#       (str_detect(contributor_last_name, "MEYER") && firstname_firstletter == "B") |
#       (str_detect(contributor_last_name, "MEYER") && firstname_firstletter == "W") |
#       (str_detect(contributor_last_name, "GOLDMAN") && firstname_firstletter == "L") |
#       (str_detect(contributor_last_name, "GOLDMAN") && firstname_firstletter == "D") |
#       (str_detect(contributor_last_name, "JACOBS") && firstname_firstletter == "I") |
#       (str_detect(contributor_last_name, "JACOBS") && firstname_firstletter == "J") |
#       (str_detect(contributor_last_name, "RAZDAN DUGGAL") && firstname_firstletter == "S") |
#       (str_detect(contributor_last_name, "O'MALLEY") && firstname_firstletter == "M") |
#       (str_detect(contributor_last_name, "HARPOOTLIAN") && firstname_firstletter == "R") |
#       (str_detect(contributor_last_name, "BONE") && firstname_firstletter == "C") |
#       (str_detect(contributor_last_name, "LANG SOLLINGER") && firstname_firstletter == "M") |
#       (str_detect(contributor_last_name, "KATZENBERG") && firstname_firstletter == "J") |
#       (str_detect(contributor_last_name, "MCLARTY") && firstname_firstletter == "M") |
#       (str_detect(contributor_last_name, "KIANI") && firstname_firstletter == "M") |
#       (str_detect(contributor_last_name, "LEVI") && firstname_firstletter == "J") |
#       (str_detect(contributor_last_name, "LEVI") && firstname_firstletter == "J") |
#       (str_detect(contributor_last_name, "FORESTER") && firstname_firstletter == "C") |
#       (str_detect(contributor_last_name, "MCAULIFFE") && firstname_firstletter == "T") |
#       (str_detect(contributor_last_name, "STANTON") && firstname_firstletter == "A") |
#       (str_detect(contributor_last_name, "SLINGERLAND") && firstname_firstletter == "D") |
#       (str_detect(contributor_last_name, "GRINSTEIN") && firstname_firstletter == "S") |
#       (str_detect(contributor_last_name, "NIX-HINES") && firstname_firstletter == "C") |
#       (str_detect(contributor_last_name, "BAGLEY") && firstname_firstletter == "E") |
#       (str_detect(contributor_last_name, "WEINSTEIN") && firstname_firstletter == "H") |
#       (str_detect(contributor_last_name, "MEYERHOFF") && firstname_firstletter == "R") |
#       (str_detect(contributor_last_name, "PISHEVAR") && firstname_firstletter == "S") |
#       (str_detect(contributor_last_name, "RUSSELL") && firstname_firstletter == "M") |
#       (str_detect(contributor_last_name, "SEPULVEDA") && firstname_firstletter == "E") |
#       (str_detect(contributor_last_name, "WEINSTEIN") && firstname_firstletter == "A") |
#       (str_detect(contributor_last_name, "GUARASCI") && firstname_firstletter == "P") |
#       (str_detect(contributor_last_name, "OBLANDER") && firstname_firstletter == "K") |
#       (str_detect(contributor_last_name, "ROBINSON") && firstname_firstletter == "A") |
#       (str_detect(contributor_last_name, "KEMPNER") && firstname_firstletter == "M") |
#       (str_detect(contributor_last_name, "SCHUSTER") && firstname_firstletter == "E") |
#       (str_detect(contributor_last_name, "BERGER") && firstname_firstletter == "M") |
#       (str_detect(contributor_last_name, "CARNAHAN") && firstname_firstletter == "T") |
#       (str_detect(contributor_last_name, "PATTERSON") && firstname_firstletter == "C") |
#       (str_detect(contributor_last_name, "YENEROGLU") && firstname_firstletter == "B") |
#       (str_detect(contributor_last_name, "ROBERTSON") && firstname_firstletter == "D") |
#       (str_detect(contributor_last_name, "KAPLAN") && firstname_firstletter == "J") |
#       (str_detect(contributor_last_name, "DEWITT") && firstname_firstletter == "M") |
#       (str_detect(contributor_last_name, "CRAIGHEAD") && firstname_firstletter == "K") |
#       (str_detect(contributor_last_name, "GARDNER") && firstname_firstletter == "A") |
#       (str_detect(contributor_last_name, "OVERTON") && firstname_firstletter == "S") |
#       (str_detect(contributor_last_name, "FREEMAN") && firstname_firstletter == "W") |
#       (str_detect(contributor_last_name, "BAINUM") && firstname_firstletter == "S") |
#       (str_detect(contributor_last_name, "PATTERSON") && firstname_firstletter == "C") |
#       (str_detect(contributor_last_name, "THOMPSON") && firstname_firstletter == "S") |
#       (str_detect(contributor_last_name, "PRITZKER") && firstname_firstletter == "P") |
#       (str_detect(contributor_last_name, "POLLAK") && firstname_firstletter == "A") |
#       (str_detect(contributor_last_name, "STACK") && firstname_firstletter == "G") |
#       (str_detect(contributor_last_name, "BARZUN") && firstname_firstletter == "M") |
#       (str_detect(contributor_last_name, "FERNANDO") && firstname_firstletter == "R") |
#       (str_detect(contributor_last_name, "RICKETTS") && firstname_firstletter == "L") |
#       (str_detect(contributor_last_name, "HIRSHBERG") && firstname_firstletter == "G") |
#       (str_detect(contributor_last_name, "FRIEDMAN") && firstname_firstletter == "D") |
#       (str_detect(contributor_last_name, "WHITE ") && firstname_firstletter == "F") |
#       (str_detect(contributor_last_name, "HALPERN") && firstname_firstletter == "D") |
#       (str_detect(contributor_last_name, "LEVINSON") && firstname_firstletter == "K") |
#       (str_detect(contributor_last_name, "REEVES") && firstname_firstletter == "B") |
#       (str_detect(contributor_last_name, "BERLIN") && firstname_firstletter == "K") |
#       (str_detect(contributor_last_name, "HOFFMAN") && firstname_firstletter == "S") |
#       (str_detect(contributor_last_name, "PIERCE") && firstname_firstletter == "W") |
#       (str_detect(contributor_last_name, "CAVERO") && firstname_firstletter == "J") |
#       (str_detect(contributor_last_name, "GARRISON") && firstname_firstletter == "D") |
#       (str_detect(contributor_last_name, "SAGNER") && firstname_firstletter == "D") |
#       (str_detect(contributor_last_name, "FORRESTER") && firstname_firstletter == "C") |
#       (str_detect(contributor_last_name, "PARHAM") && firstname_firstletter == "M") |
#       (str_detect(contributor_last_name, "SAUJANI") && firstname_firstletter == "R") |
#       (str_detect(contributor_last_name, "YOUNG") && firstname_firstletter == "J") |
#       (str_detect(contributor_last_name, "FARMER") && firstname_firstletter == "E") |
#       (str_detect(contributor_last_name, "HAMILTON") && firstname_firstletter == "C") |
#       (str_detect(contributor_last_name, "ZINN") && firstname_firstletter == "C") |
#       (str_detect(contributor_last_name, "KRAUS") && firstname_firstletter == "M") |
#       (str_detect(contributor_last_name, "SMITH") && firstname_firstletter == "D") |
#       (str_detect(contributor_last_name, "BAKALAR") && firstname_firstletter == "S") |
#       (str_detect(contributor_last_name, "EYCHANER") && firstname_firstletter == "F") |
#       (str_detect(contributor_last_name, "SACKS") && firstname_firstletter == "M") |
#       (str_detect(contributor_last_name, "GALLOGLY") && firstname_firstletter == "M") |
#       (str_detect(contributor_last_name, "POHLAD") && firstname_firstletter == "R") |
#       (str_detect(contributor_last_name, "TSUNIS") && firstname_firstletter == "G") |
#       (str_detect(contributor_last_name, "MEREDITH") && firstname_firstletter == "L") |
#       (str_detect(contributor_last_name, "TOBIAS") && firstname_firstletter == "A") |
#       (str_detect(contributor_last_name, "BONNIE") && firstname_firstletter == "S") |
#       (str_detect(contributor_last_name, "DODGE") && firstname_firstletter == "S") |
#       (str_detect(contributor_last_name, "ADAMS") && firstname_firstletter == "C") |
#       (str_detect(contributor_last_name, "PHILLIPS") && firstname_firstletter == "J") |
#       (str_detect(contributor_last_name, "WILBOURN") && firstname_firstletter == "M") |
#       (str_detect(contributor_last_name, "MYERS") && firstname_firstletter == "C") |
#       (str_detect(contributor_last_name, "PALNICK") && firstname_firstletter == "L") |
#       (str_detect(contributor_last_name, "MADDOX") && firstname_firstletter == "K") |
#       (str_detect(contributor_last_name, "ALTERS") && firstname_firstletter == "J") |
#       (str_detect(contributor_last_name, "SPINNER") && firstname_firstletter == "S") |
#       (str_detect(contributor_last_name, "ALDERMAN") && firstname_firstletter == "M") |
#       (str_detect(contributor_last_name, "BROWN") && firstname_firstletter == "K") |
#       (str_detect(contributor_last_name, "TSAO-WU") && firstname_firstletter == "G") |
#       (str_detect(contributor_last_name, "CRUMPLER") && firstname_firstletter == "J") |
#       (str_detect(contributor_last_name, "DORNBUSH") && firstname_firstletter == "K") |
#       (str_detect(contributor_last_name, "EMERSON") && firstname_firstletter == "P") |
#       (str_detect(contributor_last_name, "DWYER") && firstname_firstletter == "C") |
#       (str_detect(contributor_last_name, "FELSEN") && firstname_firstletter == "S") |
#       (str_detect(contributor_last_name, "TANNER") && firstname_firstletter == "J") |
#       (str_detect(contributor_last_name, "JORDAN") && firstname_firstletter == "W") |
#       (str_detect(contributor_last_name, "STETSON") && firstname_firstletter == "B") |
#       (str_detect(contributor_last_name, "STETSON") && firstname_firstletter == "J") |
#       (str_detect(contributor_last_name, "NATHAN") && firstname_firstletter == "S") |
#       (str_detect(contributor_last_name, "ABERLY") && firstname_firstletter == "N") |
#       (str_detect(contributor_last_name, "KOVNER") && firstname_firstletter == "S") |
#       (str_detect(contributor_last_name, "EFFRON") && firstname_firstletter == "B") |
#       (str_detect(contributor_last_name, "MEREDITH") && firstname_firstletter == "T") |
#       (str_detect(contributor_last_name, "WELTERS") && firstname_firstletter == "T") |
#       (str_detect(contributor_last_name, "GILBERT") && firstname_firstletter == "M") |
#       (str_detect(contributor_last_name, "MONKS") && firstname_firstletter == "B") |
#       (str_detect(contributor_last_name, "HARTLEY") && firstname_firstletter == "J") |
#       (str_detect(contributor_last_name, "WESNER") && firstname_firstletter == "A") |
#       (str_detect(contributor_last_name, "BENIOFF") && firstname_firstletter == "M") |
#       (str_detect(contributor_last_name, "WESTLY") && firstname_firstletter == "S") |
#       (str_detect(contributor_last_name, "STREETER") && firstname_firstletter == "J") |
#       (str_detect(contributor_last_name, "GREEN") && firstname_firstletter == "S") |
#       (str_detect(contributor_last_name, "ROCHE") && firstname_firstletter == "R") |
#       (str_detect(contributor_last_name, "BELL") && firstname_firstletter == "C") |
#       (str_detect(contributor_last_name, "LASSITER") && firstname_firstletter == "M") |
#       (str_detect(contributor_last_name, "BESHAR") && firstname_firstletter == "P") |
#       (str_detect(contributor_last_name, "CANFIELD") && firstname_firstletter == "K") |
#       (str_detect(contributor_last_name, "PENSKY") && firstname_firstletter == "C") |
#       (str_detect(contributor_last_name, "GRAY") && firstname_firstletter == "R") |
#       (str_detect(contributor_last_name, "RYAN") && firstname_firstletter == "C") |
#       (str_detect(contributor_last_name, "EMERSON") && firstname_firstletter == "J") |
#       (str_detect(contributor_last_name, "DRAPER") && firstname_firstletter == "B") |
#       (str_detect(contributor_last_name, "KENNEY") && firstname_firstletter == "D") |
#       (str_detect(contributor_last_name, "CONLON") && firstname_firstletter == "K") |
#       (str_detect(contributor_last_name, "SUSMAN") && firstname_firstletter == "S") |
#       (str_detect(contributor_last_name, "HECKLER") && firstname_firstletter == "A") |
#       (str_detect(contributor_last_name, "KELLER") && firstname_firstletter == "J") |
#       (str_detect(contributor_last_name, "PAREKH") && firstname_firstletter == "D") |
#       (str_detect(contributor_last_name, "ZUBERI") && firstname_firstletter == "I") |
#       (str_detect(contributor_last_name, "WINTOUR") && firstname_firstletter == "A") |
#       (str_detect(contributor_last_name, "GOODMAN") && firstname_firstletter == "M") |
#       (str_detect(contributor_last_name, "KONG") && firstname_firstletter == "M") |
#       (str_detect(contributor_last_name, "LONGORIA") && firstname_firstletter == "E") |
#       (str_detect(contributor_last_name, "DLUGACZ") && firstname_firstletter == "J") |
#       (str_detect(contributor_last_name, "NICHOLS") && firstname_firstletter == "M") |
#       (str_detect(contributor_last_name, "LEVINE") && firstname_firstletter == "C") |
#       (str_detect(contributor_last_name, "TOOKES") && firstname_firstletter == "P") |
#       (str_detect(contributor_last_name, "SCOTT") && firstname_firstletter == "S") |
#       (str_detect(contributor_last_name, "HARRIS") && firstname_firstletter == "S") |
#       (str_detect(contributor_last_name, "RIVKIN") && firstname_firstletter == "D") |
#       (str_detect(contributor_last_name, "HARRIS") && firstname_firstletter == "K") |
#       (str_detect(contributor_last_name, "KENNEDY") && firstname_firstletter == "L") |
#       (str_detect(contributor_last_name, "GRIFFIN") && firstname_firstletter == "C") |
#       (str_detect(contributor_last_name, "BAUER") && firstname_firstletter == "D") |
#       (str_detect(contributor_last_name, "HAMAMOTO") && firstname_firstletter == "P") |
#       (str_detect(contributor_last_name, "BUELL") && firstname_firstletter == "J") |
#       (str_detect(contributor_last_name, "LOPEZ") && firstname_firstletter == "A") |
#       (str_detect(contributor_last_name, "MATHIS") && firstname_firstletter == "B") |
#       (str_detect(contributor_last_name, "HANSON") && firstname_firstletter == "C") |
#       (str_detect(contributor_last_name, "STEFANI") && firstname_firstletter == "G") |
#       (str_detect(contributor_last_name, "KOSAR") && firstname_firstletter == "F") |
#       (str_detect(contributor_last_name, "KORGE") && firstname_firstletter == "A") |
#       (str_detect(contributor_last_name, "HARPER") && firstname_firstletter == "K") |
#       (str_detect(contributor_last_name, "CORZINE") && firstname_firstletter == "J") |
#       (str_detect(contributor_last_name, "CROWN") && firstname_firstletter == "J") |
#       (str_detect(contributor_last_name, "MORGAN") && firstname_firstletter == "J") |
#       (str_detect(contributor_last_name, "KRAMER") && firstname_firstletter == "O") |
#       (str_detect(contributor_last_name, "WOLF") && firstname_firstletter == "R") |
#       (str_detect(contributor_last_name, "LEVINE") && firstname_firstletter == "S") |
#       (str_detect(contributor_last_name, "KORGE") && firstname_firstletter == "C") |
#       (str_detect(contributor_last_name, "FALK") && firstname_firstletter == "J") |
#       (str_detect(contributor_last_name, "BUSH") && firstname_firstletter == "D") |
#       (str_detect(contributor_last_name, "DUNHAM") && firstname_firstletter == "D") |
#       (str_detect(contributor_last_name, "MUNOZ") && firstname_firstletter == "H") |
#       (str_detect(contributor_last_name, "WATTS") && firstname_firstletter == "M") |
#       (str_detect(contributor_last_name, "BLOOD") && firstname_firstletter == "D") |
#       (str_detect(contributor_last_name, "COHEN") && firstname_firstletter == "S") |
#       (str_detect(contributor_last_name, "HICKEY") && firstname_firstletter == "D") |
#       (str_detect(contributor_last_name, "WHEELER") && firstname_firstletter == "T") |
#       (str_detect(contributor_last_name, "MASON") && firstname_firstletter == "L") |
#       (str_detect(contributor_last_name, "RUEFF") && firstname_firstletter == "R") |
#       (str_detect(contributor_last_name, "BAILEY") && firstname_firstletter == "J") |
#       (str_detect(contributor_last_name, "SHERMAN") && firstname_firstletter == "B") |
#       (str_detect(contributor_last_name, "CANTOR") && firstname_firstletter == "J") |
#       (str_detect(contributor_last_name, "ECCLES") && firstname_firstletter == "L") |
#       (str_detect(contributor_last_name, "MURPHY") && firstname_firstletter == "R") |
#       (str_detect(contributor_last_name, "PERRY") && firstname_firstletter == "T") |
#       (str_detect(contributor_last_name, "SOLOMON") && firstname_firstletter == "K") |
#       (str_detect(contributor_last_name, "STEIN") && firstname_firstletter == "B") |
#       (str_detect(contributor_last_name, "POLSFUT") && firstname_firstletter == "J") |
#       (str_detect(contributor_last_name, "BERGER") && firstname_firstletter == "J") |
#       (str_detect(contributor_last_name, "MASON") && firstname_firstletter == "K") |
#       (str_detect(contributor_last_name, "SOLOW") && firstname_firstletter == "D") |
#       (str_detect(contributor_last_name, "SACCA") && firstname_firstletter == "C") |
#       (str_detect(contributor_last_name, "RASKY") && firstname_firstletter == "L") |
#       (str_detect(contributor_last_name, "POLLARA") && firstname_firstletter == "B") |
#       (str_detect(contributor_last_name, "SNYDER") && firstname_firstletter == "J") |
#       (str_detect(contributor_last_name, "LASRY") && firstname_firstletter == "M") |
#       (str_detect(contributor_last_name, "BLUHM") && firstname_firstletter == "N") |
#       (str_detect(contributor_last_name, "ROSEN") && firstname_firstletter == "J") |
#       (str_detect(contributor_last_name, "MILLER") && firstname_firstletter == "L") |
#       (str_detect(contributor_last_name, "JAMES") && firstname_firstletter == "T") |
#       (str_detect(contributor_last_name, "WAGAR") && firstname_firstletter == "K") |
#       (str_detect(contributor_last_name, "SOLOW") && firstname_firstletter == "A") |
#       (str_detect(contributor_last_name, "FRANK") && firstname_firstletter == "J") |
#       (str_detect(contributor_last_name, "BEAN") && firstname_firstletter == "T") |
#       (str_detect(contributor_last_name, "FULP") && firstname_firstletter == "C") |
#       (str_detect(contributor_last_name, "CHASE") && firstname_firstletter == "T") |
#       (str_detect(contributor_last_name, "LION") && firstname_firstletter == "M") |
#       (str_detect(contributor_last_name, "RAJI") && firstname_firstletter == "A") |
#       (str_detect(contributor_last_name, "MAMET") && firstname_firstletter == "N") |
#       (str_detect(contributor_last_name, "BARBER") && firstname_firstletter == "R") |
#       (str_detect(contributor_last_name, "SPAHN") && firstname_firstletter == "A") |
#       (str_detect(contributor_last_name, "WALLS") && firstname_firstletter == "T") |
#       (str_detect(contributor_last_name, "NESS") && firstname_firstletter == "S") |
#       (str_detect(contributor_last_name, "WELCH") && firstname_firstletter == "J") |
#       (str_detect(contributor_last_name, "DAY") && firstname_firstletter == "J") |
#       (str_detect(contributor_last_name, "BURD") && firstname_firstletter == "D") |
#       (str_detect(contributor_last_name, "PAUL") && firstname_firstletter == "G") |
#       (str_detect(contributor_last_name, "CLARK") && firstname_firstletter == "B") |
#       (str_detect(contributor_last_name, "RUDY") && firstname_firstletter == "K") |
#       (str_detect(contributor_last_name, "BROAS") && firstname_firstletter == "T") |
#       (str_detect(contributor_last_name, "LEE") && firstname_firstletter == "B") |
#       (str_detect(contributor_last_name, "CONEY") && firstname_firstletter == "L") |
#       (str_detect(contributor_last_name, "EAKES") && firstname_firstletter == "P") |
#       (str_detect(contributor_last_name, "AUSTIN") && firstname_firstletter == "J") |
#       (str_detect(contributor_last_name, "SINGH") && firstname_firstletter == "A") |
#       (str_detect(contributor_last_name, "GOYLE") && firstname_firstletter == "R") |
#       (str_detect(contributor_last_name, "MUSE") && firstname_firstletter == "M") |
#       (str_detect(contributor_last_name, "FORD") && firstname_firstletter == "T")
#     ) %>% 
#   collect()
# 
# 
# head(targetlist)


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
