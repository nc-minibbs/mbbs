##Exploring stop-level data

library(dplyr)
library(stringr)

load("data/mbbs_orange.rda")
load("data/mbbs_durham.rda")
load("data/mbbs_chatham.rda")

#create mbbs_all
mbbs_all <- bind_rows(mbbs_orange, mbbs_chatham, mbbs_durham)

#Fix potential error sources
#keep route_num, but add a route variable that has a unique route for each county. 
#let's actually REMOVE the county column so it doesn't get confusing, becuase mbbs_county is always correct and the county column from ebird is sometimes not (ie: routes right next to Wake county that ebird sorts into Wake)
mbbs_all <- mbbs_all %>% 
  mutate(
    # The route number is not unique within the study
    # (only within a county).
    route_ID = route_num + case_when(
      mbbs_county == "orange" ~ 100L,
      mbbs_county == "durham" ~ 200L,
      mbbs_county == "chatham" ~ 300L,
    )
  )

table(mbbs_all$stop_num, mbbs_all$year)
#so, 18211 records with all the stops, buuuut basically all from 2020+
#and plenty of cases where stop_num hasn't been pulled in the recent years too


