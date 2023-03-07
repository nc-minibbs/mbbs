library(dplyr)
library(stringr)

#old <- read.csv("inst/extdata/orange_1999-2009_from_website.csv")
#all old data has observers

#pull up R/process_comments.R and import_ebird_data.R
#run functions in there
#grab an ebird file ie any one of the csvs "inst/extdata/MyEBirdData_Orange_20220913.csv"
#use the comment_workflow() to check out things went

test <- read.csv( "inst/extdata/MyEBirdData_Orange_20220913.csv", header = T)
test <- test %>% import_ebird_data()
test <- test %>% rename_ebird_data() 
comment_workflow(test) #this isn't working yet, getting an "'Str_replace_all' is not an exported object from 'namespace:stringr'" error when trying to run it. Admittedly I'm running things all out of order, should maybe run it as
#yeah, if ur fixing the comment processer to pull observer correctly, just run the whole database upload again. It's literally fine to do that as many times as needed, we're on a different branch and we'll just merge once this is all updated. 

#check observers in each year
load("data/mbbs_orange.rda")
obs_only <- mbbs_orange %>% select(year, route_num, stop_num, notes, checklist_comments, observers, mbbs_county)
#easiest part - check year/route/stop1 has observer, if yes give that same observer to all the other year/route/stops
#let's make a function that can do this for just county - the processing runs on each county individualy so that's the way to go
#for data QC:
#     flag if observer doesn't show up on the conversion table?
#     flag is observer is blank
#for analysis QC:
#     flag if the route was only run by that observer in one year


#3-7-2023 
#create primary observer field conversion table, pull out unique table with route as a variable and send it to excel
load("data/mbbs_orange.rda")
load("data/mbbs_durham.rda")
load("data/mbbs_chatham.rda")
active <- mbbs_orange %>% filter(route_num == 3)
table(active$observers, active$year)
#I think I literally want a year, route, unique observers within that

o <- unique(mbbs_orange$route_num)
c <- unique(mbbs_chatham$route_num)
d <- unique(mbbs_durham$route_num)


observer_table <- read.csv("inst/extdata/observer_conversion_table.csv", header = T)
observer_table <- observer_table %>% arrange(county, route)
write.csv(observer_table, "inst/extdata/observer_conversion_table.csv", row.names = F)


# Check that routes have exactly 1 or 20 non-owling submissions.
# TODO: 2020 and after should have 20 submissions
dt %>%
  distinct(year, mbbs_county, route_num, stop_num) %>%
  group_by(year, mbbs_county, route_num) %>%
  dplyr::summarise(
    n = dplyr::n(),
    flag = !(n %in% c(1, 20))
  ) %>%
  {
    x <- .
    probs <-
      x[x$flag, ] %>%
      mutate(
        desc = glue::glue("{mbbs_county}, {year}, {route_num}")
      ) %>%
      pull(desc) %>%
      paste0(collapse = "\n * ")
    
    if (any(x$flag)) {
      warning(sprintf(
        "The following year/route don't have either 1 or 20 checklists:\n %s",
        probs
      ))
    }
  }

dt
}