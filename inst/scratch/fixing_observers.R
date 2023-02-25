library(dplyr)

#old <- read.csv("inst/extdata/orange_1999-2009_from_website.csv")
#all old data has observers

#check observers in each year
load("data/mbbs_orange.rda")
obs_only <- mbbs_orange %>% select(year, route_num, stop_num, notes, checklist_comments, observers)
unique(mbbs_orange$observers)
#easiest part - check year/route/stop1 has observer, if yes give that same observer to all the other year/route/stops
#let's make a function that can do this for just county - the processing runs on each county individualy so that's the way to go
#for data QC:
#     flag if observer doesn't show up on the conversion table?
#     flag is observer is blank
#for analysis QC:
#     flag if the route was only run by that observer in one year

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