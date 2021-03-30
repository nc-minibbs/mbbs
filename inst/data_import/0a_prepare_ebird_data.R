#------------------------------------------------------------------------------#
#  TITLE: Import MBBS ebird data 
#   DATE: 20181216
#   PROG: B. Saul
#   DESC: Import MBBS data downloaded from the ebird site in MyEBirdData.csv
#------------------------------------------------------------------------------#
library(dplyr)
library(mbbs)

## Read data ####
# dt <- read.csv("inst/extdata/MyEBirdData.csv", stringsAsFactors = FALSE)
dt <- read.csv('inst/extdata/MyEBirdData_Orange_20210316.csv', stringsAsFactors = FALSE)
dt$obs_id <- 1:nrow(dt)

## Prepare eBird data ####
ebird_dt <-
  dt %>%
  select(
    obs_id,
    sub_id      = Submission.ID,
    common_name = Common.Name,
    sci_name    = Scientific.Name,
    tax_order   = Taxonomic.Order,
    count       = Count,
    state       = State.Province,
    county      = County,
    loc         = Location,
    lat         = Latitude,
    lon         = Longitude,
    date        = Date,
    time        = Time,
    distance    = Distance.Traveled..km.,
    all_obs     = All.Obs.Reported,
    breed_code  = Breeding.Code,
    # species_comment = Species.Comments # Column doesn't appear to exist in 
    #                   latest eBrid export
  ) %>%
  ## Clean up comments ##
  left_join(
    comment_workflow(comments), 
    by = c("sub_id")
  ) %>%
  # Clean up routes
  mutate(
    loc = case_when(
      loc == "MBBS, NC, Orange Co, Route 5, Arthur Minnis" ~ 
        "MBBS, NC, Orange Co, Route 05, Arthur Minnis Road",
      TRUE ~ loc
    )
  ) %>%
  mutate(
    count = as.integer(count), 
    # TODO: how the few cases where count is not numeric be handled?
    date  = as.Date(date, format = "%m-%d-%Y"),
    year  = lubridate::year(date),
    route = stringr::str_replace(loc, "MBBS, NC, Orange Co, ", ""),
    route_num = stringr::str_extract(route, "[1-9][0-9]*")
  ) 
  


