#!/usr/bin/env Rscript --vanilla

#------------------------------------------------------------------------------#
#  TITLE: Combine MBBS ebird and website data
#   DATE: 20181216
#   PROG: B. Saul
#   DESC: Combine MBBS ebird and website data. Removing duplicates and cleaning
#         as necessary
#------------------------------------------------------------------------------#

library(magrittr)
library(mbbs)
devtools::load_all()

etax <- get_ebird_taxonomy()

# import Orange
mbbs_orange <-
  import_ebird_data("inst/extdata/MyEBirdData_Orange_20230713.csv") %>%
  prepare_mbbs_data(
    mbbs_site_dt =
      readr::read_csv("inst/extdata/orange_1999-2009_from_website.csv") %>%
      dplyr::mutate(
        # NOTE: date format is different in orange county data
        date = lubridate::mdy(date), #date format here WAS ymd before 
        time = as.character(time)
      ),
    ebird_taxonomy = etax
  ) %>%
  combine_site_ebird() %>%
  process_observers("orange")


# import Durham
mbbs_durham <-
  import_ebird_data("inst/extdata/MyEBirdData_Durham_20240117.csv") %>%
  prepare_mbbs_data(
    mbbs_site_dt =
      readr::read_csv("inst/extdata/durham_2002-2009_from_website.csv") %>%
        dplyr::filter(!is.na(route_num)) %>%
        dplyr::mutate(
          date = lubridate::mdy(date),
          time = as.character(time)
        ),
    ebird_taxonomy = etax
  ) %>%
  combine_site_ebird() %>%
  process_observers("durham")

# import Chatham
mbbs_chatham <-
  import_ebird_data("inst/extdata/MyEBirdData_Chatham_20240319.csv") %>%
  prepare_mbbs_data(
    mbbs_site_dt =
      readr::read_csv("inst/extdata/chatham_2000-2009_from_website.csv") %>%
        dplyr::mutate(
          date = lubridate::mdy(date),
          time = as.character(time)
        ),
    ebird_taxonomy = etax
  ) %>%
  combine_site_ebird() %>%
  process_observers("chatham")

# Combine counties
mbbs <- bind_rows(mbbs_orange, mbbs_chatham, mbbs_durham) %>%
  mbbs_generate_route_ID()

# Save results ####
save(mbbs_orange, file = "data/mbbs_orange.rda")
save(mbbs_durham, file = "data/mbbs_durham.rda")
save(mbbs_chatham, file = "data/mbbs_chatham.rda")
save(mbbs, file = "data/mbbs.rda")
update_survey_events()

# Create CSV version ####
write.csv(mbbs_orange, file = sprintf(
  "inst/analysis_data/mbbs_orange_%s.csv",
  format(Sys.Date(), "%Y%m%d")
))
write.csv(mbbs_durham, file = sprintf(
  "inst/analysis_data/mbbs_durham_%s.csv",
  format(Sys.Date(), "%Y%m%d")
))
write.csv(mbbs_chatham, file = sprintf(
  "inst/analysis_data/mbbs_chatham_%s.csv",
  format(Sys.Date(), "%Y%m%d")
))


rm(list = ls())