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

etax <- get_ebird_taxonomy()

# import Orange
mbbs_orange <-
  import_ebird_data("inst/extdata/MyEBirdData_Orange_20211030.csv") %>%
  prepare_mbbs_data(
    mbbs_site_dt =
      readr::read_csv("inst/extdata/orange_1999-2009_from_website.csv") %>%
      dplyr::mutate(
        # NOTE: date format is different in orange county data
        date = lubridate::ymd(date),
        time = as.character(time)
      ),
    ebird_taxonomy = etax
  ) %>%
  combine_site_ebird()

# import Durham
mbbs_durham <-
  import_ebird_data("inst/extdata/MyEBirdData_Durham_20211030.csv") %>%
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
  combine_site_ebird()

# import Chatham
mbbs_chatham <-
  import_ebird_data("inst/extdata/MyEBirdData_Chatham_20211030.csv") %>%
  prepare_mbbs_data(
    mbbs_site_dt =
      readr::read_csv("inst/extdata/chatham_2000-2009_from_website.csv") %>%
        dplyr::mutate(
          date = lubridate::mdy(date),
          time = as.character(time)
        ),
    ebird_taxonomy = etax
  ) %>%
  combine_site_ebird()

# Save results ####
save(mbbs_orange, file = "data/mbbs_orange.rda")
save(mbbs_durham, file = "data/mbbs_durham.rda")
save(mbbs_chatham, file = "data/mbbs_chatham.rda")

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
