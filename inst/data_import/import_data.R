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
library(stringr)
library(dplyr)


mbbs_orange <-
  import_ebird_data('inst/extdata/MyEBirdData_Orange_20210917.csv') %>%
  prepare_mbbs_data(
    mbbs_site_dt = readRDS("inst/extdata/mbbs_orange_sitescrape_20190127.rds")  
  ) %>%
  combine_site_ebird()



## Save results ####
# 
# save(mbbs, file = "data/mbbs.rda")
# 
# ## 8. Create CSV version ####
# 
# write.csv(mbbs, file = sprintf("inst/extdata/mbbs_data_%s.csv", format(Sys.Date(), "%Y%m%d")))
# 
# rm(list = ls())
