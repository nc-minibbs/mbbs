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
mbbs_orange <-
  import_ebird_data('inst/extdata/MyEBirdData_Orange_20210316.csv') %>%
  prepare_mbbs_data(
    # mbbs_site_20190127.rds contains website data for Orange only
    mbbs_site_dt = readRDS("inst/extdata/mbbs_site_20190127.rds")  
  ) %>%
  combine_site_ebird()



# TODO: add data checks
mbbs_orange %>%
  distinct(sub_id, year, route_num) %>%
  group_by(year, route_num) %>% 
  mutate(
    n = n()
  ) %>%
  filter(n > 1, n != 20) %>%
  arrange(year, route_num) %>%
  select(sub_id, year, route_num)

mbbs_orange %>%
  group_by(mbbs_county, year, route_num, stop_num, common_name) %>%
  tally() %>% 
  filter(n > 1) 

# TODO: where to save result files?

## 7. Save results ####
# 
# save(mbbs, file = "data/mbbs.rda")
# 
# ## 8. Create CSV version ####
# 
# write.csv(mbbs, file = sprintf("inst/extdata/mbbs_data_%s.csv", format(Sys.Date(), "%Y%m%d")))
# 
# rm(list = ls())
