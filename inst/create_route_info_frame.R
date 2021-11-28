## Creates mbbs_routes data.frame
library(dplyr)
 
get_routes <- function(x){
  # In 2020 counts were started to be recorded per stop; whereas this dataset
  # is to align routes recorded in the aggregated route format.
  x %>% 
    filter(year < 2020) %>%
    distinct(mbbs_county, route_num, lat, lon)
}

mbbs_routes <-
  bind_rows(
    import_ebird_data('inst/extdata/MyEBirdData_Orange_20211030.csv') %>%
      get_routes(),
    import_ebird_data('inst/extdata/MyEBirdData_Durham_20211030.csv') %>%
      get_routes(),
    import_ebird_data('inst/extdata/MyEBirdData_Chatham_20211030.csv') %>%
      get_routes()
  )

save(mbbs_routes, file = "data/mbbs_routes.rda")
