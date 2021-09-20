# Mini-Bird Breeding Survey

This repository contains code to download and analyze data from the [Mini-Bird Breeding Surveys](http://minibbs.us) conducted in Orange, Chatham, and Durham counties.

[View the results website](http://minibbs.us)

## Importing Data from eBird

```r
library(magrittr)
library(mbbs)

mbbs_orange <-
  import_ebird_data('inst/extdata/MyEBirdData_Orange_20210316.csv') %>%
  prepare_mbbs_data(
    # mbbs_site_20190127.rds contains website data for Orange only
    mbbs_site_dt = readRDS("inst/extdata/mbbs_site_20190127.rds")  
  ) %>%
  combine_site_ebird()
```

