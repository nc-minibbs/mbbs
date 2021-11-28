## Creates ibp_spec_codes data.frame
## data downloaded from 
## https://www.birdpop.org/pages/birdSpeciesCodes.php
library(dplyr)
 
ibp_spec_codes <-
  read.csv("inst/extdata/IBP-AOS-LIST21.csv", stringsAsFactors = FALSE) %>%
  select(aos_sci_name = SCINAME, aos_common_name = COMMONNAME, spec_code = SPEC)

save(ibp_spec_codes, file = "data/ibp_spec_codes.rda")
