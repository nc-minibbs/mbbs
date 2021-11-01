#------------------------------------------------------------------------------#
#  TITLE: Create a dataset with additional information about species in the MBBS
#   DATE: 20190203
#   PROG: B. Saul
#   DESC: 
#------------------------------------------------------------------------------#

library(mbbs)
library(dplyr)

mbbs_species <- dplyr::distinct(mbbs, common_name, sci_name, spec_code)

## 1. Identify species of local concern per NHA ####
# https://www.newhopeaudubon.org/conservation/species-of-local-concern/
local_concern <- read.table("inst/extdata/nha_local_concern.txt", 
                            header = TRUE, stringsAsFactors = FALSE) %>%
  pull(nha_local_concern)

## 2. Identify whether image is available + image credits ####
image_files <-  list.files(path = "inst/images",
                      pattern = "(jpg|png|gif)$",
                      full.names = FALSE,
                      recursive = TRUE,
                      include.dirs = FALSE)
images <- read.table("inst/images/image_credits", 
                      sep = ",",
                      header = TRUE, stringsAsFactors = FALSE) %>%
  mutate(
    common_name = stringr::str_replace(image, "\\.(jpg|png|gif)$", ""),
    common_name = stringr::str_replace_all(common_name, "_", " ")
  )

if(!all(image_files %in% images$image) && !!all(images$image %in% image_files)){
  stop("There is a mismatch between one or more image files and the image_credits.")
}

if(!all(images$common_name %in% mbbs_species$common_name)){
  stop("There is a mismatch between common names in the image data and mbbs data.")
}

## X. Put it all together #### 

mbbs_species <- mbbs_species %>%
  mutate(
    local_concern = common_name %in% local_concern
  ) %>%
  left_join(images, by = "common_name")

## X. Save results ####

save(mbbs_species, file = "data/mbbs_species.rda")