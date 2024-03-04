load("data/mbbs_orange.rda")

test <- unique(mbbs_orange$species_comments)
test <- as.data.frame(test)

library(dplyr)

test <- test %>%
  mutate(input = test) %>%
  select(input)

library(stringr)
#first step is probably to change the unicode errors on the enter signs

test$input <- test$input %>%
  stringr::str_replace_all("&#61;| =", "=")

mbbs_county$checklist_comments <- mbbs_county$checklist_comments %>%
  stringr::str_replace_all("&#61;| =", "=")