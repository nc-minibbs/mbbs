load("data/mbbs_orange.rda")
library(stringr)
library(dplyr)


test <- unique(mbbs_orange$species_comments)
test <- as.data.frame(test) 

test <- test %>%
  mutate(input = test) %>%
  select(input) 
test[1,1] <- "12,,,"
test[619,1] <- "no"


#first step is probably to change the unicode errors on the enter signs
test$input <- test$input %>%
  stringr::str_replace_all("&#61;| =", "=")

#let's make the dataframe we want to put the new information into.
cols_list <- c("s1",'s2','s3','s4',"s5","s6","s7","s8","s9","s10","s11","s12","s13","s14","s15","s16","s17","s18","s19","s20", "common_name", "route_num", "year", "mbbs_county")
df <- as.data.frame(matrix(ncol = length(cols_list), nrow = 0))
#name the columns
colnames(df) <- cols_list


#if comment is formatted like ,,,,2,,,, then, we want a regex expression that identifies if it starts with either a comma, or a number [0-9] and then a comma, and go from there extracting after every comma. 
# -check number of commas? ought to be 19?
# -....can probably use a stringr function to like, use the commas as a separator and then put the values into a table, with stop 1-20 and then count. 
#let's first have a function to seperate out all the different cases of numbers. After that, we can worry about like, cleaning out cases where "," is meant to equal a zero from cases where it's "[0-9]," and the comma should just be removed. You know...this might genuinely work better as a for loop with if and if else statements where I can talk and nest within them rather than dplyr's mutate.....yeah
test <- test %>%
  mutate(stop1 = case_when(
    stringr::str_starts(input, ",|[0-9]+,") == TRUE ~ stringr::str_extract(input, ",|[0-9]+,") 
  ))



for(i in 1:nrow(test)) {
  if(stringr::str_starts(test$input[i], ",|[0-9]+,") == TRUE) { #follows ,,,,,,1,,,, format
    test$stop1[i] <- stringr::str_extract(test$input[i], ",|[0-9]+,") 
  } else {
    test$stop1[i] <- NA
  }
}

stringr:

mbbs_county$checklist_comments <- mbbs_county$checklist_comments %>%
  stringr::str_replace_all("&#61;| =", "=")