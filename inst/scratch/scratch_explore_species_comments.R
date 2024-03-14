load("data/mbbs_orange.rda")
load("data/mbbs_durham.rda")
load("data/mbbs_chatham.rda")
load("data/mbbs.rda")
library(stringr)
library(dplyr)


test <- unique(mbbs_chatham$species_comments)
#test <- unique(mbbs$species_comments)
#test <- test[is.na()==FALSE]
test <- as.data.frame(test) 

test <- test %>%
  mutate(input = test) %>%
  select(input) 
#test[1,1] <- "12,,,"
#test[619,1] <- "no"
#test[1530,1] <- "no"


#first step is probably to change the unicode errors on the enter signs
test$input <- test$input %>%
  stringr::str_replace_all("&#61;| =", "=")

#let's make the dataframe we want to put the new information into.
reset_df <- function() {
cols_list <- c("s1",'s2','s3','s4',"s5","s6","s7","s8","s9","s10","s11","s12","s13","s14","s15","s16","s17","s18","s19","s20","input",  "common_name", "route_num", "year", "mbbs_county", "note")
df <- as.data.frame(matrix(ncol = length(cols_list), nrow = nrow(test)))
#name the columns
colnames(df) <- cols_list

#set the first column
df$input <- test$input

return(df)
}

df <- reset_df()

#if comment is formatted like ,,,,2,,,, then, we want a regex expression that identifies if it starts with either a comma, or a number [0-9] and then a comma, and go from there extracting after every comma. 
# -check number of commas? ought to be 19?
# -....can probably use a stringr function to like, use the commas as a separator and then put the values into a table, with stop 1-20 and then count. 
#let's first have a function to seperate out all the different cases of numbers. After that, we can worry about like, cleaning out cases where "," is meant to equal a zero from cases where it's "[0-9]," and the comma should just be removed. You know...this might genuinely work better as a for loop with if and if else statements where I can talk and nest within them rather than dplyr's mutate.....yeah


split_list <- NA
stop <- NA
count <- NA

for(i in 1:nrow(df)) {
  if(is.na(df$input[i]) == TRUE | is.null(df$input[i])) { #if the row is blank or is an NA
    print("error, skip row")
    df$note[i] <- "error, skip row"
  } else if(stringr::str_starts(df$input[i], ",|[0-9]+,") == TRUE) { #follows ,,,,,,1,,,, format

    #split the species_comment based on commas
    split_list <- (str_split(df$input[i], ","))[[1]] %>%
      #convert empty strings (nothing between commas) into 0s
      str_replace_all("^$", "0") %>% #"^$" denotes empty string in regex
      #turn characters to numbers
      as.numeric()
    
    #record how many stops there were, we're going to need to give an error/flag something if less than, idk 18 for now. Checking records, there's only one case of 19 from this note. I'm not worried. HOWEVER. There are some examples with 21
    df$note[i] <- paste("comma sep,", length(split_list))

    #for each entry in split_list, put it in the associated s#
    #if split_list is short, ie: length 4, other entries after 4 will be NA
    #if split_list is 21, ie: they put a final comma into the dataset at stop 20 ex: 1 vs 1, - we just leave that last 0 out. In the testing dataset, which is just mbbs_orange, all examples with a split_list of length 21 this seemed to be the exact issue. !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!Will need more testing when move to full dataset to confirm no other errors
    for(a in 1:20) {
      df[i,a] <- split_list[a]
    }
    
    #convert NAs to 0
    
  } else if(stringr::str_starts(df$input[i], "[0-9]+=[0-9]+|st(op)?( )?[0-9]+=[0-9]+") == TRUE) { #
    #split the species_comments based on , or ;
    split_list <- str_split(df$input[i], ",|;")[[1]] %>%
      #if it starts with st we're going to remove the sts
      str_replace_all("( )?st(op)?( )?", "")
    
    df$note[i] <- paste("X=X,",length(split_list))
    
    #for each entry in split_list, use the start of the entry for what stop, and the end of the entry for what count
    for(a in 1:length(split_list)) {
      stop <- as.numeric(str_extract(split_list[a], "[0-9]+")) #extracts the first set of numbers
      count <- as.numeric(str_extract(split_list[a], "(?<=\\=)[0-9]+")) #extracts a set of numbers after an equal sign 
      df[i,stop] <- count 
    }
    #convert NAs to 0
    df[i, 1:20][is.na(df[i, 1:20])] <- 0
  } else if(stringr::str_starts(df$input[i], "Stops( )?(=)?(:)?") == TRUE) { #starts with Stops: or Stops = 
    #split the species_comments based on ,
    split_list <- str_split(df$input[i], ",")[[1]] %>%
      #remove the Stops = / Stops:
      str_replace_all("Stops( )?(=)?(:)?( )?", "")
    
    df$note[i] <- paste("S:/=,",length(split_list))
    
    for(a in 1:20) {
      df[i,a] <- split_list[a]
    }
  }
}

#convert leftover NAs in df[,1:20] to 0s

report_leftovers <- function(df) {
  return(sum(is.na(df$note) == TRUE))
}



df <- reset_df()
check <- df[c(31,61,72,115),] #rows with 21 comma sep entries

df$s1[i] <- stringr::str_extract(df$input[i], ",|[0-9]+,") 

stringr:

mbbs_county$checklist_comments <- mbbs_county$checklist_comments %>%
  stringr::str_replace_all("&#61;| =", "=")