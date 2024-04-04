#
# top-of-script function should:
# -> generate df of routes with stop info by step from species_comments
# -> add info to that df extracted from when information is by quarter route in notes
# -> add to that info the dataset from the transcription work
# -> add the stop num 1 in for routes that have all the 19 stops aside from stop 1. 
# -> Allen's proposal, to maintain the highest level of granuarity in the one mbbs dataset, is to replace entries in the mbbs with stop_level information where possible
# then, when generating the two kinda 'end-user' datasets, you're really using both functions (this happens anyway) where you summarize by route, and where you extract just the information that has stop number information
# Allen's proposal avoids what my solution would not: disagreements between the mbbs by route and mbbs by stop - here, they HAVE to agree to pass the qc check. I'm manually approving them to go forward. 
# So yes, Allen's proposal where then information is granularized in mbbs when matches agree, and report when they don't. Depending on how many issues there are, either manually resolve this incongruences or discard one part of the information. 

###code below comes from the scratch file, and needs to be modified to take the entire dataset rather than just a subset of columns. Things need renaming, to be turned into a function, etc. We ought to write it to work using either just one county's mbbs or the mbbs as a whole
load("data/mbbs_orange.rda")
load("data/mbbs_durham.rda")
load("data/mbbs_chatham.rda")
load("data/mbbs.rda")
library(stringr)
library(dplyr)


#test <- unique(mbbs_chatham$species_comments)
test <- unique(mbbs$species_comments)
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
  stringr::str_replace_all("&#61;| =", "=") %>%
  #fixing specific errors
  str_replace_all("1=-,2=0", "1=0,2=0") %>%
  str_replace_all(",,,,,1,,,,,,,,,,,,,,\t,1", ",,,,,1,,,,,,,,,,,,,,1") %>%
  str_replace_all("1=0,2=013=1,", "1=0,2=0,3=1,") %>%
  str_replace_all("11=1212=0", "11=0,12=0") %>%
  str_replace_all("9=0110=0", "9=1,10=0") ##double check with the count if this should be a 1 or a 0. You need to move to the full dataset filtering anyway. So that way you can keep the species name, year, and original count and then solve more problems better. 


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
  if(is.na(df$input[i]) == TRUE | is.null(df$input[i]) == TRUE) { #if the row is null or is an NA
    df$note[i] <- "row NA or NULL"
  } else if (df$input[i] == "") {
    df$note[i] <- "blank row"
  } else if(stringr::str_starts(df$input[i], ",|[0-9]+,") == TRUE) { #follows ,,,,,,1,,,, format
    
    #split the species_comment based on commas
    split_list <- (str_split(df$input[i], ","))[[1]] %>%
      #convert empty strings (nothing between commas) into 0s
      str_replace_all("^$", "0") %>% #"^$" denotes empty string in regex
      #case by case error fixes
      str_replace_all("; another bird seen just before counting at stop 16", "0") %>%
      str_replace_all(" song clearly heard in extensive pines with thinned understory in same habitat and general location where this species has occurred on these censuses for the past 20 years", "0") %>%
      str_replace_all("; singing from trees in grassy field at 35.6383", "0") %>%
      str_replace_all("-79.0035", "0") %>%
      #remove all spaces
      str_replace_all(" ", "") %>%
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

#if note starts with "comma sep" or any of the other relevant notes, then the NAs should be converted to 0s. 

report_leftovers <- function(df) {
  return(sum(is.na(df$note) == TRUE))
}



df <- reset_df()