#
# WIP!
#


#' Replaces mbbs route-level data with the more granular stop-level information 
#' when available.
#' Works with any mbbs dataset, either the whole mbbs or one county.
#'@importFrom dplyr %>%
#'@importFrom beepr beep
#'@param mbbs Any mbbs dataset, either the whole survey area or one county
granulate_to_stop <- function(mbbs) {
  
  #is this function name a little silly? yes hehe! Also specific
  print("This function takes a minute to run. Listen for a beep when it's done.")
  
  #maybe print that this takes a minute to run. could use a beepr
  #get information from routes with stop info from species_comments
  
  process_species_comments(mbbs)
  #get information from routes with stop info in checklist notes (there's at least one) (add info to that df extracted from when information is by quarter route in notes)
  #get information from the transcription work datasets
  #read in files that end in paper_files.csv, won't have to change that when final file comes in
  #get information from the extdata/stop_level_data_files
  #compile and compare to current mbbs routes - do counts match?
  #where counts DONT match, have .csv where that decision is recorded, and correct one way or another. Usually should default to the starting route-level mbbs. At the least check mbbs$source to see where error is
  #where counts DO match, replace route-level information with stop information in the overall mbbs/mbbs_county dataset that this function is given. mbbs$source == "granulated to stop"
  #add in the stop num = 1 for routes that have all 19 stops aside from stop 1 and have 20 records for the route (like, within a species.)
  beep()
}

#' Fix species comments
#' 
#' Fix unicode errors on enter signs,
#' and known typos within the dataset
#' @param x a character vector of species comments
#' @importFrom stringr str_replace_all
fix_species_comments <- \(x) {
  x %>%
    #fix unicode = errors
    str_replace_all("&#61;| =", "=") %>% 
    #convert "Stops=" and "Stops:" to format where commas separate counts at each stop
    str_replace_all("Stop(s)?( )?(=)?(:)?( )?", "") %>%
    #replace tabs with empty string
    str_replace_all("\t", "") %>%
    #fixing specific errors
    str_replace_all(c(
      "1=-,2=0" = "1=0,2=0",
      ",,,,,1,,,,,,,,,,,,,,\t,1" = ",,,,,1,,,,,,,,,,,,,,1",
      "1=0,2=013=1," = "1=0,2=0,3=1,",
      "11=1212=0" = "11=0,12=0",
      "9=0110=0" = "9=1,10=0",
      "4=0.5=" = "4=0,5=",
      "7=28=" = "7=2,8=",
      "1=12=" = "1=1,2=",
      "8=09=" = "8=0,9=",
      "10=011=" = "10=0,11=",
      "; another bird seen just before counting at stop 16" = "",
      " song clearly heard in extensive pines with thinned understory in same habitat and general location where this species has occurred on these censuses for the past 20 years" = "",
      "; singing from trees in grassy field at 35.6383,-79.0035" = "",
      "(stop 3, )?heard at 3rd stop( )?-- it was hard to mistake." = "stop 3",
      "18 were perched at 14 on power lines" = "",
      "This is a breeding bird survey route with 20 stops." = "",
      "including 3 chasing each other!" = "",
      "this was over 10 miles on traveling count" = "",
      "over a 10 mile area" = "",
      "I had 2 additional vireos that I thought sounded like.*" = "",
      "3 white geese or ducks, walking.*" = "",
      "3 hens with 12 young birds!.*" = "",
      "Singing from field on south side of road, heard.*" = "",
      "Nesting in Eastern chimney of 1407 Baptist road" = "",
      "2 adults 5 young" = "",
      "Counted 2 groups of 5 with a couple additional birds" = "",
      "All 8 were roosting on powerline" = "",
      "One Horned Lark singing and walking.*" = "",
      "22 on wire at stop 17 (new school)" = "",
      "flying and twittering at 20" = "st 20",
      "heard at stop 17 on Mann's Chapel Road" = "st 17",
      "One flyover flock.*" = "",
      "Durham Mini Breeding Bird Survey, 20 stops" = ""
      )) 
}

#' Process a single species comment
#' 
#' Convert empty strings to 0s,
#' remove "st", "stop", "Stops =" 
#' @param x a (scalar) string
#' @param count an integer
#' @return an integer vector of length 20
#' @importFrom stringr str_replace_all
process_comment <- \(x, count){

  #ensure x is of length 1, count is number
  assertthat::assert_that(
    rlang::is_scalar_character(x),
    is.numeric(count)
  )
  
  x %>%
    case_when(
      str_starts(",|[0-9]+,") ~ sp_com_comma_seperated(x, count),
      str_starts("[0-9]+=[0-9]+|st(op)?( )?[0-9]+=[0-9]+") ~ sp_com_equals_seperated(x, count),
      str_starts("st(op)?") ~ sp_com_only_stop(x, count)
    )
  
  stopsmbbs %>%
    case_when(
      str_starts(species_comments, ",|[0-9]+,") == TRUE ~
        sp_com_comma_seperated(species_comments, count),
      str_starts(species_comments,"[0-9]+=[0-9]+|st(op)?( )?[0-9]+=[0-9]+") == TRUE ~
        sp_com_equals_seperated(species_comments, count),
      str_starts(species_comments,"st(op)?") == TRUE ~ 
        sp_com_only_stop(species_comments, count)
    )
  
  # x <- x %>%
  #   case_when(
  #     str_starts(",|[0-9]+,") ~ function1(X),
  #     str_starts("[0-9]+=[0-9]+|st(op)?( )?[0-9]+=[0-9]+") ~ function2(x),
  #     str_starts("st(op)?") ~ function3(x, count))
  # 
  x %>%
    stringr::str_split_1(",") %>%
    str_replace_all(
      c( # Convert empty strings 0
        "^$" = "0",
        # Remove the Stops = / Stops:
        "Stop(s)?( )?(=)?(:)?( )?" = "",
        # If it starts with st, drop
        "( )?st(op)?( )?" = ""
        )
    ) %>%
    # additional step go here
    (\(out) {
      assertthat::assert_that(
        length(out) == 20
        #additional checks go here.
      )
      as.integer(out)
    })()
}

#' Use when species comments follows ,,,,,,1,,,, format
#' eg. where commas seperate counts at each stop. 
#' Takes a string and returns a list of 20 numbers
#' @param x a string from the species_comment column formatted with commas
#' @param count an integer from the count column of the mbbs
sp_com_comma_seperated <- function(x, count = -99) {
  
  assertthat::assert_that(
    str_detect(x, ",") # check has commas as format requires
  )
  
  # split based on commas
  x %>% 
    stringr::str_split_1(",") %>%
    str_replace_all(
      c( # Convert empty strings 0
        "^$" = "0",
        # Remove the Stops = / Stops:
        "Stop(s)?( )?(=)?(:)?( )?" = "",
        # If it starts with st, drop
        "( )?st(op)?( )?" = ""
      )) %>%
  # make standard list of 20
  # 2 cases of 19 stops - minor dif in birds on either side of quarter route
  # leave as is.
  # Cases with 21st stop where it comes from extra comma after 20th stop
    pad_or_truncate() %>%
    as.integer() %>%
    (\(out) {
      assertthat::assert_that(
        length(out) == 20,
        sum(out) == count
        #additional checks go here.
      )
      out # return
    })
  
}

#' Use when species comments follows 1=5 or st 1=5 to stop 1=5 format
#' Takes a string and returns a list of 20 numbers
#' @param x a string from the species_comment column formatted by "stop=count"
#' @param count an integer from the count column of the mbbs
#' @importFrom stringr str_split_1 str_detect str_replace_all
#' @importFrom assertthat assert_that
#' @returns out a standardized list of 20 integers, representing the 
#'    species count at 20 stops.
sp_com_stop_equals_count <- function(x, count = -99) {
  
  assertthat::assert_that(
    str_detect(x, "=") # check has equals as format requires
  )
  
  # split based , or ;
  x %>% 
    stringr::str_split_1(",|;") %>%
    str_replace_all(
      c( # Convert empty strings 0
        "^$" = "0",
        # Remove the Stops = / Stops:
        "Stop(s)?( )?(=)?(:)?( )?" = "",
        # If it starts with st, drop
        "( )?st(op)?( )?" = ""
      )) %>%
    # make standard list of 20
    standardize_stops_equals() %>%
    (\(out) {
      assertthat::assert_that(
        length(out) == 20,
        sum(out) == count
        #additional checks go here.
      )
      out # return
    })
}


#' Use when species comment is ie: "Stop 6"
#' The count for that stop is the count for the whole route
#' and the species was not seen at any other stops
#' @param x a string from the species_comment column formatted by "stop=count"
#' @param count an integer from the count column of the mbbs
#' @importFrom stringr str_count
sp_com_only_stop <- function(x, count = -99) {
  
  assertthat::assert_that(
    # check there's only one number, up to two digits
    str_count(x, "[0-9]([0-9])?") == 1,
    # count has been given
    count > 0 
  )
  
  x %>%
    # remove anything after a comma (ie: descriptive notes)
    str_extract(".*(,)?") %>%
    # extract numbers
    str_extract("[0-9]([0-9])?") %>%
    as.numeric() %>%
    # make standard list of 20
    paste("=", count, sep = "") %>%
    standardize_stops_equals() %>%
    (\(out) {
      assertthat::assert_that(
        length(out) == 20,
        sum(out) == count
        # additional checks go here.
      )
      out # return
    })
}

#' Prepare mbbs dataset for processing species comments
#' @importFrom dplyr filter mutate bind_cols tibble relocate
#' @importFrom rlang set_names
#' @importFrom stringr str_detect
prepare_to_process <- \(mbbs) {
  mbbs %>%
    # Keep rows that have non-blank species_comments
    filter(species_comments != "") %>%
    # Keep rows that aren't already separated by stop
    filter(is.na(stop_num)) %>%
    # Keep rows that contain at least one number
    filter(str_detect(species_comments, "[0-9]")) %>%
    # Remove pre-dawn owling checklist
    filter(sub_id != "S46296942") %>%
    
    mutate(species_comments = fix_species_comments(species_comments)) %>%
    # Remove rows that were changed to have blank species_comments
    filter(species_comments != "") %>%
    dplyr::bind_cols(
      dplyr::tibble(
        !!! rlang::set_names(rep(NA_character_, 21), 
                             c(paste0("s", 1:20), "sc_note"))
      )
    ) %>%
    # relocate to be the first columns is necessary b/c
    # later data is added to a given [row,column] based on columns 1:20. 
    # Also helpful for temp and error identification
    relocate(s1:s20, species_comments, count, sc_note)
}




#'
######should this x = species_comment be... x = .$species_comment ? same goes for count. That way it's set as default to take those columns from what's passed to it?
#########If we can figure out a way to handle species comments where it only has the stop, and then count represents the count at that stop, eg species_comments = "Stop 6" in a different function, then *this* function only needs to take species_comments, and not species_comments AND count.
# convert_species_comments_to_list <- function(x = species_comments, count) {
#   
#   #species comments follows ,,,,,,1,,,, format where commas separate counts 
#   #at each stop
#   if(str_starts(x, ",|[0-9]+,")) {
#     
#     #split based on commas
#     split_list <- str_split(x, ",")[[1]] %>%
#       fix_split_species_comments() %>%
#       as.integer()
#     
#     #record n stops for testing
#     #2 cases of 19 stops - represents minor dif of 1 or 2 birds on either side
#     #of a quarter route. OK to leave as is
#     #21 stops - all cases where 21st stop is from an extra comma after 20th stop.
#     #Trim
#     #3 cases of 3 stops - represents pre-dawn ownling checklists.
#     #Remove from considerationg as they should be handled seperately.
#     sc_note <- paste("comma sep,", length(split_list))
#     
#     #make standard list of 20
#     split_list <- pad_or_truncate(split_list)
#     
#   } else if(str_starts(x, "[0-9]+=[0-9]+|st(op)?( )?[0-9]+=[0-9]+")) { #species comments follows 1=5 or st1=5 or stop 1 = 5 format
#     
#     #split species_comment based on , or ;
#     split_list <- str_split(x, ",|;")[[1]] %>%
#       fix_split_species_comments() 
#     
#     #add note
#     sc_note <- paste("X=X", length(split_list))
#     
#     #standardize list to 20
#     split_list <- standardize_stops_equals(split_list)
#     
#   } else if(str_starts(x, "st(op)?")) { #species comment is ie: "Stop 6", count for that stop is count for the whole route.
#     ####If we can figure out a way to handle this section in a different function, then *this* function only needs to take species_comments, and not species_comments AND count. 
#     
#     #extract the stop
#     stop <- x %>%
#       #remove anything after a comma (ie: descriptive notes)
#       str_extract(".*(,)?") %>%
#       #extract numbers
#       str_extract("[0-9]([0-9])?") %>%
#       as.numeric()
#     
#     #standardize list to 20
#     split_list <- paste(stop, "=", count, sep = "") %>%
#       standardize_stops_equals()
#     
#     #add note
#     sc_note <- paste("one st")
#   }
#   
#   split_list
#   ########sc_note is really only needed to remove pre-owling checklists. Otherwise, those get added erroenously as routes with 20 stops...Probably the solution is to remove pre-owling checklists BEFORE. I've drafted another function to do this below, but it's not working. 
# }

#'
#'
#'
direct_to_sp_com_function <- function(species_comments, count) {
  
  #route species comments to the relevent function to process it.
  if(str_starts(species_comments, ",|[0-9]+,")) {
    
    stop_counts_list <-
    sp_com_comma_seperated(species_comments, count)
    
  } else if (str_starts(species_comments, "[0-9]+=[0-9]+|st(op)?( )?[0-9]+=[0-9]+")) {
    
    stop_counts_list <- 
    sp_com_stop_equals_count(species_comments, count)
    
  } else if (str_starts(species_comments, "st(op)?")) {
    
    stop_counts_list <- 
    sp_com_only_stop(species_comments, count)
    
  }
  
  stop_counts_list #return
}


#' Process species comments
#' 
#' 
process_species_comments <- function(mbbs) {
  
  
  #Oh, I think I was thinking in terms of list processing... start with a function that takes a single comment and creates a list of the information for the 20 stops (this list should have a length of 20). And then run that function on each comment (i.e. each row). And then reshape the result from a list of lists to a data.frame.
  
  stopsmbbs <- 
    mbbs %>%
    ungroup %>% #need to open a new issue for this. 
    prepare_to_process() 

  #figure out a way to purr:map this or otherwise, get a list of the lists! Multiple ways explored below that aren't working properly 
  direct_to_sp_com_function(stopsmbbs$species_comments, stopsmbbs$count)
  
  stopsmbbs %>%
  purrr::map(
    .f = purr_species_comments(species_comments, count)
  )
  #returns a 0x0 tibble
  
  
  #oohhh so beautiful, so not functional. But, the problem here is inherent in using across.....that's not what I want. I want to mutate SPECIES_COMMENTS into 20 columns.
c <-  stopsmbbs %>%
    mutate(
      across(.cols = starts_with("s") & ends_with("[0-9]([0-9])?"),
             .fns = ~case_when(
                 str_starts(species_comments, "[0-9]+=[0-9]+|st(op)?( )?[0-9]+=[0-9]+") ~
                   sp_com_stop_equals_count(species_comments, count),
                 str_starts(species_comments, ",|[0-9]+,") ~ 
                   sp_com_comma_seperated(species_comments, count),
                 str_starts(species_comments, "st(op)?") ~
                   sp_com_only_stop(species_comments, count)
                 )
             )
      )
  
  
  #if I were returning a dataset.... it would include the species_comments, count, and then the list of s1:s20, which might as well be columns individually. And then I could just left_join that back to stopsmbbs?
  
 
  purrr:map_df()
  stopsmbbs <- prepare_to_process(mbbs)
  
  # x <- x %>%
  #   case_when(
  #     str_starts(",|[0-9]+,") ~ function1(X),
  #     str_starts("[0-9]+=[0-9]+|st(op)?( )?[0-9]+=[0-9]+") ~ function2(x),
  #     str_starts("st(op)?") ~ function3(x, count))
  # 
  
}




#' Process species comments
#' 
#' Processes the stop level information 
#' left in the species_comments column of mbbs rows imported from ebird
#'
#' @importFrom dplyr %>% filter mutate relocate select
#' @importFrom stringr str_replace_all str_starts str_split str_extract 
#' @importFrom tidyr pivot_longer
#' @param mbbs Any mbbs dataset, either the whole survey area or one county
#' @result data.frame of mbbs data that had an informative species_comments 
#'         split out down to the stop level
process_species_comments <- function(mbbs) {
  
  stopsmbbs <- prepare_to_process(mbbs)
  
  #pipe species_comments to the different sp_com paths based on matching patterns.
  
  #set up for-loop to fill in s1:s20 with information from species_commentts
  split_list <- stop <- count <- NA
  
  for(i in 1:nrow(stopsmbbs)) {
    if(stringr::str_starts(stopsmbbs$species_comments[i], ",|[0-9]+,") == TRUE) { #follows ,,,,,,1,,,, format
      
      #split the species_comment based on commas
      split_list <- (str_split(stopsmbbs$species_comments[i], ","))[[1]] %>%
        fix_split_species_comments() %>%
        #turn characters to numbers
        as.integer()
      
      #record how many stops there were for testing. Checking records, there's a 2 cases of 19 stops but after discussion, this represents a minor difference of 1 or 2 birds on either side of a quarter route. OK to leave as is. In all cases where length split_list == 21, the "21st stop" is from a comma with nothing after it. Muscle memory from adding a comma after each stop for 20 stops in a row, leave as it. Three cases of "comma sep, 3" - which represents a pre-dawn owling checklist. These are removed from consideration later on, as they should be handled separately.
      stopsmbbs$sc_note[i] <- paste("comma sep,", length(split_list))
      
      #for each entry in split_list, put it in the associated s#
      #if split_list is short, ie: length 4, other entries after 4 will be NA
      #if split_list is 21, ie: they put a final comma into the dataset at stop 20 ex: 1 vs 1, - we just leave that last 0 out. In the testing dataset, which is just mbbs_orange, all examples with a split_list of length 21 this seemed to be the exact issue. 
      for(a in 1:20) {
        stopsmbbs[i,a] <- split_list[a]
      }
      
      #convert NAs to 0 (error where there's a space between commas but no number, gets converted to an NA) or b/c 19 stops, last is NA (2 cases)
      stopsmbbs[i, 1:20][is.na(stopsmbbs[i, 1:20])] <- 0
      
    } else if(stringr::str_starts(stopsmbbs$species_comments[i], "[0-9]+=[0-9]+|st(op)?( )?[0-9]+=[0-9]+") == TRUE) { #
      #split the species_comments based on , or ;
      split_list <- str_split(stopsmbbs$species_comments[i], ",|;")[[1]] %>%
        fix_split_species_comments()
      
      stopsmbbs$sc_note[i] <- paste("X=X,",length(split_list))
      
      #for each entry in split_list, use the start of the entry for what stop, and the end of the entry for what count
      for(a in 1:length(split_list)) {
        stop <- as.integer(str_extract(split_list[a], "[0-9]+")) #extracts the first set of numbers
        count <- as.integer(str_extract(split_list[a], "(?<=\\=)[0-9]+")) #extracts a set of numbers after an equal sign 
        stopsmbbs[i,stop] <- count 
      }
      #convert NAs to 0
      stopsmbbs[i, 1:20][is.na(stopsmbbs[i, 1:20])] <- 0
    } else if(stringr::str_starts(stopsmbbs$species_comments[i], "Stop(s)?( )?(=)?(:)?") == TRUE) { #starts with Stops: or Stops = 
      #split the species_comments based on ,
      split_list <- str_split(stopsmbbs$species_comments[i], ",")[[1]] %>%
        fix_split_species_comments()
      
      stopsmbbs$sc_note[i] <- paste("S:/=,",length(split_list))
      
      for(a in 1:20) {
        stopsmbbs[i,a] <- as.integer(split_list[a])  
      } #end 1:20 for loop
    } else if(stringr::str_starts(stopsmbbs$species_comments[i], "st(op)?") == TRUE) { #when gives ie: Stop 6 and the count for that stop is just the count for the whole route 
      
      stop <- stopsmbbs$species_comments[i] %>%
        #remove anything after a comma (ie: descriptive notes) if there is a comma
        str_extract(".*(,)?") %>%
        #extract numbers
        str_extract("[0-9]([0-9])?")
      
      #give that stop the count that's already within that row, and set all the other stops to 0
      stopsmbbs[i,as.integer(stop)] <- stopsmbbs[i,22] #($count)
      stopsmbbs[i, 1:20][is.na(stopsmbbs[i, 1:20])] <- 0
      
      stopsmbbs$sc_note[i] <- "a_ one st no ="
      
    } else {
      #There's been thorough checks and testing throughout the process of building this function and the list of mbbs rows that are passed to this function is stable. leftover rows should be ignored.
      stopsmbbs$sc_note[i] <- "ignore"
    }#end final else if statement
  } #end overall for loop going through every stopsmbbs row.
  
  #confirm sc_notes that include "ignore" should be ignored, and don't have stop information.
  #check <- stopsmbbs %>% filter(sc_note == "ignore")
  #View(check)
  #remove rows where sc_note starts with "ignore"
  stopsmbbs <- stopsmbbs %>% 
    filter(!(sc_note %in% c("ignore", "ignore, row NA or NULL", "ignore, blank row"))) %>%
    #remove rows that are pre-dawn owling checklists
    filter(!(sc_note %in% c("comma sep, 3"))) %>%
    #mark that these routes came from this function to help with tracing any errors
    mutate(source = "stop level functions, psc") %>%
    #now..pivot! 
    pivot_longer(cols = s1:s20, names_to = "stop_num_psc", names_prefix = "s", values_to = "count_psc") %>%
    #mutate count and stop_num with the new values
    mutate(count = count_psc, 
           stop_num = stop_num_psc) %>%
    #remove extraneous rows and rearrange columns back to match what's typical for mbbs
    select(-sc_note, -stop_num_psc, -count_psc) %>%
    relocate(species_comments, .after=checklist_comments) %>%
    relocate(count, .after = notes)
  
  return(stopsmbbs)
  
}


#' takes a list and either extends it with 0s to the max_length or cuts it down
#' @importFrom assertthat assert_that
pad_or_truncate <- \(x, max_length = 20) {
  #pad with 0s if less than 20
  pad <- min(length(x), max_length)
  x <- c(x, rep(as.integer(0), max_length - pad))
  #truncate if longer than 20
  x <- x[1:max_length]
  
  #confirm no errors
  assertthat::assert_that(
    length(x) == 20
  )
  
  x #return
}


#' takes a list of [0-9]=[0=9] and returns a list of length 20
#' where the first number becomes the index in the list
#' and the second number becomes the value of the list at that index.
#' Indexes not explicitly changed in the list of 20 are set to have value '0'
#' @importFrom stringr str_extract
standardize_stops_equals <- \(x){
  #make standard list of 20 
  #we create a temp as some stops may not have been included when the count was 0
  temp_list <- rep(as.integer(0), 20)
  
  #fill in list of 20
  #for each entry in the list, the start is the stop and end is the count
  for(a in 1:length(x)){
    stop <- as.integer(str_extract(x[a], "[0-9]+")) #first set of nums
    count <- as.integer(str_extract(x[a], "(?<=\\=)( )?[0-9]+")) #nums after =
    temp_list[stop] <- count
  }
  #re-assign x
  x <- temp_list
  
  assertthat::assert_that(
    length(x) == 20,
    is.integer(x)
  )
  
  #return
  x
}