
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

#' Processes the stop level information left in the species_comments column of mbbs rows imported from ebird
#'
#'@importFrom dplyr %>% filter mutate relocate %in% select
#'@importFrom stringr str_replace_all str_starts str_split str_extract 
#'@importFrom tidyr pivot_longer
#'@param mbbs Any mbbs dataset, either the whole survey area or one county
process_species_comments <- function(mbbs) {
  
  #pull rows that have non-blank species_comments
  stopsmbbs <- mbbs %>%
    filter(species_comments != "") 
  
  #fix unicode errors on enter signs, and known typos within the dataset
  stopsmbbs$species_comments <- stopsmbbs$species_comments %>%
    #fix unicode = errors
    str_replace_all("&#61;| =", "=") %>% 
    #fixing specific errors
    str_replace_all("\t", "") %>% #replace tabs with empty string
    str_replace_all("1=-,2=0", "1=0,2=0") %>%
    str_replace_all(",,,,,1,,,,,,,,,,,,,,\t,1", ",,,,,1,,,,,,,,,,,,,,1") %>%
    str_replace_all("1=0,2=013=1,", "1=0,2=0,3=1,") %>%
    str_replace_all("11=1212=0", "11=0,12=0")%>%
    str_replace_all("9=0110=0", "9=1,10=0") %>%
    str_replace_all("4=0.5=", "4=0,5=") %>%
    str_replace_all("7=28=", "7=2,8=") %>%
    str_replace_all("1=12=", "1=1,2=") %>%
    str_replace_all("8=09=", "8=0,9=") %>%
    str_replace_all("10=011=", "10=0,11=") 
    
  #we need to add the rows for imputing the stop information to stopsmbbs
  stopsmbbs <- stopsmbbs %>%
    mutate("s1" = NA, 's2' = NA,'s3' = NA,'s4' = NA,"s5" = NA,"s6" = NA,"s7" = NA,"s8" = NA,"s9" = NA,"s10" = NA,"s11" = NA,"s12" = NA,"s13" = NA,"s14" = NA,"s15" = NA,"s16" = NA,"s17" = NA,"s18" = NA,"s19" = NA,"s20" = NA, 
           "sc_note" = NA) %>%
    relocate(s1:s20, species_comments, count, sc_note) #relocate to be the first columns is necessary b/c later data is added to a given [row,column] based on columns 1:20. Also helpful for temp and error identification

  #set up for-loop to fill in s1:s20 with information from species_commentts
  split_list <- NA; stop <- NA; count <- NA
  #for-loop 
  for(i in 1:nrow(stopsmbbs)) {
    if(is.na(stopsmbbs$species_comments[i]) == TRUE | is.null(stopsmbbs$species_comments[i]) == TRUE) { #if the row is null or is an NA
      stopsmbbs$sc_note[i] <- "ignore, row NA or NULL"
    } else if (stopsmbbs$species_comments[i] == "") {
      stopsmbbs$sc_note[i] <- "ignore, blank row"
    } else if(stringr::str_starts(stopsmbbs$species_comments[i], ",|[0-9]+,") == TRUE) { #follows ,,,,,,1,,,, format
      
      #split the species_comment based on commas
      split_list <- (str_split(stopsmbbs$species_comments[i], ","))[[1]] %>%
        #convert empty strings (nothing between commas) into 0s
        str_replace_all("^$", "0") %>% #"^$" denotes empty string in regex
        #case by case error fixes
        str_replace_all("; another bird seen just before counting at stop 16", "0") %>%
        str_replace_all(" song clearly heard in extensive pines with thinned understory in same habitat and general location where this species has occurred on these censuses for the past 20 years", "0") %>%
        str_replace_all("; singing from trees in grassy field at 35.6383", "0") %>%
        str_replace_all("-79.0035", "0") %>%
        #turn characters to numbers
        as.numeric()
      
      #record how many stops there were for testing. Checking records, there's a 2 cases of 19 stops but after discussion, this represents a minor difference of 1 or 2 birds on either side of a quarter route. OK to leave as is. In all cases where length split_list == 21, the "21st stop" is from a comma with nothing after it. Muscle memory from adding a comma after each stop for 20 stops in a row, leave as it. Three cases of "comma sep, 3" - which represents a pre-dawn owling checklist. These are removed from consideration later on, as they should be handled separately.
      stopsmbbs$sc_note[i] <- paste("comma sep,", length(split_list))
      
      #for each entry in split_list, put it in the associated s#
      #if split_list is short, ie: length 4, other entries after 4 will be NA
      #if split_list is 21, ie: they put a final comma into the dataset at stop 20 ex: 1 vs 1, - we just leave that last 0 out. In the testing dataset, which is just mbbs_orange, all examples with a split_list of length 21 this seemed to be the exact issue. !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!Will need more testing when move to full dataset to confirm no other errors
      for(a in 1:20) {
        stopsmbbs[i,a] <- split_list[a]
      }
      
      #convert NAs to 0 (error where there's a space between commas but no number, gets converted to an NA) or b/c 19 stops, last is NA (2 cases)
      stopsmbbs[i, 1:20][is.na(stopsmbbs[i, 1:20])] <- 0
      
    } else if(stringr::str_starts(stopsmbbs$species_comments[i], "[0-9]+=[0-9]+|st(op)?( )?[0-9]+=[0-9]+") == TRUE) { #
      #split the species_comments based on , or ;
      split_list <- str_split(stopsmbbs$species_comments[i], ",|;")[[1]] %>%
        #if it starts with st we're going to remove the sts
        str_replace_all("( )?st(op)?( )?", "") 
      
      stopsmbbs$sc_note[i] <- paste("X=X,",length(split_list))
      
      #for each entry in split_list, use the start of the entry for what stop, and the end of the entry for what count
      for(a in 1:length(split_list)) {
        stop <- as.numeric(str_extract(split_list[a], "[0-9]+")) #extracts the first set of numbers
        count <- as.numeric(str_extract(split_list[a], "(?<=\\=)[0-9]+")) #extracts a set of numbers after an equal sign 
        stopsmbbs[i,stop] <- count 
      }
      #convert NAs to 0
      stopsmbbs[i, 1:20][is.na(stopsmbbs[i, 1:20])] <- 0
    } else if(stringr::str_starts(stopsmbbs$species_comments[i], "Stop(s)?( )?(=)?(:)?") == TRUE) { #starts with Stops: or Stops = 
      #split the species_comments based on ,
      split_list <- str_split(stopsmbbs$species_comments[i], ",")[[1]] %>%
        #remove the Stops = / Stops:
        str_replace_all("Stop(s)?( )?(=)?(:)?( )?", "")
      
      stopsmbbs$sc_note[i] <- paste("S:/=,",length(split_list))
      
      for(a in 1:20) {
        stopsmbbs[i,a] <- as.numeric(split_list[a])  
      } #end 1:20 for loop
    } else if(stringr::str_starts(stopsmbbs$species_comments[i], "st(op)?") == TRUE) { #when gives ie: Stop 6 and the count for that stop is just the count for the whole route 
      
      stop <- stopsmbbs$species_comments[i] %>%
        #remove anything after a comma (ie: descriptive notes) if there is a comma
        str_extract(".*(,)?") %>%
        #extract numbers
        str_extract("[0-9]([0-9])?")
      
      #give that stop the count that's already within that row, and set all the other stops to 0
      stopsmbbs[i,as.numeric(stop)] <- stopsmbbs[i,22] #($count)
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
  
} #end function

#' Function to return if any rows of stopsmbbs have not been accounted for. Used for testing
psc_report_leftovers <- function(df) {
  return(sum(is.na(df$sc_note) == TRUE))
}


