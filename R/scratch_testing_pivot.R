###figuring out how to pivot the historical data
#I imagine this will require purrr:map. 

library(tidyr)
library(dplyr)
library(stringr)
library(purrr)
library(beepr)
library(readxl)

#we already have the information about times from the current survey information. What we really need from this is to get the route_num, mbbs_county, and year from the name of the file. Then, we only need the rows that start with a 4 letter bird code eg: use a stringr to look for 4 capital letters. Those are the only rows we want to keep. 

#and the habitat column, at least for the moment, because it helpfully has the 1:20

#'Filters rows where the first column is a species code
#'that is four capital letters
#'@importFrom stringr str_detect
#'@importFrom dplyr filter
#'@param x 
filter_to_species_code_rows <- \(x){
  x %>%
    filter(str_detect(.[[1]], "^[A-Z][A-Z][A-Z][A-Z]$|acsp"))
}

#' Flags if a species code has been missed
#' @param df a dataframe that contains the column "sequence"
#' @importFrom beepr beep
flag_missed_species <- function(df){
  
  if(nrow(df) != max(df$sequence)) {
    beep(11)
    return("Error! nrows does not match expected number of species codes")
  } else {
    print("Passes check. All species accounted for")
  }
  
  return(df)
}

#' Rename the columns of a historical xls
#'@importFrom dplyr %>%
#'@param x a historical dataset that's just been read in. 
rename_columns_hist_xls <- \(x){
  x %>%
    `colnames<-`(c("species_code", "s1","s2","s3","s4","s5","s6","s7","s8","s9","s10","s11","s12","s13","s14","s15","s16","s17","s18","s19", "s20", "sum", "sequence","species_code2", "common_name"))
}

#' Pull mbbs_county, route_num, and year
#' out of a historical MBBS_XL
#' route_num MUST BE the first number in the filename
#' year MUST BE the second number in the filename
#' @importFrom stringr function
#' @importFrom dplyr mutate
extract_county_num_year_from_filename <- function(filename) {
  
  mbbs_county <- str_extract(filename, "[cC]hatham|[dD]urham|[oO]range") %>%
    tolower()
  
  nums <- str_extract_all(filename, "[0-9]+")[[1]]
  route_num <- nums[1] %>% 
    as.numeric()
  year <- nums[2] %>%
    as.numeric()
  
  survey_information <- data.frame(mbbs_county, route_num, year)
  return(survey_information)
}


#' Takes a directory of historical xls
#' files, and processes them,
#' returning a dataframe that contains
#' all the information from that directory
#' broken down to the stop level.
#' @importFrom purrr map
purrr_historical_xls <- function(directory = "inst/extdata/stop_level_data_files/Pippen_Durham_stops") {

  filenames <- list.files(directory) #list directory files
  filenames <- filenames[str_detect(filenames, "\\.xls$")] #keep only .xls files
  filenames <- paste(directory, filenames, sep = "/") #add in directory
  stopsmbbs <- purrr::map_dfr(filenames, process_historical_xls) %>% #process all xls in the directory
    #pivot
    pivot_longer(cols = s1:s20, names_to = "stop_num", names_prefix = "s", values_to = "count")
  
}

#' Process historical excel sheets
#' They should all have the same format,
#' if it is in the wrong format
#' there will be errors
#'@importFrom dplyr %>% mutate relocate select matches mutate_all
#'@importFrom readxl read_excel
#'@importFrom stringr str_extract
process_historical_xls <- function(filenames) {
  
  filename <- str_extract(filenames, "_stops/.*")
  
  hist_xls <- 
    read_excel(filenames,.name_repair = "unique_quiet") %>%
    filter_to_species_code_rows() %>%
    rename_columns_hist_xls() %>%
    mutate(sequence = as.numeric(sequence)) %>% #ensure sequence is number
    dplyr::relocate(common_name, .before = species_code) %>% #readability
    flag_missed_species() %>%
    dplyr::select(common_name, matches("s[0-9]+")) %>% #remove extraneous rows, keep only common name and stops
    mutate(extract_county_num_year_from_filename(filename),
           source = "prepare_stop_level_data, hist xls") %>%
    mutate_all(~ifelse(is.na(.), 0, .)) %>%
    #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! this can be removed once errors are fixed
    mutate_at(c("s1","s2","s3","s4","s5","s6","s7","s8","s9","s10","s11","s12","s13","s14","s15","s16","s17","s18","s19", "s20"), as.character)
  #this function needs something to make sure all the s columns are numeric.
  
  return(hist_xls)
}
