
#' Takes a file directory that contains
#' Folders of historical .xls
#' and processes them all into one df
#' @importFrom purrr map_dfr
#' @param directory the mbbs directory that has folders containing historical xls files. 
hist_xls_purrr_directories <- function(directory = "inst/extdata/stop_level_data"){

  filenames <- list.files(directory)  #get list of folders in directory
  filenames <- filenames[str_detect(filenames,"\\_stops$")] #only keep folders ending in _stops
  filenames <- paste(directory,filenames,sep="/") #add in the rest of the file path
  stopsmbbs <- purrr::map_dfr(filenames, hist_xls_purrr_files_in_directory)
  
  return(stopsmbbs)
}

#' Takes a file directory of historical .xls
#' files, and processes them,
#' returning a dataframe that contains
#' all the information from that directory's files
#' broken down to the stop level.
#' @importFrom purrr map_dfr
#' @importFrom stringr str_detect
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr %>%
#' @param directory a directory that has folders containing historical xls files. Has a default for testing purposes
hist_xls_purrr_files_in_directory <- function(directory = "inst/extdata/stop_level_data/Pippen_Durham_stops") {
  
  filenames <- list.files(directory) #list directory files
  filenames <- filenames[str_detect(filenames, "\\.xls$")] #keep only .xls files
  filenames <- paste(directory, filenames, sep = "/") #add in the rest of the path
  stopsmbbs <- purrr::map_dfr(filenames, hist_xls_process_xls) %>% #process all xls in the directory
    #pivot
    pivot_longer(cols = s1:s20, 
                 names_to = "stop_num", names_prefix = "s", values_to = "count") %>%
    mutate(stop_num = as.integer(stop_num)) 
}


#' Corrects species common_names to 
#' taxonomic standard
#' @param x a historical .xls
#' @importFrom stringr str_replace_all
hist_xls_correct_common_names <- \(x){
  x <- x %>%
    mutate(common_name = str_replace(common_name, "Rock Dove", "Rock Pigeon"),
           common_name = str_replace(common_name, "^Whip-poor-will$", "Eastern Whip-poor-will"))
}


#' Process historical excel sheets
#' They should all have the same format,
#' if it is in the wrong format
#' there will be errors
#'@importFrom dplyr %>% mutate relocate select matches mutate_all mutate_at
#'@importFrom readxl read_excel
#'@importFrom stringr str_extract
#'@param filenames  a list of historical mbbs .xls files within a folder
hist_xls_process_xls <- function(filenames) {
  
  filename <- str_extract(filenames, "_stops/.*")
  
  hist_xls <- 
    read_excel(filenames,.name_repair = "unique_quiet") %>%
    hist_xls_filter_to_species_code_rows() %>%
    hist_xls_rename_columns() %>%
    mutate(sequence = as.integer(sequence)) %>% #ensure sequence is number
    dplyr::relocate(common_name, .before = species_code) %>% #readability
    hist_xls_correct_common_names() %>%
    hist_xls_flag_missed_species() %>%
    dplyr::select(common_name, matches("s[0-9]+")) %>% #remove extraneous rows, keep only common name and stops
    mutate(hist_xls_extract_county_num_year_from_filename(filename),
           source = "prep_sld, hist xls",
           route_num = as.integer(route_num)) %>%
    mutate_all(~ifelse(is.na(.), 0, .)) %>% #change NAs to 0
    hist_xls_change_xs_to_ones() %>% 
    #ensure counts are numeric
    mutate_at(c("s1","s2","s3","s4","s5","s6","s7","s8","s9","s10","s11","s12","s13","s14","s15","s16","s17","s18","s19", "s20"), as.integer) %>%
    hist_xls_match_mbbs_format()
  
  return(hist_xls)
}

#'Filters to only rows where the first column is a species code
#'that is four capital letters
#'or acsp - "Accipiter sp."
#'@importFrom stringr str_detect
#'@importFrom dplyr filter
#'@param x a historical .xls 
hist_xls_filter_to_species_code_rows <- \(x){
  x %>%
    filter(str_detect(.[[1]], "^[A-Z][A-Z][A-Z][A-Z]$|acsp"))
}


#' Rename the columns of a historical xls
#'@importFrom dplyr %>%
#'@param x a historical .xls 
hist_xls_rename_columns <- \(x){
  x %>%
    `colnames<-`(c("species_code", "s1","s2","s3","s4","s5","s6","s7","s8","s9","s10","s11","s12","s13","s14","s15","s16","s17","s18","s19", "s20", "sum", "sequence","species_code2", "common_name"))
}

#' Flags if a species code has been missed
#' @param df a dataframe that contains the column "sequence"
hist_xls_flag_missed_species <- function(df){
  
  if(nrow(df) != max(df$sequence)) {
    return("Error! nrows does not match expected number of species codes")
  } 
  
  return(df)
}


#' Pull mbbs_county, route_num, and year
#' out of a historical MBBS_XL
#' route_num MUST BE the first number in the filename
#' year MUST BE the second number in the filename
#' @importFrom stringr str_extract str_extract_all
#' @param filename a filename that contains the county, route num, and year
hist_xls_extract_county_num_year_from_filename <- function(filename) {
  
  mbbs_county <- str_extract(filename, "[cC]hatham|[dD]urham|[oO]range") %>%
    tolower()
  
  nums <- str_extract_all(filename, "[0-9]+")[[1]]
  route_num <- nums[1] %>% 
    as.integer()
  year <- nums[2] %>%
    as.integer()
  
  survey_information <- data.frame(mbbs_county, route_num, year)
  return(survey_information)
}


#'Converts 'x' and 'X' counts to 1
#'following method of mbbs route information
#'as compiled by Haven Wiley
#'@importFrom purrr map_dfr
#'@importFrom dplyr %>%
#'@param hist_xls a historical mbbs excel file
hist_xls_change_xs_to_ones <- function(hist_xls) {
  hist_xls <- hist_xls %>%
  purrr::map_dfr(., ~ifelse(.=="x",1,.)) %>%
  purrr::map_dfr(., ~ifelse(.=="X",1,.))
}



#' Match MMBS format
#' This doesn't add every mbbs column
#' But fills in those for species or route info
#' that are expected
#' @importFrom dplyr left_join select
#' @param hist_xls a historical mbbs excel file
#' @param mbbs_survey_events rda of all surveys that have been run on the mbbs
hist_xls_match_mbbs_format <- function(hist_xls, mbbs_survey_events = "data/mbbs_survey_events.rda"){
  
  #bring in other dfs
  taxonomy <- get_ebird_taxonomy()
  load(mbbs_survey_events)
  mbbs_survey_events <- mbbs_survey_events %>%
    dplyr::select(mbbs_county, route_num, year, observers) 
  
  #add columns
  hist_xls <- hist_xls %>%
    left_join(taxonomy, by = "common_name") %>%
    mbbs_generate_route_ID() %>%
    left_join(mbbs_survey_events, by = c("mbbs_county", "route_num", "year"))
  
  return(hist_xls)
}


