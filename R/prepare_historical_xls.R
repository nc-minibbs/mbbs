#' Takes a file directory that contains
#' Folders of historical .xls
#' and processes them all into one df
#' @importFrom purrr map_dfr
hist_xls_purrr_directories <- function(directory = "inst/extdata/stop_level_data_files"){

  filenames <- list.files(dir)  #get list of folders in directory
  filenames <- filenames[str_detect(filenames,"\\_stops$")] #only keep folders ending in _stops
  filenames <- paste(dir,filenames,sep="/") #add in the rest of the file path
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
hist_xls_purrr_files_in_directory <- function(directory = "inst/extdata/stop_level_data_files/Pippen_Durham_stops") {
  
  filenames <- list.files(directory) #list directory files
  filenames <- filenames[str_detect(filenames, "\\.xls$")] #keep only .xls files
  filenames <- paste(directory, filenames, sep = "/") #add in the rest of the path
  stopsmbbs <- purrr::map_dfr(filenames, hist_xls_process_xls) %>% #process all xls in the directory
    #pivot
    pivot_longer(cols = s1:s20, 
                 names_to = "stop_num", names_prefix = "s", values_to = "count") 
}


#' Process historical excel sheets
#' They should all have the same format,
#' if it is in the wrong format
#' there will be errors
#'@importFrom dplyr %>% mutate relocate select matches mutate_all mutate_at
#'@importFrom readxl read_excel
#'@importFrom stringr str_extract
hist_xls_process_xls <- function(filenames) {
  
  filename <- str_extract(filenames, "_stops/.*")
  
  hist_xls <- 
    read_excel(filenames,.name_repair = "unique_quiet") %>%
    hist_xls_filter_to_species_code_rows() %>%
    hist_xls_rename_columns() %>%
    mutate(sequence = as.numeric(sequence)) %>% #ensure sequence is number
    dplyr::relocate(common_name, .before = species_code) %>% #readability
    hist_xls_flag_missed_species() %>%
    dplyr::select(common_name, matches("s[0-9]+")) %>% #remove extraneous rows, keep only common name and stops
    mutate(hist_xls_extract_county_num_year_from_filename(filename),
           source = "prep_sld, hist xls") %>%
    mutate_all(~ifelse(is.na(.), 0, .)) %>% #change NAs to 0
    hist_xls_change_xs_to_ones() %>% 
    #ensure counts are numeric
    mutate_at(c("s1","s2","s3","s4","s5","s6","s7","s8","s9","s10","s11","s12","s13","s14","s15","s16","s17","s18","s19", "s20"), as.numeric)
  
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
#' @importFrom beepr beep
hist_xls_flag_missed_species <- function(df){
  
  if(nrow(df) != max(df$sequence)) {
    beep(11)
    return("Error! nrows does not match expected number of species codes")
  } 
  
  return(df)
}


#' Pull mbbs_county, route_num, and year
#' out of a historical MBBS_XL
#' route_num MUST BE the first number in the filename
#' year MUST BE the second number in the filename
#' @importFrom stringr str_extract str_extract_all
hist_xls_extract_county_num_year_from_filename <- function(filename) {
  
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


#'Converts 'x' and 'X' counts to 1
#'following method of mbbs route information
#'as compiled by Haven Wiley
#'@importFrom purrr map_dfr
#'@importFrom dplyr %>%
hist_xls_change_xs_to_ones <- function(hist_xls) {
  hist_xls <- hist_xls %>%
  purrr::map_dfr(., ~ifelse(.=="x",1,.)) %>%
  purrr::map_dfr(., ~ifelse(.=="X",1,.))
}


