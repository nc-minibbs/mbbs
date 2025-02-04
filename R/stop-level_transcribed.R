#------------------------------------------------------------------------------#
#   DESC: prepare transcribed paper data, plus functions to do so
#------------------------------------------------------------------------------#

#' Update transcribed paper
#' Reads in the transcribed files
#' Saves any mismatches
#' Assuming no mismatches, adds together all the double-entry data
#' writes the stop-level-transcribed-paper csv
#'
#' @importFrom dplyr select mutate rename left_join filter
#' @importFrom assertthat assert_that
#' @importFrom utils read.csv
#' @include config.R
#' @returns transcribed_paper df of all the observations
#'  transcribed from the paper files,
#'  returned just in case this function is called to set a variable.
update_transcribed_paper <- function() {
  # browser()
  # set path
  path <- config$stop_level_transcribed_dir

  # load in the list of transcribed files
  transcribed <-
    list.files(
      path = path,
      pattern = "transcribed_paper_files",
      full.names = TRUE
    )
  # load in species_code to common_name converter
  sp_code_converter <-
    read.csv(config$code_to_common_name) %>%
    dplyr::select(common_name, species_code)
  # get taxonomy
  taxonomy <- get_ebird_taxonomy()


  # read in all the transcribed files
  t1 <- read_transcribed_csv(filepath = transcribed[1])
  t2 <- read_transcribed_csv(filepath = transcribed[2])
  t3 <- read_transcribed_csv(filepath = transcribed[3]) %>%
    dplyr::mutate(count = as.integer(count))
  t4 <- read_transcribed_csv(filepath = transcribed[4])

  # check files against each other, report mis-matches
  mismatches <-
    rbind(
      tpaper_find_mismatches(t1, t2, t1$transcriber[1], t2$transcriber[1]),
      tpaper_find_mismatches(t1, t3, t1$transcriber[1], t3$transcriber[1]),
      tpaper_find_mismatches(t1, t4, t1$transcriber[1], t4$transcriber[1]),
      tpaper_find_mismatches(t2, t3, t2$transcriber[1], t3$transcriber[1]),
      tpaper_find_mismatches(t2, t4, t2$transcriber[1], t4$transcriber[1]),
      tpaper_find_mismatches(t3, t4, t3$transcriber[1], t4$transcriber[1])
    )

  # save mismatches
  write.csv(mismatches, config$transcribed_mismatches, row.names = FALSE)

  # add all the files together where they overlap!
  # assumes no mismatches remaining.
  transcribed_paper_raw <-
    rbind(
      tpaper_find_overlap(t1, t2, t1$transcriber[1], t2$transcriber[1]),
      tpaper_find_overlap(t1, t3, t1$transcriber[1], t3$transcriber[1]),
      tpaper_find_overlap(t1, t4, t1$transcriber[1], t4$transcriber[1]),
      tpaper_find_overlap(t2, t3, t2$transcriber[1], t3$transcriber[1]),
      tpaper_find_overlap(t2, t4, t2$transcriber[1], t4$transcriber[1]),
      tpaper_find_overlap(t3, t4, t3$transcriber[1], t4$transcriber[1])
    )

  # check that everything was double entered, no problems.
  assertthat::assert_that(
    sum(transcribed_paper_raw$both == "yes") == nrow(transcribed_paper_raw)
  )

  # set up proper columns for joining with other mbbs rows.
  transcribed_paper <-
    transcribed_paper_raw %>%
    dplyr::select(-both, -obs1, -obs2, -obs3) %>%
    mutate(source = "transcribed_paper") %>%
    dplyr::rename(route_num = route) %>%
    # convert species code to common_name
    left_join(sp_code_converter, by = c("species_code")) %>%
    # filter out accipiter sp.
    filter(!species_code == "Accipiter SP" & !species_code == "accipiter sp") %>%
    # add taxonomy
    left_join(taxonomy, by = "common_name") %>%
    # remove unnesecary columns
    dplyr::select(-species_code, )

  # write csv
  write.csv(transcribed_paper, config$stop_level_transcribed, row.names = FALSE)
  logger::log_trace("stop_level_transcribed_paper updated")
}


#' Reads in a transcribed file csv and adds the name of the transcriber
#' @param filepath file path to the file to be read in
#' @importFrom dplyr mutate
#' @importFrom stringr str_extract
#' @returns transcribed the df read in from the csv from the filepath
read_transcribed_csv <- function(filepath = "inst/stop-level/transcribed_paper_files/transcribed_paper_files_GEL.csv") {
  name <- str_extract(filepath, "[A-Z][A-Z][A-Z](_[0-9])?")

  transcribed <-
    read.csv(filepath, header = TRUE) %>%
    mutate(transcriber = name)

  transcribed # return
}


#' Finds mismatches between dataframes, and adds the source as a column
#' @importFrom dplyr select if_all  mutate left_join filter anti_join rename distinct
#' @importFrom tidyr contains everything
#' @returns mismatches -df of ALL mismatches between df1 and df2
#' @param df1 first transcribed paper df
#' @param df2 second transcribed paper df
#' @param source1 character
#' @param source2 character
#' @param overlap the list of who transcribed which files
tpaper_find_mismatches <- function(
    df1 = read.csv("inst/stop-level/transcribed_paper_files/transcribed_paper_files_GEL.csv"),
    df2 = read.csv("inst/stop-level/transcribed_paper_files/transcribed_paper_files_ADS.csv"),
    source1 = "GEL",
    source2 = "ADS",
    overlap = read.csv(config$transcribed_paper_list)) {
  # filter only to the rows where we expect overlap
  overlap <- overlap %>%
    # pick out just the columns we need, including the columns "transcribed_by_SOURCE1|2"
    dplyr::select(mbbs_county, route, year, contains(source1) | contains(source2)) %>%
    # filter to rows where both sources transcribed that route-year, eg. no NAs
    filter(if_all(everything(), ~ !is.na(.))) %>%
    # add a column that will be easy to filter on
    mutate(both = "yes")

  # filter the dfs
  df1 <- df1 %>%
    left_join(overlap) %>%
    filter(both == "yes")

  df2 <- df2 %>%
    left_join(overlap) %>%
    filter(both == "yes")

  # anti_join the dfs to find differences
  anti_1 <- df1 %>%
    anti_join(df2, by = c("mbbs_county", "year", "route", "stop_num", "species_code", "count")) %>%
    dplyr::rename(
      species_code_s1 = species_code,
      count_s1 = count
    ) %>%
    mutate(s1 = source1)

  anti_2 <- df2 %>%
    anti_join(df1, by = c("mbbs_county", "year", "route", "stop_num", "species_code", "count")) %>%
    dplyr::rename(
      species_code_s2 = species_code,
      count_s2 = count
    ) %>%
    mutate(s2 = source2)

  # collate all the mismatches together
  mismatches1 <- left_join(anti_1, anti_2, by = c("mbbs_county", "year", "route", "stop_num")) %>%
    dplyr::select(-contains("obs"), -contains("notes"), -contains("transcribe"))
  mismatches2 <- left_join(anti_2, anti_1, by = c("mbbs_county", "year", "route", "stop_num")) %>%
    dplyr::select(-contains("obs"), -contains("notes"), -contains("transcribe"))
  mismatches <- bind_rows(mismatches1, mismatches2) %>%
    dplyr::distinct()

  mismatches # return
}


#' Returns the rows from df1 and df2 that had double entry
#' @importFrom dplyr select if_all  mutate left_join filter
#' @importFrom tidyr contains everything
#' @returns x df with only the rows from df1 and df2 that had double entry
#' @param df1 first transcribed paper df
#' @param df2 second transcribed paper df
#' @param source1 character
#' @param source2 character
#' @param overlap the list of who transcribed which files
tpaper_find_overlap <- function(
    df1 = read.csv("inst/stop-level/transcribed_paper_files/transcribed_paper_files_GEL.csv"),
    df2 = read.csv("inst/stop-level/transcribed_paper_files/transcribed_paper_files_ADS.csv"),
    source1 = "GEL",
    source2 = "ADS",
    overlap = read.csv(config$transcribed_paper_list)) {
  # filter only to the rows where we expect overlap between the two dfs
  overlap <- overlap %>%
    # pick out just the columns we need, including the columns "transcribed_by_SOURCE1|2"
    dplyr::select(mbbs_county, route, year, contains(source1) | contains(source2)) %>%
    # filter to rows where both sources transcribed that route-year, eg. no NAs
    filter(if_all(everything(), ~ !is.na(.))) %>%
    # add a column that will be easy to filter on
    mutate(both = "yes")

  # because we assume all mismatches have been handled,
  # now filter only to rows which had double entry
  # and the two dfs will be equivalent
  x <- df1 %>%
    left_join(overlap) %>%
    filter(both == "yes") %>%
    select(-contains("transcribe"))

  x # return
}

#' Get the stop-level data transcribed from paper files
#'
get_stop_level_transcribed <- function() {
  readr::read_csv(
    file = config$stop_level_transcribed,
    col_types = readr::cols(
      mbbs_county = readr::col_character(),
      year = readr::col_integer(),
      route_num = readr::col_integer(),
      stop_num = readr::col_integer(),
      count = readr::col_integer(),
      observers = readr::col_skip(),
      notes = readr::col_skip(),
      source = readr::col_character(),
      common_name = readr::col_character(),
      tax_order = readr::col_skip(),
      sci_name  = readr::col_skip()
    )
  ) |>
    rename(county = mbbs_county) |>
    mutate(
      route = make_route_id(county, route_num)
    )
}
