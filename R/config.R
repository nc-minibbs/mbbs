#------------------------------------------------------------------------------#
# R code related to configuration of the MBBS project
#------------------------------------------------------------------------------#

#' Defines various configuration settings used in this project
#'
#' Including:
#' * filepaths/names
#' * regex patterns
#' * directories

config <- list(

  # Filepaths
  taxonomy_data_dir = "data/taxonomy",
  historical_data_dir = "data/historical",
  ebird_data_dir = "data/ebird",
  stop_level_xls_dir = "data/stop-level/xls",
  stop_level_transcribed_dir = "data/stop-level/transcribed",

  # Regex patterns
  county_pattern = "orange|chatham|durham",

  # files

  ## list of ebird submissions that are excluded
  excluded_submissions = "data/excluded_submissions.yml",

  ## list of stops that have missing data
  stop_deviations = "data/stop_deviations.yml",

  ## Used to map bird codes to common names
  code_to_common_name = "data/bird_code_to_common_name.csv",

  ## File where transcribed stop-level data is kept (statically)
  stop_level_transcribed = "data/stop-level/stop-level_transcribed.csv",

  ## Files for resolving mismatches in transcribed data
  transcribed_mismatches = "data/stop-level/transcribed/remaining_mistmatches.csv",
  transcribed_paper_list = "data/stop-level/transcribed/mbbs_paper_files_list.csv"
)
