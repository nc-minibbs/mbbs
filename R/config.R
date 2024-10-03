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

  # Regex patterns
  county_pattern = "orange|chatham|durham",

  # files
  excluded_submissions = "data/excluded_submissions.yml"
)
