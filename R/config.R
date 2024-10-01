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
  taxonomy_data_dir = "taxonomy",
  historical_data_dir = "historical",
  ebird_data_dir = "ebird",

  # Regex patterns
  county_pattern = "orange|chatham|durham",

  # files
  excluded_submissions = "excluded_submissions.yml",

  # Regex patterns
  county_pattern = "orange|chatham|durham"
)
