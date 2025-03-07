% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/taxonomy.R
\name{get_ebird_taxonomy}
\alias{get_ebird_taxonomy}
\title{Get the ebird taxonomy dataset}
\usage{
get_ebird_taxonomy(path = config$taxonomy_data_dir)
}
\description{
Gets the most recent ebird taxonomy dataset available in the project repo.
NOTE: to update the taxonomy to the latest version of the eBird taxonomy,
files must be manually downloaded.
See revelant information in `docs/data-pipeline.md`.
}
