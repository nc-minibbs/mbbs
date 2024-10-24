% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_observers.R
\name{confirm_observer_NA}
\alias{confirm_observer_NA}
\title{Takes an observer/route combo where observer is NA and throws an error if the
survey for that route/year genuinely has no recorded observer either
within the mbbs_county dataframe or on the survey_list}
\usage{
confirm_observer_NA(passed_na_row, mbbs_county, county_observer_table)
}
\arguments{
\item{passed_na_row}{a dataframe with a single route_num and observer}

\item{mbbs_county}{an mbbs dataset that's restricted to just one county (as we use route_num which is not distinct between counties)}

\item{county_observer_table}{a main_observer_table that has already been filtered to just the relevant county}
}
\description{
Takes an observer/route combo where observer is NA and throws an error if the
survey for that route/year genuinely has no recorded observer either
within the mbbs_county dataframe or on the survey_list
}
