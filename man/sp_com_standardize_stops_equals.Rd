% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_species_comments.R
\name{sp_com_standardize_stops_equals}
\alias{sp_com_standardize_stops_equals}
\title{takes a list of [0-9]=[0=9] and returns a list of length 20
where the first number becomes the index in the list
and the second number becomes the value of the list at that index.
Indexes not explicitly changed in the list of 20 are set to have value '0'}
\usage{
sp_com_standardize_stops_equals(x)
}
\arguments{
\item{x}{list of characters formatted as [0-9]+=[0-9]+}
}
\description{
takes a list of [0-9]=[0=9] and returns a list of length 20
where the first number becomes the index in the list
and the second number becomes the value of the list at that index.
Indexes not explicitly changed in the list of 20 are set to have value '0'
}
