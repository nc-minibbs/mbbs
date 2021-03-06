% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_ebird_data.R
\name{make_comment_extractor}
\alias{make_comment_extractor}
\title{Create a function for extracting data in the eBird comments field}
\usage{
make_comment_extractor(
  field_pattern,
  data_pattern,
  delimiter = ";",
  post = function(x, ...) {     ifelse(length(x) == 0, NA_character_, x) }
)
}
\arguments{
\item{field_pattern}{a regex defining the valid pattern for the name of the 
field (e.g. observer) from eBird comments}

\item{data_pattern}{a regex defining the valid pattern for the field's data}

\item{delimiter}{the delimiter between field: data entries. Defaults to ";"}

\item{post}{a `function(x, ...)` to apply to the resulting strings}
}
\value{
a `function(comments, ...)` where `comments` is a ``character` vector
   of MBBS checklist comments and returns a `list` of strings where data has
   been extracted according to the provided regular expression.
}
\description{
Create a function for extracting data in the eBird comments field
}
