% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/locations.R
\name{check_location_displacement}
\alias{check_location_displacement}
\title{Checks for displacement of stops from the location of the previous year
using an 'as the crow flies' measure of how far apart two points are
on the globe
NOTE: possible bug where, if a stop location moves 50m year over year, 
can drift very far without being noted}
\usage{
check_location_displacement(df, file = config$stop_coordinates)
}
\description{
Checks for displacement of stops from the location of the previous year
using an 'as the crow flies' measure of how far apart two points are
on the globe
NOTE: possible bug where, if a stop location moves 50m year over year, 
can drift very far without being noted
}
