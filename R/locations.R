#------------------------------------------------------------------------------#
# Function for compiling the locations of stop-level data
# really also a record of what stop-level data has been surveyed
# like survey-list but for stops instead of whole routes
# name is temporary
#------------------------------------------------------------------------------#

#get data from the $stop_counts 
#left join the ebird locations data

locations_workflow <- function(counts) {
  
  counts |>
    get_ebird_locations() |>
    check_location_displacement()
  
}

#' Gets the lat+lon of stop locations each year
#' @param counts ebird data.frame, gone through compute_ebird_counts(), handle_deviations(), and ebird_import_checks()
#' @keywords internal
get_ebird_locations <- function(counts) {
  counts |>
    #filter only to stop data
    dplyr::filter(is.na(stop_num) == FALSE) |>
    #keep one entry for each year/route/stop_num
    distinct(year, route, stop_num, lat, lon)
}

#' Checks for displacement of stops from the location of the previous year
#' using an 'as the crow flies' measure of how far apart two points are
#' on the globe
#' NOTE: possible bug where, if a stop location moves 50m year over year, 
#' can drift very far without being noted
check_location_displacement <- function(df, file = config$stop_coordinates) {
  
  base_coordinates <- read.csv(file)
  
  #This is year dependent and checks against prev year, 
  #if matches within 100m no flag, further than 100m, flag for location change.
  #for 2020, check matches 2021 data? Currently 2020 gets NA
  df |>
    group_by(route, stop_num) |>
    arrange(route, stop_num, year) |>
    mutate(
      distance_m = distHaversine(c(lat, lon), c(dplyr::lag(lat), dplyr::lag(lon))),
      flag_for_change = dplyr::case_when(
        is.na(distance_m) == TRUE ~ NA,
        is.na(distance_m) == FALSE & distance_m > 100 ~ TRUE,
        is.na(distance_m) == FALSE & distance_m <= 100 ~ FALSE
      ))
}

#' Compute distance between two points on the globe
#' taken from the 'geosphere' package, didn't want to add another dependency
#' ref: https://github.com/rspatial/geosphere/blob/master/R/distHaversine.R
#' modified lightly so p1 and p2 take c(lat, lon) which makes the most sense
distHaversine <- function(p1, p2, r=6378137) {
  #* Haversine formula to calculate distance between two points specified by 
  #* from: Haversine formula - R.W. Sinnott, "Virtues of the Haversine",
  #*  Sky and Telescope, vol 68, no 2, 1984
  #*  http:#//www.census.gov/cgi-bin/geo/gisfaq?Q5.1
  
  # source http://www.movable-type.co.uk/scripts/latlong.html
  # (c) 2002-2009 Chris Veness
  
  toRad <- pi / 180 #convert decimal degrees to radians
  
  assertthat::assert_that(is.vector(p1) & is.vector(p2))
  p1 <- matrix(p1, ncol = 2) * toRad
  p2 <- matrix(p2, ncol = 2) * toRad
  
  p = cbind(p1[,1], p1[,2], p2[,1], p2[,2], as.vector(r))
  
  dLon <- p[,4]-p[,2]
  dLat <- p[,3]-p[,1]
  
  a <- (sin(dLat/2))^2 + cos(p[,2]) * cos(p[,4]) * (sin(dLon/2))^2
  # to avoid values of 'a' that are a sliver above 1
  # which may occur at antipodes
  # https://stackoverflow.com/questions/45889616/why-does-disthaversine-return-nan-for-some-pairs-of-coordinates#
  a <- pmin(a, 1)
  dist <- 2 * atan2(sqrt(a), sqrt(1-a)) * p[,5]
  dist
}
