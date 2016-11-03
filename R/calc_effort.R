# Copyright European Union, 2016
# Author: Finlay Scott (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>, Nuno Prista (SLU) and Thomas Reilly (Marine Scotland)
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

#' Calculate days at sea for a fishing trip.
#'
#' Calculate days at sea for a single fishing trip using data in the format
#' described in the package vignette \emph{checking_data}.
#'
#' The input is a single fishing trip. The format of the data should be
#' checked by \code{\link{check_format}} before calling this function 
#' (see the package vignette \emph{checking_data} for more details).
#' Days at sea is reported at the gear (type and mesh size), fishing area and
#' economic zone level. The total number of days at sea is the number of
#' commenced 24 hour periods.
#' The total number of days at sea of a trip is split equally over dates on
#' which fishing occurs. The effort for each fishing date is split equally over
#' the fishing activity on that date.  Active and passive gears are treated
#' equally.
#' See the vignette \emph{calculating_fishing_effort} for more details.
#' This function is called by \code{\link{calc_fishing_effort}}.
#'
#' @param trip Data.frame of the trip data
#' @return A \code{data.frame} with the days at sea by gear, fishing area
#' and economic zone.
#' @export
#' @seealso See \code{\link{calc_fishing_effort}}.
#' See the package vignette \emph{checking_data} for data preparation
#' and the vignette \emph{calculating_fishing_effort} for the calculation
#' details.
#' @examples
#' trip1 <- data.frame(
#'    eunr_id = "my_boat", loa = 2000, gt = 70, kw = 400,
#'    trip_id = "trip1",
#'    # 4 day trip
#'    depdate = "20140718", deptime = "0615", retdate = "20140721", rettime = "1615",
#'    # Only fish on 2 of those
#'    fishdate = c("20140719", "20140719", "20140719", "20140719", "20140720",
#'        "20140720", "20140720"), 
#'    gear = c("OTB","OTB","OTB","GN","OTB","GN","FPO"), gear_mesh_size = c(80,80,80,50,80,50,0),
#'    fishing_area = "27.4.B",
#'    economic_zone = "EU",
#'    rectangle = c("39F0","40F0","41F0","41F0","41F0","41F0","41F0"),
#'    stringsAsFactors = FALSE
#' )
#' das <- calc_days_at_sea_trip(trip1)
calc_days_at_sea_trip <- function(trip){
    # Check that this is just 1 trip
    if(length(unique(trip$trip_id)) != 1){
        stop("More than one trip found in data. This function only processes one trip at a time,")
    }
    # Calculate total days at sea
    # Number of commenced 24 hour periods - use lubridate interval class
    trip_start <- lubridate::ymd_hm(paste(trip$depdate[1],trip$deptime[1],sep=""))
    trip_end <- lubridate::ymd_hm(paste(trip$retdate[1],trip$rettime[1],sep=""))
    total_das <- ceiling(lubridate::int_length(lubridate::interval(trip_start, trip_end)) / (24*60*60))
    # Within each unique fish date, the days effort is split equally between fishing activity
    no_fishdates <- length(unique(trip$fishdate))
    # Add a column of the proportion of daily effort attributed to each activity
    fishdate <- NULL # Just to get rid of NOTE about visible binding when running check - seems annoying
    triptemp <- plyr::ddply(trip, "fishdate", transform, activity_prop = 1/length(fishdate))
    triptemp$days_at_sea <- triptemp$activity_prop * (total_das/no_fishdates)
    # Collapse date and rectangle
    days_at_sea <- NULL # Just to get rid of NOTE about visible binding when running check - seems annoying
    out <- plyr::ddply(triptemp, c("eunr_id", "loa", "gt", "kw", "trip_id", "depdate", "deptime", "retdate", "rettime", "gear", "gear_mesh_size", "fishing_area", "economic_zone"), plyr::summarise, days_at_sea = sum(days_at_sea))
    
    return(out)
}

#' Calculate fishing days for a fishing trip.
#'
#' Calculate fishing days for a single fishing trip using data in the format
#' described in the package vignette \emph{checking_data}.
#'
#' The input is a single fishing trip. The format of the data should be
#' checked by \code{\link{check_format}} before calling this function 
#' (see the package vignette \emph{checking_data} for more details).
#' Fishing days is reported at the gear (type and mesh size), fishing area, economic zone and rectangle level.
#' Passive and active gears are treated separately.
#' For active gears, each fishing date has 1 fishing day that is spread equally over the active gears.
#' For passive gears, each use of a passive gear is one fishing day, i.e. on fishing date can have several passive fishing days simultaneously.
#' See the vignette \emph{calculating_fishing_effort} for more details.
#' This function is called by \code{\link{calc_fishing_effort}}.
#'
#' @param trip Data.frame of the trip data
#' @return A data.frame with the fishing days by gear, fishing area, economic zone and rectangle.
#' @export
#' @seealso See \code{\link{calc_fishing_effort}}.
#' See the package vignette \emph{checking_data} for data preparation
#' and the vignette \emph{calculating_fishing_effort} for the calculation
#' details.
#' @examples
#' trip1 <- data.frame(
#'    eunr_id = "my_boat", loa = 2000, gt = 70, kw = 400,
#'    trip_id = "trip1",
#'    # 4 day trip
#'    depdate = "20140718", deptime = "0615", retdate = "20140721", rettime = "1615",
#'    # Only fish on 2 of those
#'    fishdate = c("20140719", "20140719", "20140719", "20140719", "20140720",
#'        "20140720", "20140720"), 
#'    gear = c("OTB","OTB","OTB","GN","OTB","GN","FPO"), gear_mesh_size = c(80,80,80,50,80,50,0),
#'    fishing_area = "27.4.B",
#'    economic_zone = "EU",
#'    rectangle = c("39F0","40F0","41F0","41F0","41F0","41F0","41F0"),
#'    stringsAsFactors = FALSE
#' )
#' fd <- calc_days_at_sea_trip(trip1)
calc_fishing_days_trip <- function(trip){
    # Check that this is just 1 trip
    if(length(unique(trip$trip_id)) != 1){
        stop("More than one trip found in data. This function only processes one trip at a time,")
    }
    # Separate passive and active gears - gear_codes is a data set included in the package - lazy loading
    gear_codes <- fecR::gear_codes
    passive_gears <- gear_codes[gear_codes$passive,"gear_code"]
    active_gears <- gear_codes[!gear_codes$passive,"gear_code"]
    active <- trip[trip$gear %in% active_gears,]
    passive <- trip[trip$gear %in% passive_gears,]
    # Process active gears - date which has some fishing is given 1 fishing day. This is spread equally over all active fishing activities on that date.
    # Add a column of the fishing day attributed to each activity
    fishdate <- NULL # Just to get rid of NOTE about visible binding when running check - seems annoying
    activetemp <- data.frame()
    if (nrow(active)>0){
        activetemp <- plyr::ddply(active, "fishdate", transform, fishing_days = 1/length(fishdate), passive_gear = FALSE)
    }
    # Process passive gears - each entry is a passive gear used on a day so gets 1 fishing day
    passivetemp <- data.frame()
    if (nrow(passive)>0){
        passivetemp <- transform(passive, fishing_days=1, passive_gear = TRUE)
    }
    # Stick the results together
    triptemp <- rbind(activetemp, passivetemp)
    # Collapse date
    fishing_days <- NULL # Just to get rid of NOTE about visible binding when running check - seems annoying
    out <- plyr::ddply(triptemp, c("eunr_id", "loa", "gt", "kw", "trip_id", "depdate", "deptime", "retdate", "rettime", "gear", "gear_mesh_size", "rectangle", "fishing_area", "economic_zone"), plyr::summarise, fishing_days = sum(fishing_days))
    return(out)
}

#' Calculate days at sea and fishing days for a full trip data set.
#'
#' Calculates the days at sea and the fishing days for each trip in a data set.
#'
#' The input is a \code{data.frame} that contains details of fishing trips.
#' The format of the data should be checked by \code{\link{check_format}} before calling this function 
#' (see the package vignette \emph{checking_data} for more details on the data format).
#' See the documentation of \code{\link{calc_fishing_days_trip}} and \code{\link{calc_days_at_sea_trip}}
#' and the vignette \emph{calculating_fishing_effort} for more details
#' of how the different effort measures are calculated.
#' This function has the additional option of calling \code{\link{check_format}} before
#' the calculations are performed.
#'
#' @param dat data.frame with the details of all the fishing trips.
#' @param check_data Should the \code{\link{check_format}} function be called on the data first (default is TRUE).
#' It is not possible to run \code{\link{check_format}} with automatic corrections here. Do this yourself first.
#' @return A list with two data.frames: one with the fishing days by gear, fishing area, economic zone and rectangle,
#' the other with the days at sea by gear, fishing area and economic zone.
#' @export
#' @seealso See \code{\link{check_format}}.
#' See the package vignette \emph{checking_data} for data preparation
#' and the vignette \emph{calculating_fishing_effort} for the calculation
#' details.
#' @examples
#' trip1 <- data.frame(
#'     eunr_id = "my_boat", loa = 2000, gt = 70, kw = 400,
#'     trip_id = "trip1",
#'     # 4 day trip
#'     depdate = "20140718", deptime = "0615", retdate = "20140721", rettime = "1615",
#'     # Only fish on 2 of those
#'     fishdate = c("20140719", "20140719", "20140719", "20140719", "20140720",
#'        "20140720", "20140720"), 
#'     gear = c("OTB","OTB","OTB","GN","OTB","GN","FPO"), gear_mesh_size = c(80,80,80,50,80,50,0),
#'     fishing_area = "27.4.B",
#'     economic_zone = "EU",
#'     rectangle = c("39F0","40F0","41F0","41F0","41F0","41F0","41F0"),
#'     stringsAsFactors = FALSE
#' )
#' trip2 <- data.frame(
#'     eunr_id = "my_boat", loa = 2000, gt = 70, kw = 400,
#'     trip_id = "trip2",
#'     # 2 day trip
#'     depdate = "20140718", deptime = "0615", retdate = "20140719", rettime = "0600",
#'     # Only fish on 2 of those
#'     fishdate = c("20140718", "20140719", "20140719", "20140719"), 
#'     gear = c("OTB","OTB","GN","FPO"), gear_mesh_size = c(80,80,50,0),
#'     fishing_area = "27.4.B",
#'     economic_zone = "EU",
#'     rectangle = c("39F0","39F0"),
#'     stringsAsFactors = FALSE
#' )
#' dat <- rbind(trip1, trip2)
#' effort <- calc_fishing_effort(dat)
calc_fishing_effort <- function(dat, check_data = TRUE){
    # Check data first of all
    if(check_data){
        print("Running check on data before calculating effort")
        dat <- tryCatch(check_format(dat),error=function(e) e, warning=function(w) w)
        if(methods::is(dat, "warning")){
            stop("Checking the input data generated a warning. Stopping here")
        }
        print("Data looks OK. Continuing")
    }
    # Get days at sea
    das <- plyr::ddply(dat, "trip_id", calc_days_at_sea_trip)
    fd <- plyr::ddply(dat, "trip_id", calc_fishing_days_trip)
    return(list(days_at_sea = das, fishing_days = fd))
}




