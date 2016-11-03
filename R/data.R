# Copyright European Union, 2016
# Author: Finlay Scott (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>, Nuno Prista (SLU) and Thomas Reilly (Marine Scotland)
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# Man pages for data sets

#' Fishing area
#'
#' A dataset of fishing areas given as DCF level 3 (DCF level 4 for the Baltic).
#'
#' @format A data frame with 150 rows and 2 variables:
#' \describe{
#'   \item{fishing_area}{code of the fishing area}
#'   \item{description}{description of the fishing area}
#' }
"fishing_areas"

#' Gear codes
#'
#' A dataset of gear codes taken from the Master Data Register.
#'
#' @format A data frame with 61 rows and 3 variables:
#' \describe{
#'   \item{gear_code}{code of the gear}
#'   \item{gear_type}{type of the gear}
#'   \item{description}{description of the gear}
#'   \item{passive}{is the gear passive (TRUE) or active (FALSE)}
#' }
"gear_codes"

#' ICES rectangles
#'
#' A dataset of ICES rectangles.
#'
#' @format A data frame with 7068 rows and 5 variables:
#' \describe{
#'   \item{ices_rectangle}{code of the rectangle}
#'   \item{south}{southern coordinate}
#'   \item{west}{westerly coordinate}
#'   \item{north}{northern coordinate}
#'   \item{east}{easterly coordinate}
#' }
"ices_rectangles"


