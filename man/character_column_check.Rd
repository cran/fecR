% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_data.R
\name{character_column_check}
\alias{character_column_check}
\title{Checks a character column in a fishing trip data column}
\usage{
character_column_check(column, column_name, character_correct = FALSE,
  missing_correct = FALSE, unknown_value = "UNKNOWN")
}
\arguments{
\item{column}{The column of data from the data.frame.}

\item{column_name}{The name of the column.}

\item{character_correct}{Should non-character strings be forced to be characters.}

\item{missing_correct}{Should missing entries be corrected.}

\item{unknown_value}{The value to replace missing entries with}
}
\value{
A list with the column (\code{x}, with or without attempted
corrections) and a flag (\code{ok}) to indicate if the data is OK or has
problems.
}
\description{
Checks a character column in a fishing trip data column. Used by the
\code{\link{check_format}} function when checking the format of the input
data.
}
\details{
This function checks that entries in a data column are character strings.
It is possible to attempt to correct non-character string entries by forcing
them to be characters. The function also checks for missing entries in the
data (either the R value \code{NA} or character strings \code{"NA"} or
\code{""}). It is possible to replace missing entries by a user defined
value. Appropriate messages are printed to the screen.
}
\examples{
dat <- data.frame(gear = c("OTB","OTB",NA, "NA", ""), gt = 100, stringsAsFactors=FALSE)
# Check with no correction
check <- character_column_check(dat$gear, "gear")
# Check with correction
check <- character_column_check(dat$gear, "gear", missing_correct=TRUE, unknown_value = "NK")
}
\seealso{
This function is used by \code{\link{check_format}}.
}
