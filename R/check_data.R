# Author: Finlay Scott (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>, Nuno Prista (SLU) and Thomas Reilly (Marine Scotland)
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

#' Checks a character column in a fishing trip data column
#'
#' Checks a character column in a fishing trip data column. Used by the
#' \code{\link{check_format}} function when checking the format of the input
#' data. 
#'
#' This function checks that entries in a data column are character strings.
#' It is possible to attempt to correct non-character string entries by forcing
#' them to be characters. The function also checks for missing entries in the
#' data (either the R value \code{NA} or character strings \code{"NA"} or
#' \code{""}). It is possible to replace missing entries by a user defined
#' value. Appropriate messages are printed to the screen.
#' 
#' @param column The column of data from the data.frame.
#' @param column_name The name of the column.
#' @param character_correct Should non-character strings be forced to be characters.
#' @param missing_correct Should missing entries be corrected.
#' @param unknown_value The value to replace missing entries with
#' @return A list with the column (\code{x}, with or without attempted
#' corrections) and a flag (\code{ok}) to indicate if the data is OK or has
#' problems.
#' @export
#' @seealso This function is used by \code{\link{check_format}}.
#' @examples
#' dat <- data.frame(gear = c("OTB","OTB",NA, "NA", ""), gt = 100, stringsAsFactors=FALSE)
#' # Check with no correction
#' check <- character_column_check(dat$gear, "gear")
#' # Check with correction
#' check <- character_column_check(dat$gear, "gear", missing_correct=TRUE, unknown_value = "NK")
character_column_check <- function(column, column_name, character_correct=FALSE, missing_correct=FALSE, unknown_value = "UNKNOWN"){
    ok <- TRUE
    # Is it even a character?
    if (!is.character(column)){
        error_text <- paste("ATT: Error in ", column_name, " column. Column must be a character string.", sep="")
        print(error_text)
        # If not a character, we can do nothing else - abort
        if (character_correct==FALSE){
            warning(error_text)
            ok <- FALSE
            out <- list(column=column, ok=ok)
            return(out)
        } else {
            print("Attempting to correct by forcing to character string")
            column <- as.character(column)
        }
    }
    # Check missing rows
    missing_rows <- is.na(column) | (column %in% c("NA",""))
    if(any(missing_rows)){
        error_text <- paste("ATT: Missing code in the ", column_name, " column", sep="")
        print(error_text)
        print(paste("Problem rows: ", paste(which(missing_rows),collapse=", "), sep=""))
        if(missing_correct==FALSE){
            warning(error_text)
            ok <- FALSE
        } else {
            print(paste("Attempting to correct by replacing missing codes with ", unknown_value, sep=""))
            column[missing_rows] <- unknown_value
        }
    }
    out <- list(column=column, ok=ok)
    return(out)
}



#' Checks the fishing trip input data
#'
#' Checks the fishing trip data input data that is then used by the
#' \code{\link{calc_fishing_effort}} function.

#' The fishing trip data is stored in a \code{data.frame} which describes the
#' trip details (departure and return date and time) and the fishing activities
#' that occurred on the trip (by date, gear and area). The format of the input
#' data is explained in the the Nicosia report Annex 4 and in the package
#' vignette \emph{checking_data}.
#' The associated function \code{\link{calc_fishing_effort}} uses the input data
#' to calculate the fishing effort of each trip. This function depends on the
#' input data being in the correct format.
#' The \code{\link{check_format}} function checks that the format and contents
#' of the data are correct.  When the function is called various messages are
#' reported to the screen describing if the contents are OK and if not, why not.
#' If the data is not correct then warnings are produced.
#' Users should repeatedly check their date with this function and modify it as
#' described until the check passes without warning. Only if the data passes the
#' checks without warning can it used by the \code{\link{calc_fishing_effort}}.
#' It is possible to ask the function to attempt to automatically correct simple
#' errors (wrong case, numerics instead of characters etc). If this option is
#' used the function outputs messages describing the changes being performed.
#' The corrected data is returned by the function. Note that calling the
#' function with automatic corrections does not guarantee that the returned data
#' is correct. The returned data should be passed again into
#' \code{\link{check_format}} to see if it now passes.
#' More information on how \code{\link{check_format}} should be used can be found in
#' the package vignette \emph{checking_data}.
#' @param x The input data.frame.
#' @param correct Should automatic corrections to the data be attempted.
#' @return The input data.frame, with or without attempted corrections.
#' @export
#' @seealso See \code{\link{calc_fishing_effort}} for calculating effort.
#' See the package vignette \emph{checking_data} for more details on how to use
#' \code{\link{check_format}}.
check_format<-function(x, correct=FALSE)
{
    checkok <- TRUE
    print("===========================")
    print(" STECF Transversal2 checks on formats")
    print("===========================")
    if (correct==TRUE)
    {
        print("===============================")
        print("WARNING WARNING WARNING WARNING")
        print("correct=TRUE selected => changes other than simple formatting will be made to the dataset")
        print("The changes may not be what you want and implemented only for testing purposes")
        print("The corrected data is returned as the output")
        print("Correct input data should run with correct=FALSE")
        print("===============================")
    }

    # Check column names
    print("Checking column names...")
    # Allowed column names from Annex 4
    cols <- c('eunr_id','loa','gt','kw','trip_id','depdate','deptime','retdate','rettime','fishdate','gear','gear_mesh_size','fishing_area','economic_zone','rectangle')
    # If missing columns the rest of the code will fail so we need to escape here
    if(!all(cols %in% colnames(x))){
        checkok <- FALSE
        warning(paste("You are missing the columns:",cols[!(cols %in% colnames(x))], sep=" "))
        cat("Attention: There are problems with this data set.\nYou are missing one or more column. Exiting.\n")
        return(x)
    }
    # Spurious column
    if(!all(colnames(x) %in% cols)){
        error_text <- "You have 1 or more unrecognised columns"
        print(paste("ATT: ", error_text, sep=""))
        if(correct==FALSE){
            warning(error_text)
            checkok <- checkok & FALSE
        }
        else{
            print("Attempting to fix this problem by removing the unrecognised columns")
            x <- x[,cols]
        }
    }
    

    # eunr_id - vessel id - anonymous
    print("Checking eunr_id...")
    eunr_id_check <- character_column_check(x$eunr_id, "eunr_id", character_correct=correct, missing_correct=FALSE)
    checkok <- checkok & eunr_id_check$ok
    x$eunr_id <- eunr_id_check$column

    # numeric columns:
    # loa - vessel length in cm - numeric
    # gt - gross tonnage - numeric
    # kw - engine power - numeric
    for (numcol in c("loa", "gt", "kw")){
        print(paste("Checking ", numcol, "...", sep=""))
        notnum <- !is.numeric(x[,numcol])
        if (notnum){
            print(paste("ATT: ", numcol, " column must be numeric", sep=""))
            if(correct==FALSE){
                warning(paste("Error in ", numcol, " column", sep=""))
                checkok <- FALSE
            } else {
                print("Attempting to correct error by removing non-numeric characters")
                numcol_temp <- gsub("[^0-9]","", x[,numcol])
                numcol_temp <- as.numeric(numcol_temp)
                # If any NAs - flag up the problem
                if(any(is.na(numcol_temp))){
                    print("Unable to force to numeric. There is a problem.")
                    warning(paste("Error in ", numcol, " column", sep=""))
                    checkok <- FALSE
                } else {
                    x[,numcol] <- numcol_temp
                }
            }
        } else {
            # It is numeric but may be NA
            if(any(is.na(x[,numcol]))){
                error_text <- paste("NA in ", numcol, " column. Unable to correct",sep="")
                print(paste("ATT: ", error_text, sep=""))
                warning(error_text)
                checkok <- FALSE
            }
        }
    }

    # depdate, retdate, fishdate - date of departure etc - 8 numeric characters - YYYYMMDD
    for (datecol in c("depdate", "retdate", "fishdate")){
        print(paste("Checking ", datecol, "...", sep=""))
        datecol_check <- character_column_check(x[,datecol], datecol, character_correct=correct, missing_correct=FALSE)
        x[,datecol] <- datecol_check$column
        checkok <- checkok & datecol_check$ok
        if(datecol_check$ok){
            # Test that all are numeric
            notnumber <- grepl("[^0-9]", x[,datecol])
            problem_rows <- notnumber | nchar(x[,datecol])!=8
            # If not OK - no autocorrection is possible
            if (any(problem_rows)){
                error_text <- paste("ATT: Error in ", datecol, " column. Character string of format: yyyymmdd needed.", sep="")
                print(error_text)
                print(paste("Problem rows:", paste(which(problem_rows),collapse=", "), sep=" "))
                warning(error_text)
                checkok <- FALSE
            }
        }
    }

    # deptime and rettime - character string - 4 numeric characters - either HHMM or HH:MM
    for (timecol in c("deptime", "rettime")){
        print(paste("Checking ", timecol, "...", sep=""))
        # Do we have characters or missing values
        timecol_check <- character_column_check(x[,timecol], timecol, character_correct=correct, missing_correct=FALSE)
        x[,timecol] <- timecol_check$column
        checkok <- checkok & timecol_check$ok
        # Remove : character if exists
        x[,timecol] <- gsub(":","",x[,timecol]) 
        # If character string and none are missing
        if(timecol_check$ok){
            # Test that all numeric
            notnumber <- grepl("[^0-9]", x[,timecol])
            # Correct if just 2 characters
            only2 <- nchar(x[,timecol]) == 2
            if (any(only2 & !notnumber)){
                if(correct==TRUE){
                    print(paste("Only 2 characters in ", timecol, " column in rows: ", paste(which(only2),collapse=", "), sep=""))
                    #print(paste("ATT: ", timecol, " needs 4 characters: HHMM. Only 2 found.", sep=""))
                    print("Assuming these 2 are HH. Adding 00 as MM")
                    x[only2, timecol] <- paste(x[only2,timecol], "00", sep="")
                }
            }
            # Check that we now have 4 characters
            not4 <- !(nchar(x[,timecol]) == 4)
            if (any(not4 | notnumber)){
                error_text <- paste("ATT: ", timecol, " needs 4 numeric characters: HHMM.", sep="")
                print(error_text)
                print(paste("4 numeric characters in ", timecol, " column are needed in rows: ", paste(which(not4),collapse=", "), sep=""))
                warning(error_text)
                checkok <- FALSE
            }
        }
    }

    # gear - character string - MDR code
    print("Checking gear...")
    # Load allowed gear codes. Taken from: https://circabc.europa.eu/faces/jsp/extension/wai/navigation/container.jsp Gear Type 1.0
    # https://circabc.europa.eu/sd/d/d0dcd582-1beb-4ffc-8bac-ecf3dc021423/Code-GearType-v1.0.xls
    # Need to check that these are the proper codes!
    gear_codes <- fecR::gear_codes # lazy loading
    # Check for missing gear, i.e. real NA, text NA or nothing - if correct replace missing with "NK"
    gear_check <- character_column_check(x$gear, "gear", character_correct=correct, missing_correct=correct, unknown_value = "NK")
    x$gear <- gear_check$column
    checkok <- checkok & gear_check$ok
    # If not missing and is character - proceed with other checks
    if(gear_check$ok){
        # Check for whitespace - remove if correct
        # remove whitespace
        whitespace_rows <- grepl("\\s", x$gear)
        if(any(whitespace_rows)){
            print("ATT: Whitespace found in gear column")
            print(paste("Problem rows:", paste(which(whitespace_rows),collapse=", "), sep=" "))
            if(correct==FALSE){
                checkok <- FALSE
            } else {
                print ("Attempting to correct by removing whitespace from gears")
                x$gear <- gsub("\\s", "", x$gear)
            }
        }
        # Is gear in the gear list - unknown gear codes are not allowed and not corrected for
        # This is case sensitive
        wrong_gear_code <- !(x$gear %in% gear_codes$gear_code)
        if (any(wrong_gear_code)){
            error_text <- "ATT: Unknown code in gear column"
            print(error_text)
            print(paste("Problem rows:", paste(which(wrong_gear_code),collapse=", "), sep=" "))
            warning(error_text)
            checkok <- FALSE
        }
    }

    # gear_mesh_size - integer - every mesh size is a different gear
    print("Checking gear_mesh_size...")
    # Check that it is numeric - if anything other than numeric throw an error - safe for NA
    notnumber <- grepl("[^0-9]", x$gear_mesh_size)
    if(any(notnumber)){
            print(paste("ATT: Error in gear_mesh_size column. Only integers allowed.", sep=""))
            print(paste("Problem rows:", paste(which(notnumber),collapse=", "), sep=" "))
            warning(paste("Error in gear_mesh_size column", sep=""))
            checkok <- FALSE
    }
    # If just numbers
    if(all(!notnumber)){
        x$gear_mesh_size <- as.integer(x$gear_mesh_size)
        # Check for missing values - if missing could set 0 if correct
        no_mesh_size <- is.na(x$gear_mesh_size)
        if (any(no_mesh_size)){
            if(correct==TRUE){
                print("ATT: Attempting to correct missing gear mesh sizes by setting mesh size to 0")
                x[no_mesh_size,"gear_mesh_size"] <- 0
            } else {
                print("ATT: Missing gear_mesh_size. If no mesh, set to 0")
                warning("Missing gear_mesh_size")
                checkok <- FALSE
            }
            print(paste("Problem rows:", paste(which(no_mesh_size),collapse=", "), sep=" "))
        }
    }

    # fishing_area - DCF level 3 (level 4 for Baltic)
    print("Checking fishing_area...")
    # Check that this list is OK
    fishing_areas <- fecR::fishing_areas
    # Check for missing - no replacement
    fishing_area_check <- character_column_check(x$fishing_area, "fishing_area", character_correct=FALSE, missing_correct=FALSE)
    x$fishing_area <- fishing_area_check$column
    checkok <- checkok & fishing_area_check$ok
    if(fishing_area_check$ok==TRUE){
        whitespace_rows <- grepl("\\s", x$fishing_area)
        pointstart_rows <- grepl("^\\.", x$fishing_area)
        pointend_rows <- grepl("\\.$",x$fishing_area)
        lowercase_rows <-grepl("[a-z]", x$fishing_area) 
        if (any(whitespace_rows | pointstart_rows | pointend_rows | lowercase_rows)){
            error_text <- "There is whitespace, leading or ending points, or lowercase characters in some of the data."
            print(error_text)
            if(correct){
                print("Attempting to correct these issues")
                x$fishing_area <- gsub("\\s", "", x$fishing_area)
                # Removes . at start if exists
                x$fishing_area <- gsub("^\\.","", x$fishing_area)
                # Removes . at end if exists
                x$fishing_area <- gsub("\\.$","",x$fishing_area)
                x$fishing_area <- toupper(x$fishing_area)
            } else {
                warning(error_text)
                checkok <- FALSE
            }
        }
        # Check area is in the allowed list
        wrong_areas <- !(x$fishing_area %in% fishing_areas$fishing_area)
        if(any(wrong_areas)){
            print("ATT: Unknown areas in fishing_area column")
            print(paste("Unknown fishing_area rows:", paste(which(wrong_areas),collapse=", "), sep=" "))
            warning("Problem in fishing_area column")
            checkok <- FALSE
        }
    }

    # economic_zone - character
    print("Checking economic_zone...")
    # Is the unknown code OK?
    economic_zone_list<-c('EU','NOR','UNKNOWN')
    # Checking for missing values and characters
    economic_zone_check <- character_column_check(x$economic_zone, "economic_zone", character_correct=FALSE, missing_correct=FALSE)
    x$economic_zone <- economic_zone_check$column
    checkok <- checkok & economic_zone_check$ok
    if (economic_zone_check$ok){
        # Missing info
        unknown_zones <- !(x$economic_zone %in% economic_zone_list)
        if(any(unknown_zones)){
            print("ATT: Unknown code in economic_zone column")
            print(paste("Unknown economic_zone code rows:", paste(which(unknown_zones),collapse=", "), sep=" "))
            warning("Problem in economic_zone column")
            checkok <- FALSE
        }
    }

    # rectangle - ICES rectangle
    # Wishlist: current code does not ensure that rectangle*area*economic_zone combination. This can be added when table exists
    print("Checking rectangle...")
    ices_rectangles <- fecR::ices_rectangles
    # Checking for missing values
    rectangle_check <- character_column_check(x$rectangle, "rectangle", character_correct=FALSE, missing_correct=FALSE)
    x$rectangle <- rectangle_check$column
    checkok <- checkok & rectangle_check$ok
    if(rectangle_check$ok){
        # Checking for non alphanumeric characters and optionally removing them
        nonalpha_rect <- grepl("[^a-zA-Z0-9]", x$rectangle)
        lowercase_rows <-grepl("[a-z]", x$rectangle) 
        if (any(nonalpha_rect | lowercase_rows)){
            error_text <- "ATT: Non alphanumeric or lowercase characters in ices rectangle column ."
            print(error_text)
            if(correct==TRUE){
                print("Attempting to correct this by removing them.")
                x$rectangle <- gsub("[^a-zA-Z0-9]", "", x$rectangle)
                x$rectangle <- toupper(x$rectangle)
           } else {
                checkok <- FALSE
                warning(error_text)
            }
        }
        # Checking rectangle is in approved list
        unknown_rect <- !(x$rectangle %in% ices_rectangles$ices_rectangle)
        if(any(unknown_rect)){
            print("ATT: Unknown code in rectangle column")
            print(paste("Unknown rectangle code in rows:", paste(which(unknown_rect),collapse=", "), sep=" "))
            warning("Problem in rectangle column")
            checkok <- FALSE
        }
    }

    # trip_id - unique id of fishing trip
    # Must be unique character - check against dates and times
    print("Checking trip_id")
    trip_id_check <- character_column_check(x$trip_id, "trip_id", character_correct=correct, missing_correct=FALSE)
    checkok <- checkok & trip_id_check$ok
    x$trip_id <- trip_id_check$column
    if(trip_id_check$ok){
        print("Checking uniqueness of trip_id...")
        trip_id_duplicate <- !(tapply(paste(x$trip_id, x$eunr_id, x$depdate, x$deptime, x$retdate, x$rettime), x$trip_id, function(x){length(unique(x))}) == 1)
        if(any(trip_id_duplicate)){
            print ("ATT: trip_id does not uniquely identify fishing_trip:")
            print("e.g.:")
            print(unique(x[x$trip_id==names(which(trip_id_duplicate))[1],c("eunr_id","trip_id","depdate","deptime","retdate","rettime")]))
            warning("Trips must have unique combination of eunr_id, depdate, deptime, retdate and rettime")
            checkok <- FALSE
        }
    }

    # Checks to be added
    # Check that the trips for a vessel do not overlap
    # Check departure time is before return time 

    # Check for duplicates in dataset
    # Removes if wanted
    print("Checking duplicates...")
    dups <- duplicated(x)
    if(any(dups)){
        if(correct==TRUE){
            print(paste("ATT: ",sum(dups)," duplicated entries removed", sep=""))
            x <- x[!dups,]
        } else {
            print(paste("ATT: duplicate row(s) in final dataset:", sum(dups)))
            warning("Duplicate rows detected")
            checkok <- FALSE
        }
    }

    if(!checkok){
        print("===========================")
        print("Attention: There are problems with this data set.")
        print("===========================")
    }
    else{
        print("===========================")
        print("The returned data passes the check.")
        print("===========================")
    }
    return(x)
}

