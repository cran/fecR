# Copyright European Union, 2016
# Author: Finlay Scott (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>, Nuno Prista (SLU) and Thomas Reilly (Marine Scotland)
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# Check the check_format function
library(fecR)
context("check_format")
source("make_okdata.R")

test_that("ok data", {
    # Should pass without warning
    expect_warning(out <- check_format(okdata), regexp=NA)
    expect_equal(out, okdata)
})

test_that("extra column", {
    extra_col <- cbind(okdata, new_col = runif(nrow(okdata)))
    expect_warning(out <- check_format(extra_col))
    expect_warning(corout <- check_format(extra_col, correct=TRUE), regexp=NA)
    # Try corrected data
    expect_warning(out <- check_format(corout), regexp=NA)
})

test_that("wrong column names", {
    wrong_col <- okdata
    colnames(wrong_col)[3] <- "something"
    expect_warning(out <- check_format(wrong_col))
})

test_that("eurn_id column", {
    wrong_eunr_id <- okdata
    # Force them to be numeric instead of character
    wrong_eunr_id[,"eunr_id"] <- 1234
    expect_warning(out <- check_format(wrong_eunr_id))
    expect_warning(corout <- check_format(wrong_eunr_id, correct=TRUE), regexp=NA)
    # Check the corrected output
    expect_warning(out <- check_format(corout), regexp=NA)
    wrong_eunr_id <- okdata
    # Set entry to be missing
    wrong_eunr_id[1,"eunr_id"] <- as.character(NA)
    expect_warning(out <- check_format(wrong_eunr_id))
    # No correction possible
    expect_warning(out <- check_format(wrong_eunr_id, correct=TRUE))
})

test_that("loa, gt and kw", {
    for (col in c("loa","gt","kw")){
        wrong_col <- okdata
        rws <- sample(1:nrow(wrong_col), runif(1,1,nrow(wrong_col)))
        # Make character with numeric
        wrong_col[rws,col] <- "90m"
        expect_warning(out <- check_format(wrong_col))
        expect_warning(corout <- check_format(wrong_col, correct=TRUE), regexp=NA)
        expect_warning(out <- check_format(corout), regexp=NA)
        # Make character without numeric - no correction
        wrong_col[rws,col] <- "ninety"
        expect_warning(out <- check_format(wrong_col))
        expect_warning(corout <- check_format(wrong_col, correct=TRUE))
        expect_warning(out <- check_format(corout))
        # If missing
        wrong_col <- okdata
        wrong_col[rws,col] <- NA
        expect_warning(out <- check_format(wrong_col))
    }
})

test_that("date columns", {
    for(date_col in c("depdate","retdate", "fishdate")){
        wrong_date <- okdata
        # Needs to be character string, not numeric even if format is OK
        wrong_date[,date_col] <- as.numeric(wrong_date[,date_col])
        expect_warning(out <- check_format(wrong_date))
        # We can correct
        expect_warning(corout <- check_format(wrong_date, correct=TRUE), regexp=NA)
        expect_warning(out <- check_format(corout), regexp=NA)
        # If the format is wrong then we cannot automatically correct and check complains.
        wrong_date <- okdata
        rws <- sample(1:nrow(wrong_date), runif(1,1,nrow(wrong_date)))
        # Wrong format - year is too short
        wrong_date[rws, date_col] <- "141024"
        expect_warning(out <- check_format(wrong_date))
        expect_warning(out <- check_format(wrong_date, correct=TRUE))
        # Wrong format again - month must be a numeric character
        wrong_date[rws, date_col] <- "October14"
        expect_warning(out <- check_format(wrong_date))
        expect_warning(out <- check_format(wrong_date, correct=TRUE))
        # Missing data is not allowed and check complains.  It is not possible to correct for missing data.
        wrong_date <- okdata
        wrong_date[rws, date_col] <- as.character(NA)
        expect_warning(out <- check_format(wrong_date))
        expect_warning(out <- check_format(wrong_date, correct=TRUE))
    }
})

test_that("time column", {
    for (time_col in c("deptime", "rettime")){
        wrong_time <- okdata
        # Needs to be character string, not numeric even if format is OK
        wrong_time[, time_col] <- 1000
        expect_warning(out <- check_format(wrong_time))
        # We can correct by forcing to character
        expect_warning(corout <- check_format(wrong_time, correct=TRUE), regexp=NA)
        expect_warning(out <- check_format(corout), regexp=NA)
        # If only 2 characters are provided and automatic correction is TRUE,
        # the characters are assumed to be hours (*HH*) and minutes of "00" 
        # are appended to the string, e.g. "16" becomes "1600".
        wrong_time <- okdata
        wrong_time[wrong_time$trip_id=="trip1", time_col] <- "16"
        expect_warning(out <- check_format(wrong_time))
        expect_warning(outcor <- check_format(wrong_time, correct=TRUE), regexp=NA)
        expect_warning(out <- check_format(outcor), regexp=NA)
        # This automatic correction only works if the 2 characters are numeric
        wrong_time <- okdata
        wrong_time[wrong_time$trip_id=="trip1", time_col] <- "TT"
        expect_warning(out <- check_format(wrong_time))
        expect_warning(out <- check_format(wrong_time, correct=TRUE))
        # Two or four characters are needed. For example, "0130" is OK whereas "130" is not
        wrong_time <- okdata
        wrong_time[wrong_time$trip_id=="trip1", time_col] <- "130"
        expect_warning(out <- check_format(wrong_time))
        expect_warning(out <- check_format(wrong_time, correct=TRUE))
        # A : separator is accecptable (*HH:MM*) but is removed from the returned data.
        wrong_time <- okdata
        wrong_time[wrong_time$trip_id=="trip1", time_col] <- "16:15"
        expect_warning(out <- check_format(wrong_time), regexp=NA)
        expect_equal(out[out$trip_id=="trip1", time_col], rep("1615", sum(out$trip_id=="trip1")))
        # *NA*s and missing values are not acceptable and cannot be automatically corrected.
        wrong_time <- okdata
        wrong_time[wrong_time$trip_id=="trip1", time_col] <- as.character(NA)
        expect_warning(out <- check_format(wrong_time))
        expect_warning(out <- check_format(wrong_time, correct=TRUE))
    }
})

test_that("gear codes", {
    #Whitespace is not allowed. It can be automatically removed if wanted.
    wrong_gear <- okdata
    wrong_gear[1,"gear"] <- " OTB"
    expect_warning(out <- check_format(wrong_gear))
    # Correct removes whitespace
    expect_warning(corout <- check_format(wrong_gear, correct=TRUE), regexp=NA)
    expect_warning(out <- check_format(corout), regexp=NA)
    # If the gear code is not found in the MDR list then no automatic correction is possible.
    # Here the gear is unknown because it is lower case. All gear codes must be upper case.
    wrong_gear[1,"gear"] <- "otb"
    expect_warning(out <- check_format(wrong_gear))
})

test_that("gear mesh size", {
    # Entries that are not integer will make check complain. There is no option to autocorrect this.
    wrong_ms <- okdata
    # Text in the entry - must be integer - no correction possible
    row <- runif(1,1,nrow(wrong_ms))
    wrong_ms[row,"gear_mesh_size"] <- "80mm"
    expect_warning(out <- check_format(wrong_ms))
    expect_warning(out <- check_format(wrong_ms, correct=TRUE))
    # Cannot be an integer
    wrong_ms[row,"gear_mesh_size"] <- 80.8
    expect_warning(out <- check_format(wrong_ms))
    expect_warning(out <- check_format(wrong_ms, correct=TRUE))
    # If an entry is missing (e.g. it is *NA*) then check will complain.
    # It is possible to automatically correct this in which case the missing entry has a mesh size of 0.
    # The returned data will pass check but may not be what you want.
    wrong_ms[row,"gear_mesh_size"] <- NA
    expect_warning(out <- check_format(wrong_ms))
    expect_warning(corout <- check_format(wrong_ms, correct=TRUE) , regexp=NA)
    expect_warning(out <- check_format(corout), regexp=NA)
    expect_equal(corout[row,"gear_mesh_size"], 0L)
})

test_that("fishing area", {
    # Can correct for some errors
    wrong_fish_area <- okdata
    # Point at end - bad syntax
    row <- runif(1,1,nrow(wrong_fish_area))
    wrong_fish_area[row,"fishing_area"] <- "27.4.A."
    expect_warning(out <- check_format(wrong_fish_area))
    expect_warning(corout <- check_format(wrong_fish_area, correct=TRUE), regexp=NA)
    expect_warning(out <- check_format(corout), regexp=NA)
    # Lowercase
    wrong_fish_area[row,"fishing_area"] <- "27.4.a"
    expect_warning(out <- check_format(wrong_fish_area))
    expect_warning(corout <- check_format(wrong_fish_area, correct=TRUE), regexp=NA)
    expect_warning(out <- check_format(corout), regexp=NA)
    # White space
    wrong_fish_area[row,"fishing_area"] <- "27.4.A "
    expect_warning(out <- check_format(wrong_fish_area))
    expect_warning(corout <- check_format(wrong_fish_area, correct=TRUE), regexp=NA)
    expect_warning(out <- check_format(corout), regexp=NA)
    # Missing values are not allowed and cannot be automatically corrected.
    wrong_fish_area[row,"fishing_area"] <- as.character(NA)
    expect_warning(out <- check_format(wrong_fish_area))
    expect_warning(out <- check_format(wrong_fish_area, correct=TRUE))
})

test_that("economic zone", {
    # Wrong code - cannot be corrected
    wrong_econ <- okdata
    wrong_econ[3,"economic_zone"] <- "USA"
    expect_warning(out <- check_format(wrong_econ))
    expect_warning(out <- check_format(wrong_econ, correct=TRUE))
    # Missing values are not allowed and cannot be automatically corrected.
    wrong_econ <- okdata
    wrong_econ[3,"economic_zone"] <- "" # empty
    expect_warning(out <- check_format(wrong_econ))
    expect_warning(out <- check_format(wrong_econ, correct=TRUE))
    wrong_econ[3,"economic_zone"] <- as.character(NA) 
    expect_warning(out <- check_format(wrong_econ))
    expect_warning(out <- check_format(wrong_econ, correct=TRUE))
    wrong_econ[3,"economic_zone"] <- 56 # If numeric
    expect_warning(out <- check_format(wrong_econ))
    expect_warning(out <- check_format(wrong_econ, correct=TRUE))
})

test_that("rectangles", {
    # Remove extra punctuation - non-alphanumeric symbols
    wrong_rect <- okdata
    # with extra punctuation
    wrong_rect[3,"rectangle"] <- "39F0'" 
    expect_warning(out <- check_format(wrong_rect))
    expect_warning(corout <- check_format(wrong_rect, correct=TRUE), regexp=NA)
    expect_warning(out <- check_format(corout), regexp=NA)
    # Missing values are not allowed and cannot be automatically corrected.
    wrong_rect <- okdata
    wrong_rect[3,"rectangle"] <- ""
    expect_warning(out <- check_format(wrong_rect))
    expect_warning(out <- check_format(wrong_rect, correct=TRUE))
    wrong_rect[3,"rectangle"] <- as.character(NA)
    expect_warning(out <- check_format(wrong_rect))
    expect_warning(out <- check_format(wrong_rect, correct=TRUE))
    wrong_rect[3,"rectangle"] <- 666
    expect_warning(out <- check_format(wrong_rect))
    expect_warning(out <- check_format(wrong_rect, correct=TRUE))
})

test_that("trip identifier is unique", {
    # one trip, two days, different departure time, same identifier - no correction
    wrong_unique <- okdata
    wrong_unique[4,"deptime"] <- "0731"
    expect_warning(out <- check_format(wrong_unique))
    expect_warning(out <- check_format(wrong_unique, correct=TRUE))
    # Similarly, a trip with the same dates and times cannot have a different name.
    wrong_unique <- okdata
    wrong_unique[4,"trip_id"] <- "trip3"
    expect_warning(out <- check_format(wrong_unique))
    expect_warning(out <- check_format(wrong_unique, correct=TRUE))
})

test_that("duplicate rows", {
    # No duplicate rows allowed
    wrong_dup <- okdata
    # Add a duplicate row
    wrong_dup <- rbind(wrong_dup, wrong_dup[1,])
    expect_warning(out <- check_format(wrong_dup))
    expect_warning(outcor <- check_format(wrong_dup, correct=TRUE), regexp=NA)
    expect_warning(out <- check_format(outcor), regexp=NA)
    expect_equal(outcor, wrong_dup[-nrow(wrong_dup),])
})
