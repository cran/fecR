## ------------------------------------------------------------------------
library(fecR)

## ------------------------------------------------------------------------
okdata <- data.frame(
    eunr_id = "my_boat", loa = 2000, gt = 70, kw = 400,
    trip_id = c("trip1","trip1","trip2","trip2"),
    depdate = rep(c("20140718", "20141023"), each=2),
    deptime = rep(c("0615", "0730"), each=2),
    retdate = rep(c("20140719", "20141024"), each=2),
    rettime = rep(c("1830", "1615"), each=2),
    fishdate = c("20140718", "20140719", "20141023", "20141024"),
    gear = c("OTB","OTB","GN","GN"), gear_mesh_size = 80, fishing_area = "27.4.B",
    economic_zone = "EU", rectangle = "39F0",
    stringsAsFactors = FALSE
)
okdata

## ------------------------------------------------------------------------
test <- check_format(okdata)

## ------------------------------------------------------------------------
extra_col <- cbind(okdata, new_col = runif(nrow(okdata)))
test <- check_format(extra_col)

## ------------------------------------------------------------------------
test <- check_format(extra_col, correct=TRUE)

## ------------------------------------------------------------------------
test2 <- check_format(test)

## ------------------------------------------------------------------------
wrong_col <- okdata
colnames(wrong_col)[3] <- "something"
test <- check_format(wrong_col)

## ------------------------------------------------------------------------
wrong_eunr_id <- okdata
# Force them to be numeric instead of character
wrong_eunr_id[,"eunr_id"] <- 1234
test <- check_format(wrong_eunr_id)
# With the automatic check
test <- check_format(wrong_eunr_id, correct=TRUE)

## ------------------------------------------------------------------------
wrong_eunr_id <- okdata
# Set to be missing
wrong_eunr_id[1,"eunr_id"] <- as.character(NA)
test <- check_format(wrong_eunr_id)

## ------------------------------------------------------------------------
wrong_loa <- okdata
# Turn to a character string
wrong_loa[c(2,3),"loa"] <- "90m"
test <- check_format(wrong_loa)

## ------------------------------------------------------------------------
test <- check_format(wrong_loa, correct=TRUE)

## ------------------------------------------------------------------------
wrong_loa <- okdata
# Change to some entries to be alphabetical with no numerics
wrong_loa[c(2,3),"loa"] <- "notnumeric"
test <- check_format(wrong_loa, correct=TRUE)

## ------------------------------------------------------------------------
wrong_date <- okdata
# Needs to be character string, not numeric even if format is OK
wrong_date[, "retdate"] <- as.numeric(wrong_date[, "retdate"])
test <- check_format(wrong_date)
# We can correct
test <- check_format(wrong_date, correct=TRUE)

## ------------------------------------------------------------------------
wrong_date <- okdata
# Wrong format - year is too short
wrong_date[c(3,4), "retdate"] <- "141024"
test <- check_format(wrong_date)
# Wrong format again - month must be a numeric character
wrong_date[c(3,4), "retdate"] <- "October14"
test <- check_format(wrong_date)

## ------------------------------------------------------------------------
wrong_date <- okdata
# Missing data
wrong_date[c(3,4), "retdate"] <- as.character(NA)
test <- check_format(wrong_date)

## ------------------------------------------------------------------------
wrong_time <- okdata
# Needs to be character string, not numeric even if format is OK
wrong_time[, "rettime"] <- as.numeric(wrong_time[, "rettime"])
test <- check_format(wrong_time)
# We can correct by forcing to character
test <- check_format(wrong_time, correct=TRUE)

## ------------------------------------------------------------------------
wrong_time <- okdata
wrong_time[c(3,4), "rettime"] <- "16"
test <- check_format(wrong_time)
test <- check_format(wrong_time, correct=TRUE)

## ------------------------------------------------------------------------
wrong_time <- okdata
wrong_time[c(3,4), "rettime"] <- "TT"
test <- check_format(wrong_time, correct=TRUE)

## ------------------------------------------------------------------------
wrong_time <- okdata
wrong_time[c(3,4), "rettime"] <- "130"
test <- check_format(wrong_time)

## ------------------------------------------------------------------------
wrong_time <- okdata
wrong_time[c(3,4), "rettime"] <- "16:15"
test <- check_format(wrong_time)
test

## ------------------------------------------------------------------------
wrong_time <- okdata
wrong_time[c(3,4), "rettime"] <- as.character(NA)
test <- check_format(wrong_time)

## ------------------------------------------------------------------------
wrong_gear <- okdata
# Gear code is OK but whitespace
wrong_gear[1,"gear"] <- " OTB"
# Fails
test <- check_format(wrong_gear)
# Correct removes whitespace
test <- check_format(wrong_gear, correct=TRUE)

## ------------------------------------------------------------------------
wrong_gear[1,"gear"] <- "otb"
test <- check_format(wrong_gear)

## ------------------------------------------------------------------------
wrong_ms <- okdata
# Text in the entry - must be integer
wrong_ms[4,"gear_mesh_size"] <- "80mm"
test <- check_format(wrong_ms)
# Not an integer
wrong_ms[4,"gear_mesh_size"] <- 80.8
test <- check_format(wrong_ms)

## ------------------------------------------------------------------------
wrong_ms <- okdata
wrong_ms[4,"gear_mesh_size"] <- NA
test <- check_format(wrong_ms)
test <- check_format(wrong_ms, correct=TRUE)
test

## ------------------------------------------------------------------------
wrong_fish_area <- okdata
# Point at end
wrong_fish_area[c(1,2),"fishing_area"] <- "27.4.A."
test <- check_format(wrong_fish_area)
test <- check_format(wrong_fish_area, correct=TRUE)
# Lowercase
wrong_fish_area[c(1,2),"fishing_area"] <- "27.4.a"
test <- check_format(wrong_fish_area)
test <- check_format(wrong_fish_area, correct=TRUE)
# White space
wrong_fish_area[c(1,2),"fishing_area"] <- "27.4.A "
test <- check_format(wrong_fish_area)
test <- check_format(wrong_fish_area, correct=TRUE)

## ------------------------------------------------------------------------
wrong_fish_area <- okdata
wrong_fish_area[c(1,2),"fishing_area"] <- as.character(NA)
test <- check_format(wrong_fish_area)

## ------------------------------------------------------------------------
wrong_econ <- okdata
wrong_econ[3,"economic_zone"] <- "USA"
test <- check_format(wrong_econ)

## ------------------------------------------------------------------------
wrong_econ <- okdata
wrong_econ[3,"economic_zone"] <- ""
test <- check_format(wrong_econ)

## ------------------------------------------------------------------------
wrong_rect <- okdata
# with extra punctuation
wrong_rect[3,"rectangle"] <- "39F0'" 
test <- check_format(wrong_rect)
test <- check_format(wrong_rect, correct=TRUE)
test

## ------------------------------------------------------------------------
wrong_rect <- okdata
wrong_rect[3,"rectangle"] <- ""
test <- check_format(wrong_rect)

## ------------------------------------------------------------------------
# one trip, two days, different departure time, same identifier
wrong_unique <- okdata
wrong_unique[4,"deptime"] <- "0731"
test <- check_format(wrong_unique)

## ------------------------------------------------------------------------
# Duplicates
wrong_dup <- okdata
# Add a duplicate row
wrong_dup <- rbind(wrong_dup, wrong_dup[1,])
test <- check_format(wrong_dup)
test <- check_format(wrong_dup, correct=TRUE)

