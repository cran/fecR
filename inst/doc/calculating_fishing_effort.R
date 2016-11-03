## ------------------------------------------------------------------------
# Make fake data of two trips
trip1 <- data.frame(
    eunr_id = "my_boat", loa = 2000, gt = 70, kw = 400, trip_id = "trip1",
    # 4 day trip
    depdate = "20140718", deptime = "0615", retdate = "20140721", rettime = "1615",
    # Only fish on 2 of those
    fishdate = c("20140719", "20140719", "20140719", "20140719", "20140720", "20140720", "20140720"), 
    gear = c("OTB","OTB","OTB ","GN","OTB","GN","FPO"),
    gear_mesh_size = c(80,80,80,50,80,50,0),
    fishing_area = "27.4.B", economic_zone = "EU",
    rectangle = c("39F0","40F0","41F0","41F0","41F0","41F0","41F0"),
    stringsAsFactors = FALSE
)
trip2 <- data.frame(
    eunr_id = "my_boat", loa = 2000, gt = 70, kw = 400, trip_id = "trip2",
    # 2 day trip
    depdate = "20140722", deptime = "0615", retdate = "20140723", rettime = "0600",
    # Only fish on 2 of those
    fishdate = c("20140722", "20140723", "20140723", "20140723"), 
    gear = c("OTB","OTB","GN","FPO"),
    gear_mesh_size = c(80,80,50,0),
    fishing_area = "27.4.B",
    economic_zone = "EU",
    rectangle = c("39F0","39F0"),
    stringsAsFactors = FALSE
)
# Stich them together to make a data set of two fishing trips
dat <- rbind(trip1, trip2)
dat

## ------------------------------------------------------------------------
library(fecR)

## ------------------------------------------------------------------------
dat <- check_format(dat)

## ------------------------------------------------------------------------
dat[3,"gear"]

## ------------------------------------------------------------------------
dat <- check_format(dat, correct=TRUE)

## ------------------------------------------------------------------------
# Problem has been fixed
dat[3,"gear"]
# Check again
dat <- check_format(dat)

## ------------------------------------------------------------------------
effort <- calc_fishing_effort(dat, check=FALSE)

## ------------------------------------------------------------------------
effort$days_at_sea

## ------------------------------------------------------------------------
effort$fishing_days

## ------------------------------------------------------------------------
subset(effort$fishing_days, trip_id=="trip1")

