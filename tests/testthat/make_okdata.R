# Copyright European Union, 2016
# Author: Finlay Scott (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>, Nuno Prista (SLU) and Thomas Reilly (Marine Scotland)
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

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
