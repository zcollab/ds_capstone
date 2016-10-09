rm(list=ls()[ls() != "datenv"])

source("utilities.R")

library(rgdal)

parcel_size <- 50 # UTM is in meters, 25m ~ 80ft
xgrid <- seq(310000, 344000, by=parcel_size)
ygrid <- seq(4680000, 4708000, by=parcel_size)

assign_build_id2 <- function(x, y) {
    x_ids <- findInterval(x, xgrid)
    y_ids <- findInterval(y, ygrid)
    BuildID2 <- paste(x_ids, y_ids, sep="_")

    return(BuildID2)
}

add_buildID2 <- function(dat) {
    #' Add BuildID2 to a dataframe
    BuildID2 <- assign_build_id2(dat$x, dat$y)
    dat$BuildID2 <- BuildID2

    return(dat)
}
datenv <- lapply(datenv, add_buildID2)
