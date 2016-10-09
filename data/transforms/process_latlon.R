rm(list=ls()[ls() != "datenv"])

source("utilities.R")

if(!exists("datenv")) {
    datenv <- new.env()
    read_transformed_dat(datenv)
}

# Remove any records where lat/lon is missing
remove_missing_geo <- function(dat) {
    missing_geo <- is.na(dat$lon) | is.na(dat$lat)

    return(dat[!missing_geo,])
}
datenv <- lapply(datenv, remove_missing_geo)

# Some of the records have lat/lon pairs outside of Detroit.  Just discard them
remove_incorrect_geo <- function(dat) {
    wrong_lat <- (dat$lat < 42.25 ) | (dat$lat > 42.5)
    wrong_lon <- (dat$lon < -83.3 ) | (dat$lon > -82.9)
    incorrect_geo <- wrong_lat | wrong_lon

    return(dat[!incorrect_geo,])
}
datenv <- lapply(datenv, remove_incorrect_geo)

# Add fields for UTM coordinates
add_utm <- function(dat) {
    xy <- dat %>% select(lat, lon)
    coordinates(xy) <- c("lon", "lat")
    proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")

    # Detroit is in UTM zone 17
    res <- spTransform(xy, CRS("+proj=utm +zone=17 ellps=WGS84"))
    dat$x <- res@coords[,1]
    dat$y <- res@coords[,2]

    return(dat)
}
datenv <- lapply(datenv, add_utm)
