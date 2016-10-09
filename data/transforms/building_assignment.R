rm(list=ls()[ls() != "datenv"])

library(sp)
library(rgdal)
library(RANN)

source("utilities.R")

if(!exists("datenv")) {
    datenv <- new.env()
    read_transformed_dat(datenv)
}

# Nearest neighbors -------------------------------------------------------
building_coords <- datenv$dat_parcels_transform %>% select(x, y)

assign_building_id <- function(dat) {
    coords <- dat %>% select(x, y)
    nearest <- nn2(building_coords, coords, k=1)

    # A observation is "too far" away if it is further than max_dist from the
    # building's center
    max_dist <- 30 # 30 meters ~ 100 feet
    too_far <- ifelse(nearest$nn.dists > max_dist, TRUE, FALSE)

    nearest_building_IDs <- datenv$dat_parcels_transform$ID[nearest$nn.idx]
    dat$BuildID <- ifelse(too_far, NA, nearest_building_IDs)

    return(dat)
}
datenv <- lapply(datenv, assign_building_id)
