rm(list=ls())

source("utilities.R")

library(dplyr)
library(rgdal)
library(rgeos)
library(ggplot2)

read_raw_dat(environment())
#dat_parcels <- read.csv("data/Parcel_Points_Ownership.csv", stringsAsFactors = FALSE)

# Add lat/lon -------------------------------------------------------------

split_lat_lon <- function(lat_lon_pairs) {
    #' Given a vector of pairs like "(42.36318237000006, -83.09167672099994)"
    #' split them up into two separate vectors of lattitude and longitude
    split_pairs <- strsplit(lat_lon_pairs, ",")

    lat_strings <- sapply(split_pairs, function(lat_lon_pair) {
        lat_string <- lat_lon_pair[1]
    })
    lat <- as.numeric(substring(lat_strings, 2))

    lon_strings <- sapply(split_pairs, function(lat_lon_pair) {
        lat_string <- lat_lon_pair[2]
    })
    lon <- as.numeric(gsub(")", "", lon_strings))

    return(list(lat=lat, lon=lon))
}

# Parcels data already has Latitude/Longitude, but one record is incorrectly entered
dat_parcels$ID <- 1:nrow(dat_parcels)
dat_parcels <- dat_parcels %>% rename(lat=Latitude, lon=Longitude)
endsNAN <- function(charvec) endsWith(charvec, "NAN")
# Discard the incorrect record
dat_parcels <- dat_parcels[!endsNAN(dat_parcels$lat) & !endsNAN(dat_parcels$lon),]
dat_parcels$lat <- as.numeric(dat_parcels$lat)
dat_parcels$lon <- as.numeric(dat_parcels$lon)
# Remove any records without an address
dat_parcels <- dat_parcels %>% filter(PropAddr!="")
# There are a few listed as no square feet/acres
dat_parcels <- dat_parcels %>% filter(TotSqFt>0) %>% filter(TotAcres>0)

# 311 already has lat/lng
dat_311 <- dat_311 %>% rename(lon=lng)

# Blight
blight_lat_lon_pairs <- sapply(strsplit(dat_blight$ViolationAddress, "\n"), function(address_triple) address_triple[3])
blight_lat_lon <- split_lat_lon(blight_lat_lon_pairs)
dat_blight$lat <- blight_lat_lon$lat
dat_blight$lon <- blight_lat_lon$lon

# Crime
# Crime already has lat/lon
dat_crime <- dat_crime %>% rename(lon=LON, lat=LAT)

# Demolition
demolition_lat_lon_pairs <- sapply(strsplit(dat_demolition$site_location, "\n"), function(address_triple) address_triple[3])
# Some of the demolition lat/lon pairs are NA.  Some records are missing lat/lon
# and some records just don't have any location listed at all
demolition_lat_lon <- split_lat_lon(demolition_lat_lon_pairs)
dat_demolition$lat <- demolition_lat_lon$lat
dat_demolition$lon <- demolition_lat_lon$lon

# Process lat/lon ---------------------------------------------------------
dset_names <- dat_names()
names(dset_names) <- dat_transform_names()
datenv <- sapply(dset_names, function(name) get(name))
source(paste(DATA_TRANSFORM_BASE_PATH, "process_latlon.R", sep=""))

# Assign buildings --------------------------------------------------------
source(paste(DATA_TRANSFORM_BASE_PATH, "building_assignment.R", sep=""))

# Assign alternative building IDs based on a grid -------------------------
source(paste(DATA_TRANSFORM_BASE_PATH, "building_assignment2.R", sep=""))

# Write output ------------------------------------------------------------
BASE_OUT_PATH <- "data/transforms/"
for(i in 1:length(datenv)) {
    dat_name <- dat_transform_names()[i]
    out_filename <- TRANSFORMED_CSV_NAMES[i]
    write_csv(datenv[[dat_name]], BASE_OUT_PATH, out_filename)
}
