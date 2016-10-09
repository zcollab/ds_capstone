rm(list=ls())

source("utilities.R")

library(dplyr)
library(rgdal)
library(rgeos)
library(ggplot2)

#dat_parcels_transform <- read_csv(DATA_TRANSFORM_BASE_PATH, "dat_parcels_transform.csv")
read_transformed_dat(environment())

detroit_zipcode_map <- readOGR(dsn="data/shapefiles/City of Detroit Zip Code Boundaries", layer="geo_export_811c61ad-91e0-45e5-8fee-48e8f79a05f1") %>%
    spTransform(CRS("+proj=longlat +datum=WGS84"))

## Just map the zip codes
zip_map <- ggplot(data = detroit_zipcode_map, aes(x = long, y = lat, group=group)) + geom_path()
zip_map

scatter_records <- function(dset) {
    zip_map +
    geom_point(
        data = dset,
        aes(
            group = NULL,
            x = lon,
            y = lat,
            inherit.aes = FALSE
        ),
        col = "blue",
        alpha = .1
    )
}

# Get a quick idea of what the data looks like geographically
scatter_records(dat_parcels_transform)
scatter_records(dat_311_transform)
scatter_records(dat_blight_transform)
scatter_records(dat_crime_transform)
scatter_records(dat_demolition_transform)
