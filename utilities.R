library(dplyr)
library(data.table)

DATA_BASE_PATH <- "data/"
DATA_TRANSFORM_BASE_PATH <- "data/transforms/"
RAW_CSV_NAMES <- c("detroit-311.csv",
                "detroit-blight-violations.csv",
                "detroit-crime.csv",
                "detroit-demolition-permits.tsv",
                "Parcel_Points_Ownership.csv")
TRANSFORMED_CSV_NAMES <- c("dat_311_transform.csv",
                "dat_blight_transform.csv",
                "dat_crime_transform.csv",
                "dat_demolition_transform.csv",
                "dat_parcels_transform.csv")

dat_names <- function() {
    sapply(TRANSFORMED_CSV_NAMES, function(filename) gsub("_transform.csv", "", filename))
}

dat_transform_names <- function() {
    sapply(TRANSFORMED_CSV_NAMES, function(filename) gsub(".csv", "", filename))
}

read_csv <- function(basepath, filename, sep=",") {
     tbl_df(read.csv(file=paste(basepath, filename, sep=""), sep = sep, stringsAsFactors = FALSE))
    #fread(paste(basepath, filename, sep=""))
}

write_csv <- function(dat, basepath, filename) {
    write.csv(dat, file=paste(basepath, filename, sep=""), row.names = FALSE)
}

read_dat <- function(envir, basepath, filenames, suffix="") {
    dframe_names <- dat_names()
    for (i in 1:length(dframe_names)) {
        filename <- filenames[i]
        if (endsWith(filename, "csv")) {
            assign(paste(dframe_names[i], suffix, sep=""),
                   read_csv(basepath, filename),
                   envir = envir)
        }
        else {
            # For that one tab delimited file
            assign(paste(dframe_names[i], suffix, sep=""),
                   read_csv(basepath, filename, sep = "\t"),
                   envir = envir)
        }
    }
}

read_raw_dat <- function(envir) {
    read_dat(envir, DATA_BASE_PATH, RAW_CSV_NAMES)
}

read_transformed_dat <- function(envir) {
    read_dat(envir, DATA_TRANSFORM_BASE_PATH, TRANSFORMED_CSV_NAMES, suffix="_transform")
}