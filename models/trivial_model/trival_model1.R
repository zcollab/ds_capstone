rm(list=ls()[ls() != "datenv"])

library(dplyr)
library(caret)
library(kernlab)

source("utilities.R")

set.seed(38292989)

if(!exists("datenv")) {
    datenv <- new.env()
    read_transformed_dat(datenv)
}

## Get IDs of the blighted buildings
blighted_buildings_filtered <- datenv$dat_demolition_transform %>%
    filter(!is.na(BuildID))
blighted_building_ids <- unique(blighted_buildings_filtered$BuildID)

## Create a dataframe of all the building IDs and whether they're blighted or not
buildings <- datenv$dat_parcels_transform %>% select(BuildID, x, y)
buildings$blighted <- buildings$BuildID %in% blighted_building_ids

## Sample an equal number of non-blighted buildings as blighted
nblighted <- length(blighted_building_ids)
non_blighted_samp <- sample_n(buildings %>% filter(blighted==FALSE), nblighted)

## Count up blight violations by building ID
violations_by_building <- datenv$dat_blight_transform %>%
    group_by(BuildID) %>%
    summarise(n_blight_violations=n())

## Make the datasets
add_violations_by_building <- function(dset) {
    dset <- dset %>% left_join(violations_by_building, by=c("BuildID"))
    dset$n_blight_violations <- ifelse(is.na(dset$n_blight_violations), 0, dset$n_blight_violations)
    dset$blighted <- factor(dset$blighted)

    return(dset)
}

blighted <- buildings %>% filter(blighted==TRUE)
model_dset <- rbind(blighted, non_blighted_samp)
model_dset <- add_violations_by_building(model_dset)

full_dset <- add_violations_by_building(buildings)


# Using only the sampled dataset ------------------------------------------
train_idx <- createDataPartition(model_dset$blighted, p=.8, list=FALSE)
train <- model_dset[train_idx,]
test <- model_dset[-train_idx,]

ctrl <- trainControl(method = "repeatedcv", number=5, savePredictions=TRUE)
#mod_fit <- train(blighted ~ n_blight_violations + x + y,
mod_fit <- train(blighted ~ n_blight_violations,
                 data=model_dset,
                 method="glm",
                 family="binomial",
                 trControl=ctrl)

pred <- predict(mod_fit, newdata=test)
confusionMatrix(data=pred, test$blighted)

# Using the full dataset --------------------------------------------------
pred <- predict(mod_fit, newdata=full_dset)
confusionMatrix(data=pred, full_dset$blighted)


# GP ----------------------------------------------------------------------
gp_mod_fit <- gausspr(blighted ~ n_blight_violations + x + y,
                 data=model_dset)

gp_pred <- predict(gp_mod_fit, newdata=model_dset)
confusionMatrix(data=gp_pred, model_dset$blighted)

gp_pred <- predict(gp_mod_fit, newdata=full_dset)
confusionMatrix(data=gp_pred, full_dset$blighted)
