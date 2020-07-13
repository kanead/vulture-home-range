#' clean Ogada data
library(tidyverse)

#' remove all
rm(list = ls())

#' load
mydata <- read_csv("data_ogada/raw_data_drive/northern Kenya vulture project.csv", col_names = T)
head(mydata)
names(mydata)

#' select relevant columns
mydata <- mydata %>% dplyr::select(timestamp, `location-long`, `location-lat`, `sensor-type`, `individual-local-identifier`, `individual-taxon-canonical-name`, `gps:satellite-count`)

#' rename them
#' #' time long lat id species
mydata <- mydata %>% rename(time = timestamp)
mydata <- mydata %>% rename(long = `location-long`)
mydata <- mydata %>% rename(lat = `location-lat`)
mydata <- mydata %>% rename(sensor = `sensor-type`)
mydata <- mydata %>% rename(id = `individual-local-identifier`)
mydata <- mydata %>% rename(species = `individual-taxon-canonical-name`)
mydata <- mydata %>% rename(quality = `gps:satellite-count`)
mydata

#' filter to GPS data only
levels(as.factor(mydata$sensor))

mydata <- filter(mydata, sensor == "gps")

mydata <- mydata %>% dplyr::select(-sensor)
mydata

#' remove low quality fixes
summary(mydata$quality)
mydata <- filter(mydata, quality > 5)
summary(mydata$quality)

#' 3 species, rename species to something shorter 
#' assume gyps is wb
levels(as.factor(mydata$species))
#' mydata <- filter(mydata, species == "Gyps africanus")
mydata$species <- as.factor(mydata$species)
levels(mydata$species)[levels(mydata$species)=="Gyps africanus"] <- "wb"
levels(mydata$species)[levels(mydata$species)=="Gyps"] <- "wb"
levels(mydata$species)[levels(mydata$species)=="Gyps rueppellii"] <- "rv"
levels(mydata$species)[levels(mydata$species)=="Necrosyrtes monachus"] <- "hv"
levels(mydata$species)

#' how many individuals do we have?
levels(as.factor(mydata$id))

#' change the names to something simpler
mydata$id <- gsub( " .*$", "", mydata$id)
mydata$id <- gsub( "-", "", mydata$id)
mydata$id <- gsub( "'", "", mydata$id)

levels(as.factor(mydata$id))


#' check the first row for deployment date
mydata %>% 
  group_by(id) %>%
  slice(1) %>% 
  ungroup()


####' export cleaned data ----
#' export the cleaned tracks
#' write the function
customFun  = function(DF) {
  write.csv(DF, paste0("", unique(DF$id), ".csv"), row.names = FALSE)
  return(DF)
}

#' apply the function to the data set by bird ID
mydata %>%
  group_by(id) %>%
  dplyr::select(time, long, lat, id, species) %>%
  do(customFun(.))


