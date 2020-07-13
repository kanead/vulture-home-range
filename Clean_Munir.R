#' clean Munir data
library(tidyverse)

#' remove all
rm(list = ls())

#' load
mydata <- read_csv("data_munir/raw_data_drive/African White-backed Vultures Masai Mara.csv", col_names = T)
head(mydata)
names(mydata)

#' select relevant columns
mydata <- mydata %>% dplyr::select(timestamp, `location-long`, `location-lat`, `sensor-type`, `individual-local-identifier`, `individual-taxon-canonical-name`)

#' rename them
#' #' time long lat id species
mydata <- mydata %>% rename(time = timestamp)
mydata <- mydata %>% rename(long = `location-long`)
mydata <- mydata %>% rename(lat = `location-lat`)
mydata <- mydata %>% rename(sensor = `sensor-type`)
mydata <- mydata %>% rename(id = `individual-local-identifier`)
mydata <- mydata %>% rename(species = `individual-taxon-canonical-name`)
mydata

mydata %>% group_by(id) %>% slice(1) %>% dplyr::select(id) 

#' filter to GPS data only
levels(as.factor(mydata$sensor))

mydata <- filter(mydata, sensor == "gps")

mydata <- mydata %>% select(-sensor)
mydata

#' 2 species, rename species to something shorter 
levels(as.factor(mydata$species))
#' mydata <- filter(mydata, species == "Gyps africanus")
mydata$species <- as.factor(mydata$species)
levels(mydata$species)[levels(mydata$species)=="Gyps africanus"] <- "wb"
levels(mydata$species)[levels(mydata$species)=="Gyps rueppellii"] <- "rv"
levels(mydata$species)

#' how many individuals do we have?
levels(as.factor(mydata$id))

#' Baldrick is assumed to be an AWBV
mydata$species[is.na(mydata$species)] = "wb"

#' check the first row for deployment date
mydata %>% 
  group_by(id) %>%
  slice(1) %>% 
  ungroup()


#' clean up the names
mydata$id <- gsub(" ", "", mydata$id)
mydata$id <- gsub("-", "", mydata$id)
levels(as.factor(mydata$id))

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


