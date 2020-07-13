#' clean Buechley data
library(tidyverse)
library(readxl)

#' remove all
rm(list = ls())

#' load
mydata <-
  read_excel("data_buechley/raw_data_drive/Pan-Africa Vulture Tracking.xlsx",
             col_names = T)
head(mydata)
names(mydata)

#' select relevant columns
mydata <-
  mydata %>% dplyr::select(
    timestamp,
    `location-long`,
    `location-lat`,
    `sensor-type`,
    `individual-local-identifier`,
    `individual-taxon-canonical-name`
  )

#' rename them
#' #' time long lat id species
mydata <- mydata %>% rename(time = timestamp)
mydata <- mydata %>% rename(long = `location-long`)
mydata <- mydata %>% rename(lat = `location-lat`)
mydata <- mydata %>% rename(sensor = `sensor-type`)
mydata <- mydata %>% rename(id = `individual-local-identifier`)
mydata <-
  mydata %>% rename(species = `individual-taxon-canonical-name`)
mydata

mydata %>% group_by(id) %>% slice(1) %>% print(n=nrow(.))

#' everything is in GPS so we can remove sensor type
levels(as.factor(mydata$sensor))
mydata <- mydata %>% filter(sensor == "gps")
mydata <- mydata %>% dplyr::select(-sensor)

#' take a look at the ids
levels(as.factor(mydata$id))
mydata$id <- as.factor(mydata$id)

#' look at the first row for each bird to check start dates
mydata %>% group_by(id) %>% filter(row_number()==1) %>% print(n = nrow(.))

#' 5 species, rename them to something shorter
levels(as.factor(mydata$species))
mydata$species <- as.factor(mydata$species)
levels(mydata$species)[levels(mydata$species) == "Gyps africanus"] <-  "wb"
levels(mydata$species)[levels(mydata$species) == "Gyps rueppellii"] <- "rv"
levels(mydata$species)[levels(mydata$species) == "Necrosyrtes monachus"] <- "hv"
levels(mydata$species)[levels(mydata$species) == "Neophron percnopterus"] <- "ev"
levels(mydata$species)[levels(mydata$species) == "Torgos tracheliotus"] <- "lf"
levels(mydata$species)

#' how many individuals do we have?
levels(as.factor(mydata$id))

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
