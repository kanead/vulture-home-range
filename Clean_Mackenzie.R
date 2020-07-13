#' clean MacKenzie data
library(tidyverse)

#' remove all
rm(list = ls())

#' load
mydata <- read_csv("data_mackenzie/raw_data_drive/Gyps africanus Namibia.csv", col_names = T)
head(mydata)
names(mydata)

mydata %>% group_by(`individual-local-identifier`) %>% slice(1) %>% 
  dplyr::select(`individual-local-identifier`, `individual-taxon-canonical-name`) %>% 
  print(n = nrow(.))


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

#' filter to GPS data only
levels(as.factor(mydata$sensor))

mydata <- filter(mydata, sensor == "gps")

mydata <- mydata %>% dplyr::select(-sensor)
mydata

#' 2 species, rename species to something shorter 
levels(as.factor(mydata$species))
#' mydata <- filter(mydata, species == "Gyps africanus")
mydata$species <- as.factor(mydata$species)
levels(mydata$species)[levels(mydata$species)=="Gyps africanus"] <- "wb"
levels(mydata$species)[levels(mydata$species)=="Trigonoceps occipitalis"] <- "wh"
levels(mydata$species)

#' NAs are assumed to be AWBVs
mydata$species[is.na(mydata$species)] = "wb"
levels(as.factor(mydata$id))

#' how many individuals do we have?
levels(as.factor(mydata$id))

#' combine id with species name for clarity
mydata$id <- paste0(mydata$id,mydata$species)

#' check the first row for deployment date
mydata %>% 
  group_by(id) %>%
  slice(1) %>% 
  ungroup()

wb <- filter(mydata, species == "wb")
wh <- filter(mydata, species == "wh")

levels(as.factor(wb$id))
levels(as.factor(wh$id)) # "6032wh" "6220wh" "6484wh" "6485wh"

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


