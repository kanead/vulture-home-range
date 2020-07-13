#' clean Lizanne data
library(tidyverse)
library(readxl)

#' remove all
rm(list = ls())

#' load
mydata <-
  read_excel("data_lizanne/raw_data_drive/EWT_gsmdata_Gyps_DSA324.xlsx",
             col_names = T)
head(mydata)
names(mydata)

levels(as.factor(mydata$unitid))
levels(as.factor(mydata$individualID))

mydata %>% group_by(individualID) %>% slice(1) %>% 
  dplyr::select(unitid, individualID, sciname) %>% 
  print(n = nrow(.))

#' select relevant columns
mydata <-
  mydata %>% dplyr::select(
    gpstime,
    longitude,
    latitude,
    individualID,
    sciname
  )

#' rename them
#' #' time long lat id species
mydata <- mydata %>% rename(time = gpstime)
mydata <- mydata %>% rename(long = longitude)
mydata <- mydata %>% rename(lat = latitude)
mydata <- mydata %>% rename(id = individualID)
mydata <- mydata %>% rename(species = sciname)
mydata


#' sort out the time zone by transforming it into UTC
#' "In relation to GMT that transmitter data was provided in"
head(mydata)
mydata$time <-
  as.POSIXct(mydata$time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
attr(mydata$time, "tzone")
head(mydata)

#' take a look at the ids
levels(as.factor(mydata$id))
mydata$id <- as.factor(mydata$id)

#' look at the first row for each bird to check start dates
mydata %>% group_by(id) %>% filter(row_number()==1) %>% print(n = nrow(.))

#' Rename species to something shorter
levels(as.factor(mydata$species))
mydata <-
  filter(mydata, species == "Gyps africanus" |
           species == "Gyps coprotheres") %>% droplevels()
mydata$species <- as.factor(mydata$species)
levels(mydata$species)[levels(mydata$species) == "Gyps africanus"] <-
  "wb"
levels(mydata$species)[levels(mydata$species) == "Gyps coprotheres"] <-
  "cv"
levels(mydata$species)

#' how many individuals do we have?
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
