#' clean Schabo data
library(tidyverse)

#' remove all
rm(list = ls())
graphics.off()

#' load
mydata <- read_csv("data_schabo/raw_data_drive/vultures_marburg.csv", col_names = T)
head(mydata)
names(mydata)

#' select relevant columns
mydata <- mydata %>% dplyr::select(timestamp, `location.long`, `location.lat`, `sensor.type`, name)

#' rename them
#' #' time long lat id species
mydata <- mydata %>% rename(time = timestamp)
mydata <- mydata %>% rename(long = `location.long`)
mydata <- mydata %>% rename(lat = `location.lat`)
mydata <- mydata %>% rename(sensor = `sensor.type`)
mydata <- mydata %>% rename(id = name)
mydata

#' filter to GPS data only
#' looks like it's all GPS data here
levels(as.factor(mydata$sensor))

mydata <- mydata %>% dplyr::select(-sensor)
mydata

#' add a species column
mydata$species <- "cv"

#' how many individuals do we have?
levels(as.factor(mydata$id))


#' look at the first row for each bird
mydata$id <- as.factor(mydata$id)

mydata %>% group_by(id) %>% 
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


