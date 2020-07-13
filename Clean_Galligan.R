#' clean Galligan data
library(tidyverse)
library(readxl)

#' remove all
rm(list = ls())

#' load
#' add species names to each
mydata1 <- read_excel("data_galligan//raw_data_drive/ARDB data 110117_Galligan.xlsx", col_names = T, sheet = 2)

mydata1$species <- "wb"
mydata1$id <- "wb143657"

mydata2 <- read_excel("data_galligan//raw_data_drive/ARDB data 110117_Galligan.xlsx", col_names = T, sheet = 4)

mydata2$species <- "lf"
mydata2$id <- "lf143660"

mydata <- rbind(mydata1, mydata2)

head(mydata)
names(mydata)

#' select relevant columns
mydata <- mydata %>% dplyr::select(Date, Time, `Longitude(E)`, `Latitude(N)`, species, id)

#' combine date and time
mydata$Date <- format(as.POSIXct(mydata$Date,format='%Y-%m-%d %H:%M:%S'),format='%Y-%m-%d')
mydata$Time <- format(as.POSIXct(mydata$Time,format='%Y-%m-%d %H:%M:%S'),format='%H:%M:%S')
mydata
mydata$time <- as.POSIXct(paste(mydata$Date, mydata$Time), format="%Y-%m-%d %H:%M:%S")

#' rename them
#' time long lat id species
mydata <- mydata %>% rename(long = `Longitude(E)`)
mydata <- mydata %>% rename(lat = `Latitude(N)`)
mydata <- mydata %>% dplyr::select(time, long, lat, id, species)
mydata

#' how many individuals do we have?
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


