#' clean Phipps data
library(tidyverse)
library(readxl)

#' remove all
rm(list = ls())

mydata1 <-
  read_csv("data_phipps/raw_data_drive/AG032_18thMarch09_11thOct09_JuvAWbV.csv")

mydata2 <-
  read_csv("data_phipps/raw_data_drive/AG094_16thMay09_9thAug09_4thyr_AWbV.csv")

mydata3 <-
  read_csv("data_phipps/raw_data_drive/AG331_21stNov09_29thDec10_JuvAWbV.csv")

mydata4 <-
  read_excel("data_phipps/raw_data_drive/AG382_AG383_24thJune10_6thAug11_ImmCV.xlsx",
           sheet = 2)
mydata5 <-
  read_excel("data_phipps/raw_data_drive/DEAD_AG382_12thApr10_12thMay10_AdCV.xlsx",
           sheet = 2)

head(mydata1)
head(mydata2)
head(mydata3)
head(mydata4)
head(mydata5) 

#' first 3 have same headers and last 2 have same headers
mydata_sub1 <- rbind(mydata1,mydata2,mydata3)
mydata_sub1

#' these are a horror show
#' dates and months mixed up, omit them
mydata_sub2 <- rbind(mydata4,mydata5)
mydata_sub2

#' add a species column
mydata_sub1$species <- "wb"
mydata_sub2$species <- "cv"

#' select relevant columns
mydata_sub1 <-
  mydata_sub1 %>% dplyr::select(`Unit ID`,
                           Longitude,
                           Latitude,
                           `Date GMT + 0 (MM/DD/YYYY)`,
                           species)

#' rename them
#' #' time long lat id species
mydata_sub1 <- mydata_sub1 %>% rename(time = `Date GMT + 0 (MM/DD/YYYY)`)
mydata_sub1 <- mydata_sub1 %>% rename(long = Longitude)
mydata_sub1 <- mydata_sub1 %>% rename(lat = Latitude)
mydata_sub1 <- mydata_sub1 %>% rename(id = `Unit ID`)
mydata_sub1
head(mydata_sub1)
tail(mydata_sub1)

#' set the time
mydata_sub1$time <-
  as.POSIXct(mydata_sub1$time, format = "%m/%d/%Y %H:%M", tz = "UTC")
head(mydata_sub1)

attr(mydata_sub1$time, "tz")

sum(is.na(mydata_sub1$time))

#' how many individuals do we have?
levels(as.factor(mydata_sub1$id))

#' time long lat id species
mydata <- dplyr::select(mydata_sub1, time, long, lat, id, species)
head(mydata)
tail(mydata)

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


