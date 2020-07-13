#' clean Glynn data
library(tidyverse)
library(readxl)

#' remove all
rm(list = ls())

#' load
mydata1 <-
  read_excel("data_glynn/raw_data_drive/Master_White_backed vulture.xlsx",
             col_names = T)
head(mydata1)
names(mydata1)
mydata1$species <- "wb"

mydata2 <-
  read_excel("data_glynn/raw_data_drive/MASTERcape.xlsx",
             col_names = T)
head(mydata2)
names(mydata2)
mydata2$species <- "cv"

mydata <- rbind(mydata1, mydata2)
head(mydata)

#' select relevant columns
mydata <-
  mydata %>% dplyr::select(
    Date_Time,
    Long,
    Lat,
    ID,
    species
  )

#' rename them
#' #' time long lat id species
mydata <- mydata %>% rename(time = Date_Time)
mydata <- mydata %>% rename(long = Long)
mydata <- mydata %>% rename(lat =  Lat)
mydata <- mydata %>% rename(id = ID)
mydata

#' take a look at the ids
levels(as.factor(mydata$id))
mydata$id <- as.factor(mydata$id)

#' look at the first row for each bird to check start dates
mydata %>% group_by(id) %>% filter(row_number()==1) %>% print(n = nrow(.))

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
