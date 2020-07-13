#' clean Corinne Kendall Tanzania data
library(tidyverse)

#' remove all
rm(list = ls())

#' load
mydata1 <- read_csv("data_murn/raw_data_drive/murn_1.csv", col_names = T)
mydata2 <- read_csv("data_murn/raw_data_drive/murn_2.csv", col_names = T)

head(mydata1)
mydata1$time <-
  as.POSIXct(paste(mydata1$date, mydata1$time), format = "%m/%d/%Y %H:%M", tz = "Africa/Johannesburg")
head(mydata1)

attr(mydata1$time, "tzone")
attr(mydata1$time, "tzone") <- "UTC"
head(mydata1)

head(mydata2)
mydata2$time <-
  as.POSIXct(paste(mydata2$date, mydata2$time), format = "%d/%m/%Y %H:%M",tz = "Africa/Johannesburg")
head(mydata2)

attr(mydata2$time, "tzone")
attr(mydata2$time, "tzone") <- "UTC"
head(mydata2)

#' get columns in the same order
mydata1 <- mydata1 %>% dplyr::select(time, long, lat, id, species)
mydata2 <- mydata2 %>% dplyr:: select(time, long, lat, id, species)
mydata2$lat <- as.numeric(mydata2$lat)

#' bind the two
mydata <- rbind(mydata1,mydata2)
mydata

#' drop the NAs
mydata <- mydata %>% drop_na(lat)
plot(mydata$long, mydata$lat)

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


