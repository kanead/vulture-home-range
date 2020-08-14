#' Combine monthly brownian bridge models based on regularised data
#' raster version

#' remove all
rm(list = ls())
graphics.off()

#' Load the required packages
library(tidyverse)

####' load the data ----
data_path <- "summary/brownian/regularised_raster/"   # path to the data

files <- dir(data_path, pattern = "*month") # get file names
length(files)

#' combine them all 
mydata <- files %>%
  # read in all the files, appending the path before the filename
  map( ~ read_csv(file.path(data_path, .))) %>%
  reduce(rbind)

head(mydata)

#' extract the name of the bird which occurs before the underscore 
mydata$bird <- gsub("(.+?)(\\_.*)", "\\1", mydata$ID)
mydata

#' how many Birds have we got?
length(levels(as.factor(mydata$bird)))
levels(as.factor(mydata$bird))

#' export the data
write.csv(x = mydata, file = "combined_month_reg_bb_raster.csv", row.names = F)
