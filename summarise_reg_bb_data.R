#' Combine yearly brownian bridge models based on regularised data

#' remove all
rm(list = ls())
graphics.off()

#' Load the required packages
library(tidyverse)

####' load the data ----
data_path <- "summary/brownian/regularised/"   # path to the data

files <- grep(list.files(path=data_path), pattern='*month', invert=TRUE, value=TRUE)
length(files)

#' combine them all 
mydata <- files %>%
  # read in all the files, appending the path before the filename
  map( ~ read_csv(file.path(data_path, .))) %>%
  reduce(rbind)

head(mydata)

#' how many Birds have we got?
length(levels(as.factor(mydata$ID)))

#' export the data
write.csv(x = mydata, file = "combined_reg_bb.csv", row.names = F)
