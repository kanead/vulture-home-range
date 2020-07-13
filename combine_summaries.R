#' combine summary data

#' remove all
rm(list = ls())

#' Load the required packages
library(tidyverse)
library(readr)

#' read in the individual study summaries
####' load the data ----
data_path <- "summary"   # path to the data

files <- dir(data_path, pattern = "*.csv") # get file names
length(files)

mydata <- files %>%
  # read in all the files, appending the path before the filename
  map( ~ read_csv(file.path(data_path, .))) %>%
  reduce(rbind)

head(mydata)
levels(as.factor(mydata$study))

write.csv(mydata, "all_summaries.csv", row.names = F)
