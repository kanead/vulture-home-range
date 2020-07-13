#' Summarise yearly records

#' remove all
rm(list = ls())
graphics.off()

#' Load the required packages
library(tidyverse)

####' load the data ----
data_path <- "summary/yearly/"   # path to the data

files <- dir(data_path, pattern = "*.csv") # get file names
length(files)

mydata <- files %>%
  # read in all the files, appending the path before the filename
  map( ~ read_csv(file.path(data_path, .))) %>%
  reduce(rbind)

head(mydata)
levels(as.factor(mydata$study))
levels(as.factor(mydata$id))

mydata <- mydata %>% arrange(study, id)

#' export the combined yearly summaries
write.csv(mydata, file = "summary/yearly_data_summary.csv", row.names = FALSE)

