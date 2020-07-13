#' Summarise monthly records

#' remove all
rm(list = ls())
graphics.off()

#' Load the required packages
library(tidyverse)

####' load the data ----
data_path <- "summary"   # path to the data

files <- dir(data_path, pattern = "*month") # get file names
length(files)

mydata <- files %>%
  # read in all the files, appending the path before the filename
  map( ~ read_csv(file.path(data_path, .))) %>%
  reduce(rbind)

mydata <- mydata %>% arrange(study, bird, id)
levels(as.factor(mydata$bird))
head(mydata)

#' export the combined monthly summaries
write.csv(mydata, file = "summary/monthly_data_summary.csv", row.names = FALSE)

