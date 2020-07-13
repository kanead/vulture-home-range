#' Summarise breeding data

#' remove all
rm(list = ls())
graphics.off()

#' Load the required packages
library(tidyverse)

####' load the data ----
####' 
data_path <- "C:\\Users\\Adam\\Documents\\Science\\Manuscripts\\Vulture protected areas\\summary"
#' data_path <- "summary"   # path to the data

files <- dir(data_path, pattern = "*recursion") # get file names
length(files)

mydata <- files %>%
  # read in all the files, appending the path before the filename
  map( ~ read_csv(file.path(data_path, .))) %>%
  reduce(rbind)

head(mydata)

#' load in the yearly summary data file to add on extra info.
yearly <- read_csv("C:\\Users\\Adam\\Documents\\Science\\Manuscripts\\Vulture protected areas\\summary\\modified summaries\\yearly_data_summary.csv", col_names = T)

head(yearly)

#' Some of Buechley's data is actually located in Southern Africa 
#' and needs to have region changed to south:

south_buechley <- c(
  "Fringilla",
  "Lemba",
  "Lizzy",
  "Mubanga",
  "Precision",
  "Timbavati",
  "WBVUSA01",
  "WBVUSA02"
)

#' do it for yearly data
filter(yearly, study == "Buechley") %>%
  group_by(id) %>%
  slice(1) %>%
  dplyr::select(id, study, region)

yearly$region <-
  ifelse(yearly$id %in% south_buechley, "south", yearly$region)

filter(yearly, study == "Buechley") %>%
  group_by(id) %>%
  slice(1) %>%
  dplyr::select(id, study, region)

####' add the age column to the monthly data using ids ----
#' this is called id in the yearly data and bird in the breeding data
mydata$age <- yearly$age[match(mydata$bird, yearly$id)]
head(mydata)

#' add the known breeders 
mydata$recorded_breeder <- yearly$`confirmed_breeding?`[match(mydata$bird, yearly$id)]
mydata

#' add the region
mydata$region <- yearly$region[match(mydata$bird, yearly$id)]
mydata

#' add the species
mydata$species <- yearly$species[match(mydata$bird, yearly$id)]
mydata

#' look at adults only
adults <- mydata %>% dplyr::filter(age == "adult")

#' what is the maximum value at one site for known breeders?
filter(adults, recorded_breeder == "yes") %>% 
  group_by(bird) %>% 
  summarise(max_time = max(time))

#' look at bird with the smallest max value for breeding time in one area
filter(adults, bird == "X027")

#' create a column with month 
adults$month <- basename(adults$id)

#' can plot the times at one area as a histogram
ggplot(adults, aes(x = time)) + geom_histogram( fill = "steelblue", colour = "navy", alpha = 0.9) + 
  geom_vline(data = adults, aes(xintercept = 4.65, colour = "red")) +
  theme_minimal() + theme(legend.position = "none")

#' plot time spent in one location by region
ggplot(adults, aes(x = as.numeric(month), y = time)) + geom_point(fill = "steelblue", colour = "navy", alpha = 0.9) + theme_minimal() + theme(legend.position = "none") + 
  scale_x_continuous(breaks=c(1:12)) + facet_wrap(~region) + xlab("month")

#' same as above but with a boxplot
adults %>% filter(species == "rv") %>% 
ggplot(., aes(x = month, y = time)) + geom_boxplot(fill = "steelblue", colour = "navy", alpha = 0.9) + theme_minimal() + theme(legend.position = "none") + facet_wrap(~region) + xlab("month") + ggtitle("ruppells vultures")


#' can export these summaries
write.csv(mydata, file = "summary/breeding_data_summary.csv", row.names = FALSE)
write.csv(adults, file = "summary/breeding_data_summary_subset.csv", row.names = FALSE)
