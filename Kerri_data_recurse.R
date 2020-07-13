#' Kerri - identification of breeding sites

#' remove all
rm(list = ls())
graphics.off()

#' Load the required packages
library(readr)
library(tidyverse)
library(amt)
library(SDLfilter)
library(recurse)

####' load the data ----
data_path <- "data_kerri"   # path to the data

files <- dir(data_path, pattern = "*.csv") # get file names
length(files)

mydata <- files %>%
  # read in all the files, appending the path before the filename
  map( ~ read_csv(file.path(data_path, .))) %>%
  reduce(rbind)

mydata$long <- as.numeric(mydata$long)
mydata$lat <- as.numeric(mydata$lat)

#' get rid of unusual negative longitudes
mydata <- mydata %>% dplyr::filter(long > 0)
mydata <- mydata %>% dplyr::filter(lat < -5)

levels(as.factor(mydata$study))
levels(as.factor(mydata$id))

#' Some of Kerri's tags were used on more than one bird and are removed
#' doubles <-c("AM234", "AG382", "AM89", "AM88", "AM87")

#' ---
#' Some data are GMT some are GMT + 2, some are unknown
#' GMT+2
#' 5008, AG313, AG314, AG329, AG330, AG331, AG332, AG349
#' AG350, AG351, AG352, AG353, AG356, AG382, AM220, AM222,
#' AM226, AM227, AM233, AM234, AM235, AM240, AM264
#' AM267, AM272, AM295, AM86, AM87, AM88, AM89
#' AM22 has seconds listed for some time stamps so do AM227, AM234, AM235, AM240, AM264, AM267, AM272
#'
#' GMT
#' Ingelheim, 22959306, 27230695, 27233665, 33640, 33798
#'
#' Unknown and excluded here for the time being
#' LFV_009, X009
#'
#' Some data have time arranged from most recent
#' Ingelheim
#'
#' ---
#' Date format for these is month-day-year
#' "AM222" "AM226" "AM227" "AM234" "AM235" "AM240" "AM264" "AM267" "AM272" "AM295"
#'
#' Date format for these is day-month-year, no seconds
#' 22959306, 27230695, 27233665, 33640, 33798, 5008, AG313, AG314, AG329,
#' AG330, Ag331, AG332, AG349, AG350 AG351, AG352, AG353, AG356, AG382,
#' AM220, AM233, AM86, AM87, AM88, AM89, Ingelheim, LFV_009, X009
#' 
#' 
#' 
#' 27230695 was released multiple times according to metadata and will be removed 
#' because it is causing issues for the HR measures 
#' 
#' Ingelheim, AM222, AM227, 5008 & 33798 give fractional values for monthly HRs
#' Remove them to test

group1 <-
  c(#"AM222",
    "AM226",
    #"AM227",
    "AM235",
    "AM240",
    "AM264",
    "AM267",
    "AM272",
    "AM295")
temp1 <- mydata %>% filter(id %in% group1)
temp1
temp1$time <-
  as.POSIXct(x = temp1$time, c("%m/%d/%Y %H:%M:%S"), tz = "africa/johannesburg")
temp1
attr(temp1$time, "tz") 

#' change to UTC
attr(temp1$time, "tzone") <- "UTC"
head(temp1)
sum(is.na(temp1$time))


group2 <-
  c(
    # "5008",
    "AG313",
    "AG314",
    "AG329",
    "AG330",
    "AG331",
    "AG332",
    "AG349",
    "AG350",
    "AG351",
    "AG352",
    "AG353",
    "AG356",
    "AM220",
    "AM233",
    "AM86"
  )
temp2 <- mydata %>% filter(id %in% group2)
temp2
temp2$time <-
  as.POSIXct(x = temp2$time, c("%d/%m/%Y %H:%M"), tz = "africa/johannesburg")
temp2
attr(temp2$time, "tz")
#' change to UTC
attr(temp2$time, "tzone") <- "UTC"
head(temp2)
sum(is.na(temp2$time))

group3 <-
  c(# "Ingelheim",
    "22959306",
    #   "27230695", removed
    "27233665",
    "33640"#,
    #   "33798"
  )
temp3 <- mydata %>% filter(id %in% group3)
temp3
temp3$time <- as.POSIXct(x = temp3$time, c("%d/%m/%Y %H:%M"), tz = "UTC")
temp3
sum(is.na(temp3$time))

#' stick them all back together 
mydata <- rbind(temp1, temp2, temp3)
mydata
attr(mydata$time, "tz")
levels(as.factor(mydata$id))

#' look at the species and extract the ids for each
levels(as.factor(mydata$species))
lf <- filter(mydata, species == "lf")
lf <- levels(as.factor(lf$id))
wb <- filter(mydata, species == "wb")
wb <- levels(as.factor(wb$id))
cv <- filter(mydata, species == "cv")
cv <- levels(as.factor(cv$id))

# Some of Kerri's data is in reverse order of time
# sort by the bird ID and reverse the order
mydata <- mydata %>% group_by(id)  %>% 
  arrange(time, .by_group = TRUE)
mydata

#' look at the starting dates
mydata %>% group_by(id) %>% slice(1) %>% print(n=nrow(.))

#' set the starting times
length(mydata$time)
mydata <- mydata %>% filter(
  id == "22959306"  |
    #   id == "27230695"  |
    id == "27233665"  |
    id == "33640"  |
    id == "33798"  |
    id == "Ingelheim"  |
    id == "AM220" |
    id == "5008" & time >= as.Date("2016-09-29 00:00:00") | #
    id == "AG313" & time >= as.Date("2009-11-08 00:00:00")| #
    id == "AG314" & time >= as.Date("2009-11-08 00:00:00")| #
    id == "AG329" & time >= as.Date("2009-12-02 00:00:00")| #
    id == "AG330" & time >= as.Date("2009-12-02 00:00:00")| #
    id == "AG331" & time >= as.Date("2009-11-21 00:00:00")| #
    id == "AG332" & time >= as.Date("2009-11-21 00:00:00")| #
    id == "AG349" & time >= as.Date("2009-12-02 00:00:00")| #
    id == "AG350" & time >= as.Date("2009-12-02 00:00:00")| #
    id == "AG351" & time >= as.Date("2009-12-09 00:00:00")| #
    id == "AG352" & time >= as.Date("2009-12-09 00:00:00")| #
    id == "AG353" & time >= as.Date("2010-01-17 00:00:00")| #
    id == "AG356" & time >= as.Date("2013-02-10 00:00:00")| #
    id == "AM222" & time >= as.Date("2007-03-02 00:00:00")| #
    id == "AM226" & time >= as.Date("2007-03-02 00:00:00")| #
    id == "AM227" & time >= as.Date("2007-03-02 00:00:00")| #
    id == "AM233" & time >= as.Date("2006-04-24 00:00:00")| #
    id == "AM235" & time >= as.Date("2007-03-15 00:00:00")| #
    id == "AM240" & time >= as.Date("2007-03-15 00:00:00")| #
    id == "AM264" & time >= as.Date("2009-01-29 00:00:00")| #
    id == "AM267" & time >= as.Date("2008-01-29 00:00:00")| #
    id == "AM272" & time >= as.Date("2007-12-19 00:00:00")| #
    id == "AM295" & time >= as.Date("2007-11-27 00:00:00")| #
    id == "AM86" & time >= as.Date("2005-12-04 00:00:00"))  #
length(mydata$time)

####' clean the data ----
#' Check for duplicated observations (ones with same lat, long, timestamp,
#'  and individual identifier).
ind2 <- mydata %>% dplyr::select(time, long, lat, id) %>%
  duplicated
sum(ind2)
#' remove them
mydata$dups <- ind2
mydata <- filter(mydata, dups == "FALSE")
mydata

#' drop the NAs
mydata <- mydata %>% drop_na(long, lat)

#' filter extreme data based on a speed threshold
#' based on vmax which is km/hr
#' time needs to be labelled DateTime for these functions to work
names(mydata)[names(mydata) == 'time'] <- 'DateTime'
SDLfilterData <-
  ddfilter.speed(data.frame(mydata), vmax = 100, method = 1)
length(SDLfilterData$DateTime)
head(SDLfilterData)

#' rename everything as before
mydata <- SDLfilterData
names(mydata)[names(mydata) == 'DateTime'] <- 'time'

#' select only the columns we need
mydata <- dplyr::select(mydata, time, lat, long, id, species)
head(mydata)

####' use amt package functions ----
#' convert into a track using amt
#' We can also use lat, long, which will allow us to determine
#' time of day
trk <- mk_track(
  mydata,
  .x = long,
  .y = lat,
  .t = time,
  id = id,
  crs = CRS("+init=epsg:4326")
)
trk

trk <- trk %>% arrange(id, t_)

#' Now it is easy to calculate day/night with either movement track
trk <- trk %>% time_of_day()

#' can remove the night time points as follows:
day_trk <- filter(trk, tod_ == "day") %>% arrange(id, t_)

####' create monthly groups ----
#' we need to extract monthly home ranges, some animals were tracked for over a year
#' so we must include a year-month-id grouping variable
#' first combine year and month
day_trk$yr_month <- format(day_trk$t_, format = "%Y/%m")
day_trk$yr_month <- as.factor(day_trk$yr_month)

#' we also need a year-month-day variable to see what the coverage is like over the course of
#' a month for each individual
day_trk$yr_month_day <- format(day_trk$t_, format = "%Y/%m/%d")
day_trk$yr_month_day <- as.factor(day_trk$yr_month_day)
head(day_trk)

#' count the number of unique days when grouped by id and and month
short_months <- day_trk %>%
  group_by(id, yr_month) %>%
  summarise(count = n_distinct(yr_month_day)) %>% filter(count < 28) %>% droplevels()
short_months
short_months$yr_month

#' we merge the two data frames and force all = TRUE so even the values that don't have a count
#' are included, this allows us to extract the tracks that have ~ a month
#' of coverage
test <- merge(short_months, day_trk, all = TRUE)
length(test$id)
length(day_trk$id)
summary(test$count)

#' keep only the rows with the NAs which are the counts > 28 i.e. data with ~ a month
#' of coverage
day_trk_mod <- test %>% dplyr::filter(is.na(count))
head(day_trk_mod)
summary(day_trk_mod$count)

#' now create a unique identifier that has the ID, year and month
#' We will use these to build home ranges
day_trk_mod$identifier <-
  paste(day_trk_mod$id, day_trk_mod$yr_month, sep = "_")
day_trk_mod$identifier <- as.factor(day_trk_mod$identifier)
head(day_trk_mod)

#' add the month and extract months where the bird could be breeding
day_trk_mod$month <- format(day_trk_mod$t_, format = "%m")
head(day_trk_mod)
day_trk_mod$month <- as.numeric(day_trk_mod$month)
head(day_trk_mod)
# day_trk_mod <- day_trk_mod %>% filter(month >= 3 & month <= 9)

#' turn this back into a track object
day_trk <- mk_track(
  day_trk_mod,
  .x = x_,
  .y = y_,
  .t = t_,
  id = identifier,
  crs = CRS("+init=epsg:4326")
)
day_trk

##### albers equal area projection ----
#' save the track object by nesting according to id and
#' converting coords into albers equal area so units are in metres
trk1 <- day_trk %>%
  transform_coords(
    sp::CRS(
      #' we can transform the CRS of the data to an equal area projection
      #' https://epsg.io/102022
      "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
    )
  )
trk1

#-------------------------------------
# Caluculate revisits in 50m radius
#-------------------------------------

# 50 m radius to find nesting/roosting sites (be careful of GPS error, approx. +/- 18m)
point_list <-
  split(trk1, trk1$id)
point_list <- discard(point_list, function(x)
  nrow(x) == 0)

recursions <- lapply(point_list, function(x)
  getRecursions(data.frame(x), 50, timeunits = "days"))
summary(recursions)

n <- c()
for (i in 1:length(recursions)) {
  n[i] <- max(recursions[[i]]$residenceTime)
}

n
sum(n > 15)

#' combine the IDs with the max time data from the recursion analysis
recursion_time <- cbind(data.frame(names(point_list), n))
recursion_time

#' #' time long lat id species
recursion_time <- recursion_time %>% rename(id = names.point_list.)
recursion_time <- recursion_time %>% rename(time = n)
#' pull out the ID which occurs before the first _
recursion_time$bird <- gsub("(.+?)(\\_.*)", "\\1", recursion_time$id)
# recursion_time$breeding <- if_else(recursion_time$time > 10, "yes", "no")
recursion_time$study <- "kerri"
recursion_time
write.csv(recursion_time, file = "summary/recursion_kerri.csv", row.names = FALSE)

