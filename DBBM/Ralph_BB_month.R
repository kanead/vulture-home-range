####' Vulture comparative analysis
####' Ralph tracks - Ralph Buij 
####' 4 individuals
####' Raw data

# 5013	juvenile#5013	T occipitalis	2017-08-18 00:00:00.000	2017-09-05 00:00:00.000
# 5011	#399745060	Gyps africanus	2017-08-25 00:00:00.000	
# 5019	#399749318	Gyps africanus	2017-08-25 00:00:00.000	
# 5020	individual#5020	T occipitalis	2017-11-19 00:00:00.000	

#' I remove the wh because they are only tracked for a few days

#' remove all
rm(list = ls())

#' Load the required packages
library(readr)
library(tidyverse)
library(amt)
library(SDLfilter)
library(ggmap)
library(sf)
library(raster)
library(adehabitatHR)
library(adehabitatLT)
library(move)


####' load the data ----
data_path <- "data_ralph"   # path to the data

files <- dir(data_path, pattern = "*.csv") # get file names
length(files)

mydata <- files %>%
  # read in all the files, appending the path before the filename
  map(~ read_csv(file.path(data_path, .))) %>%
  reduce(rbind)

mydata$long <- as.numeric(mydata$long)
mydata$lat <- as.numeric(mydata$lat)

#' remove wh
mydata <- filter(mydata, species == "wb")
levels(as.factor(mydata$species))

#' Tracks are assumed to be in UTC
head(mydata)
mydata$time <-
  as.POSIXct(mydata$time, format = "%Y-%m-%d %H:%M", tz = "UTC")
head(mydata)

attr(mydata$time, "tz")

#' remove the seconds from the time stamp
mydata$time <- round(mydata$time, units = "mins")

attr(mydata$time, "tz")
mydata

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

#' remove the duplicate time stamps
ind2 <- mydata %>% dplyr::select(time, id) %>%
  duplicated
sum(ind2)
#' remove them
mydata <- mydata %>% dplyr::select(time, long, lat, id, species)
mydata$dups <- ind2
mydata$time <- as.character(mydata$time)
mydata
mydata <- dplyr::filter(mydata, dups == "FALSE")
mydata

#' turn time column back into time object
mydata$time <-
  as.POSIXct(mydata$time, format = "%Y-%m-%d %H:%M", tz = "UTC")
head(mydata)

attr(mydata$time, "tz")

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

#####' plot the data ----
#' plot all of the data on the one map
#' p1 <- qmplot(long,
#             lat,
#             data = mydata,
#             maptype = "toner-lite",
#             colour = id)
#' p1
#' or plot a simple scatter plot without a map
# p2 <- ggplot(mydata, aes(long, lat, color = id)) + geom_point()
#' p2

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
day_trk_mod$identifier <- paste(day_trk_mod$id, day_trk_mod$yr_month, sep = "_")
day_trk_mod$identifier <- as.factor(day_trk_mod$identifier)
head(day_trk_mod)

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

track <- data.frame(trk1)
track <- arrange(track, id)
head(track)
#' create a move object 
loc <-
  move(
    x = track$x_,
    y = track$y_,
    time = track$t_,
    proj = CRS( "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"),
    data = track,
    animal = track$id
  )

#' Now create a dBBMM object
dbbmm <-
  brownian.bridge.dyn(
    object = loc,
    location.error = 20,
    window.size = 31,
    margin = 11,
    dimSize = 100,
    ext = 0.8
  )

#' try a function
#' this one works but doesn't give the names
dbbmm_size <- function (x) {
  return(tryCatch(
    st_area(hr_isopleths(dbbmm[[x]], level = 0.95)) / 1e6,
    error = function(e)
      NULL
  ))
}
y <- c(1:length(dbbmm[1]))
areas <- lapply((y), dbbmm_size); areas
#' add names by taking them from dbbmm object
names(dbbmm)
names(areas) <- names(dbbmm)
bb_areas <- data.frame(unlist(areas))
length(bb_areas$unlist.areas.)

#' try one
st_area(hr_isopleths(dbbmm$X.399745060, level = 0.95)) / 1e6 #' 9600.509  
st_area(hr_isopleths(dbbmm$X.399749318, level = 0.95)) / 1e6 #' 135931.7

#' load in the cleaned version which is in Albers Equal Area
merged_Africa_tranform <- read_sf("shapefile//merged_Africa_protected_clean.shp")
st_crs(merged_Africa_tranform)

#' what's the area of overlap with the protected areas?
#' this is at 95%
#' should be 170.6318 for X.399745060
#' test it here
intersection_1_95 <-
  st_intersection(hr_isopleths(dbbmm$X.399745060, level = 0.95),
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_1_95)) / 1e6 #' 2709.998 

#' function to calculate for overlap between BB and protected areas
dbbmm_overlap_size <- function (x) {
  return(tryCatch(
    sum(st_area(st_intersection(hr_isopleths(dbbmm[[x]], level = 0.95),
                                merged_Africa_tranform$geometry))) / 1e6 , 
    error = function(e)
      NULL
  ))
}

y <- c(1:length(dbbmm[1]))
#' run it over all of the IDs
pa_overlap <- lapply(y, dbbmm_overlap_size);pa_overlap 
names(pa_overlap) <- names(dbbmm)
pabb_areas <- data.frame(unlist(pa_overlap))
length(pabb_areas$unlist.pa_overlap.)

export_data <- data.frame(cbind(bb_areas, pabb_areas))
#' rename columns
export_data <- export_data %>% rename(bb_area_95 = unlist.areas.)
export_data <- export_data %>% rename(bb_area_95_overlap = unlist.pa_overlap.)
export_data
export_data <- cbind(ID = rownames(export_data), export_data)
rownames(export_data) <- NULL
#' remove the X that gets added to the IDs
levels(as.factor(trk1$id))
export_data$ID <- gsub(pattern = "X", replacement = "#", x = export_data$ID)
export_data$ID <- gsub(pattern = "\\.", replacement = "/", x = export_data$ID)
export_data

write.csv(export_data, file = "summary/brownian/ralph_bb_month.csv")


