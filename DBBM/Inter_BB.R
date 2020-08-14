####' Vulture comparative analysis
####' Intermountain tracks - Kaltenecker
####' 22 indiviuals 10 AWBVs + 12 White headed
####' Raw data

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
data_path <- "data_inter"   # path to the data

files <- dir(data_path, pattern = "*.csv") # get file names
length(files)

mydata <- files %>%
  # read in all the files, appending the path before the filename
  map(~ read_csv(file.path(data_path, .))) %>%
  reduce(rbind)

mydata$long <- as.numeric(mydata$long)
mydata$lat <- as.numeric(mydata$lat)

#' Told that they are in UTC
head(mydata)
mydata$time <-
  as.POSIXct(mydata$time, format = "%Y/%m/%s %H:%M:%S", tz = "UTC")
head(mydata)

attr(mydata$time, "tz")

#' remove underscores from bird IDs
mydata$id <- gsub("_", "", mydata$id)
levels(as.factor(mydata$id))

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

#' remove the erroneous coordinates
mydata <- mydata %>% filter(long > -100)

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

# check the minimum time and the maximum time
min_time <- mydata %>% group_by(id) %>% slice(which.min(time))
data.frame(min_time)

max_time <- mydata %>% group_by(id) %>% slice(which.max(time))
data.frame(max_time)

#' determine the length of time each bird was tracked for
duration <-
  difftime(max_time$time, min_time$time, units = "days")
duration

#' extract the species
species <- max_time$species

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

#' hourly resolution for these data

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
    raster = 1000
    # dimSize = 100,
    # ext = 0.8
  )

#' export the brownian bridge
writeRaster(dbbmm, filename='brownian_bridges/inter_dbbm.tif', overwrite=TRUE)
writeRaster(dbbmm, filename='brownian_bridges/inter_dbbm', overwrite=TRUE)

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
st_area(hr_isopleths(dbbmm$Sanguza, level = 0.95)) / 1e6 #' 21703.99  
st_area(hr_isopleths(dbbmm$Tumaine, level = 0.95)) / 1e6 #' 1922.571

#' load in the cleaned version which is in Albers Equal Area
merged_Africa_tranform <- read_sf("shapefile//merged_Africa_protected_clean.shp")
st_crs(merged_Africa_tranform)

#' what's the area of overlap with the protected areas?
#' this is at 95%
#' should be 6671.142 for Sanguza
#' test it here
intersection_1_95 <-
  st_intersection(hr_isopleths(dbbmm$Sanguza, level = 0.95),
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_1_95)) / 1e6 #' 6671.142 

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
export_data

write.csv(export_data, file = "summary/brownian/inter_bb_raster.csv", row.names = F)
