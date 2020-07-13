####' Vulture comparative analysis
####' Spiegel tracks - Orr Spiegel 
####' 15 individuals
####' Raw data
####' 12- or 13-hour duty cycle starting at 7 AM local time; GPS fixes every 10 min

#' remove all
rm(list = ls())
graphics.off()

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
data_path <- "data_spiegel"   # path to the data

files <- dir(data_path, pattern = "*.csv") # get file names
length(files)

mydata <- files %>%
  # read in all the files, appending the path before the filename
  map(~ read_csv(file.path(data_path, .))) %>%
  reduce(rbind)

mydata$long <- as.numeric(mydata$long)
mydata$lat <- as.numeric(mydata$lat)

#' Tracks are meant to go from 7am - 7pm
#' data show tracks go from 6am - 6pm
slice(mydata,45:55)
mydata$time <-
  as.POSIXct(mydata$time, format = "%Y/%m/%d %H:%M:%S", tz = "Africa/Algiers")
slice(mydata,45:55)

#' change from a UTC+1 TZ to UTC
attr(mydata$time, "tzone") <- "UTC"
slice(mydata,45:55)
attr(mydata$time, "tzone")

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
mydata <- dplyr::select(mydata, time, lat, long, id, species, study)
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

#' export the brownian bridge
writeRaster(dbbmm, filename='brownian_bridges/spiegel_dbbm.tif', overwrite=TRUE)
writeRaster(dbbmm, filename='brownian_bridges/spiegel_dbbm', overwrite=TRUE)

plot(dbbmm)

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
st_area(hr_isopleths(dbbmm$X1, level = 0.95)) / 1e6 #' 3925.874 
st_area(hr_isopleths(dbbmm$X2, level = 0.95)) / 1e6 #' 7122.582 

#' load in the cleaned version which is in Albers Equal Area
merged_Africa_tranform <- read_sf("shapefile//merged_Africa_protected_clean.shp")
st_crs(merged_Africa_tranform)

#' what's the area of overlap with the protected areas?
#' this is at 95%
#' should be 5282.431 for X170948
#' test it here
intersection_1_95 <-
  st_intersection(hr_isopleths(dbbmm$X1, level = 0.95),
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_1_95)) / 1e6 #' 0km2 

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
export_data$ID <- gsub(pattern = "X", replacement = "", x = export_data$ID)
export_data

write.csv(export_data, file = "summary/brownian/spiegel_bb.csv")

####' OLD WAY ----
#' convert the brownian bridge raster to vector and calculate its size
#' need to specify the level
X1BB <- hr_isopleths(dbbmm$X1, level = 0.95)
X2BB <- hr_isopleths(dbbmm$X2, level = 0.95)
X3BB <- hr_isopleths(dbbmm$X3, level = 0.95)
X4BB <- hr_isopleths(dbbmm$X4, level = 0.95)

X5BB <- hr_isopleths(dbbmm$X5, level = 0.95)
X6BB <- hr_isopleths(dbbmm$X6, level = 0.95)
X7BB <- hr_isopleths(dbbmm$X7, level = 0.95)
X8BB <- hr_isopleths(dbbmm$X8, level = 0.95)

X9BB <- hr_isopleths(dbbmm$X9, level = 0.95)
X10BB <- hr_isopleths(dbbmm$X10, level = 0.95)
X11BB <- hr_isopleths(dbbmm$X11, level = 0.95)
X12BB <- hr_isopleths(dbbmm$X12, level = 0.95)

X13BB <- hr_isopleths(dbbmm$X13, level = 0.95)
X14BB <- hr_isopleths(dbbmm$X14, level = 0.95)
X15BB <- hr_isopleths(dbbmm$X15, level = 0.95)

st_area(X1BB) / 1e6 #' 10536.44km2
st_area(X2BB) / 1e6 #' 3528.454km2
st_area(X3BB) / 1e6 #' 6266.703km2
st_area(X4BB) / 1e6 #' 1252.918km2

st_area(X5BB) / 1e6 #' 4824.341km2
st_area(X6BB) / 1e6 #' 1184.891km2
st_area(X7BB) / 1e6 #' 14551.46km2
st_area(X8BB) / 1e6 #' 1843.064km2

st_area(X9BB) / 1e6 #' 33468.38km2
st_area(X10BB) / 1e6 #' 9006.094km2
st_area(X11BB) / 1e6 #' 5621.669km2
st_area(X12BB) / 1e6 #' 22048.2km2

st_area(X13BB) / 1e6 #' 10147.03km2
st_area(X14BB) / 1e6 #' 19929.37km2
st_area(X15BB) / 1e6 #' 26320.07km2

#' load in the cleaned version which is in Albers Equal Area
merged_Africa_tranform <- read_sf("shapefile//merged_Africa_protected_clean.shp")
st_crs(merged_Africa_tranform)

#' what's the area of overlap with the protected areas?
#' this is at 95%
intersection_1_95 <-
  st_intersection(X1BB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_1_95)) / 1e6 #' 2315.194

intersection_2_95 <-
  st_intersection(X2BB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_2_95)) / 1e6 #' 3015.178

intersection_3_95 <-
  st_intersection(X3BB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_3_95)) / 1e6 #' 3517.423

intersection_4_95 <-
  st_intersection(X4BB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_4_95)) / 1e6 #' 938.0902

intersection_5_95 <-
  st_intersection(X5BB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_5_95)) / 1e6 #' 2076.745

intersection_6_95 <-
  st_intersection(X6BB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_6_95)) / 1e6 #' 323.8602

intersection_7_95 <-
  st_intersection(X7BB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_7_95)) / 1e6 #' 6493.599

intersection_8_95 <-
  st_intersection(X8BB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_8_95)) / 1e6 #' 1150.226

intersection_9_95 <-
  st_intersection(X9BB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_9_95)) / 1e6 #' 9590.968

intersection_10_95 <-
  st_intersection(X10BB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_10_95)) / 1e6 #' 6065.834

intersection_11_95 <-
  st_intersection(X11BB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_11_95)) / 1e6 #' 4331.537

intersection_12_95 <-
  st_intersection(X12BB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_12_95)) / 1e6 #' 9285.137

intersection_13_95 <-
  st_intersection(X13BB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_13_95)) / 1e6 #' 3789.282

intersection_14_95 <-
  st_intersection(X14BB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_14_95)) / 1e6 #' 10948.21

intersection_15_95 <-
  st_intersection(X15BB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_15_95)) / 1e6 #' 14313.09

