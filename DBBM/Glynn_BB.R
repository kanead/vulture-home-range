####' Vulture comparative analysis
####' Glynn Maude tracks
####' Raw data
####' 2 Cape vultures, 2 AWBVs = 4 IDs

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
data_path <- "data_glynn"   # path to the data

files <- dir(data_path, pattern = "*.csv") # get file names
length(files)

mydata <- files %>%
  # read in all the files, appending the path before the filename
  map(~ read_csv(file.path(data_path, .))) %>%
  reduce(rbind)

mydata$long <- as.numeric(mydata$long)
mydata$lat <- as.numeric(mydata$lat)

#' Tracks are assumed to be in UTC
head(mydata)
mydata$time <-
  as.POSIXct(mydata$time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
head(mydata)

attr(mydata$time, "tz")

#' id column is a factor
mydata$id <- as.factor(mydata$id)
mydata <- mydata %>% arrange(id, time)
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

#' remove NAs
summary(mydata)
mydata <- drop_na(mydata)
summary(mydata)

mydata <- dplyr::filter(mydata, lat > 0 | long > 0)

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
writeRaster(dbbmm, filename='brownian_bridges/glynn_dbbm.tif', overwrite=TRUE)
writeRaster(dbbmm, filename='brownian_bridges/glynn_dbbm', overwrite=TRUE)

plot(dbbmm)
contour(dbbmm, add=T, levels=c(.5,.95))

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
st_area(hr_isopleths(dbbmm$X128502, level = 0.95)) / 1e6 #' 95824.23 
st_area(hr_isopleths(dbbmm$X136377, level = 0.95)) / 1e6 #' 28335.66

#' load in the cleaned version which is in Albers Equal Area
merged_Africa_tranform <- read_sf("shapefile//merged_Africa_protected_clean.shp")
st_crs(merged_Africa_tranform)

#' what's the area of overlap with the protected areas?
#' this is at 95%
#' should be 19225.29 for X128502
#' test it here
intersection_1_95 <-
  st_intersection(hr_isopleths(dbbmm$X128502, level = 0.95),
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_1_95)) / 1e6 #' 19225.29 

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

write.csv(export_data, file = "summary/brownian/glynn_bb.csv")

####' MANUAL WAY ----

#' extract for each ID
dbbmm$X128502
dbbmm$X136377
dbbmm$X137296
dbbmm$X1120042


#' convert the brownian bridge raster to vector and calculate its size
#' need to specify the level
X128502BB <- hr_isopleths(dbbmm$X128502, level = 0.95)
X136377BB <- hr_isopleths(dbbmm$X136377, level = 0.95)
X137296BB <- hr_isopleths(dbbmm$X137296, level = 0.95)
X1120042BB <- hr_isopleths(dbbmm$X1120042, level = 0.95)
st_area(X128502BB) / 1e6 #' 89585.55km2
st_area(X136377BB) / 1e6 #' 23840.59km2
st_area(X137296BB) / 1e6 #' 125925.9km2
st_area(X1120042BB) / 1e6 #' 69094.73km2

#' load in the cleaned version which is in Albers Equal Area
merged_Africa_tranform <- read_sf("shapefile//merged_Africa_protected_clean.shp")
st_crs(merged_Africa_tranform)

#' what's the area of overlap with the protected areas?
#' this is at 95%
intersection_1_95 <-
  st_intersection(X128502BB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_1_95)) / 1e6 #' 18674.23km2

intersection_2_95 <-
  st_intersection(X136377BB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_2_95)) / 1e6 #' 1.069568km2

intersection_3_95 <-
  st_intersection(X137296BB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_3_95)) / 1e6 #' 7919.018km2

intersection_4_95 <-
  st_intersection(X1120042BB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_4_95)) / 1e6 #' 3415.732km2
