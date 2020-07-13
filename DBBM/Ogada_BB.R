####' Vulture comparative analysis
####' Ogada tracks
####' Raw data
####' 13 individuals

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
data_path <- "data_ogada"   # path to the data

files <- dir(data_path, pattern = "*.csv") # get file names
length(files)

mydata <- files %>%
  # read in all the files, appending the path before the filename
  map( ~ read_csv(file.path(data_path, .))) %>%
  reduce(rbind)

mydata$long <- as.numeric(mydata$long)
mydata$lat <- as.numeric(mydata$lat)

mydata
mydata$time <-
  as.POSIXct(mydata$time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
mydata
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

#' get rid of NAs 
summary(mydata)
mydata <- mydata %>% na.omit()
summary(mydata)

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
summary(mydata)

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
    window.size = 311,
    margin = 111,
    dimSize = 100,
    ext = 0.8
  )

#' export the brownian bridge
writeRaster(dbbmm, filename='brownian_bridges/ogada_dbbm.tif', overwrite=TRUE)
writeRaster(dbbmm, filename='brownian_bridges/ogada_dbbm', overwrite=TRUE)

plot(dbbmm)

#' convert the brownian bridge raster to vector and calculate its size
#' need to specify the level
AjaliBB <- hr_isopleths(dbbmm$Ajali, level = 0.95) 
#' Error in raster::rasterToContour(cumulative_ud(x), level = level) : no contour lines
BastardBB <- hr_isopleths(dbbmm$Bastard, level = 0.95)
ChickenBB <- hr_isopleths(dbbmm$Chicken, level = 0.95)
DutchBB <- hr_isopleths(dbbmm$Dutch, level = 0.95)

EasyBB <- hr_isopleths(dbbmm$Easy, level = 0.95)
FrancosBB <- hr_isopleths(dbbmm$Francos, level = 0.95)
MelakoBB <- hr_isopleths(dbbmm$Melako, level = 0.95)
MpoleBB <- hr_isopleths(dbbmm$Mpole, level = 0.95)

RainBB <- hr_isopleths(dbbmm$Rain, level = 0.95)
SantaBB <- hr_isopleths(dbbmm$Santa, level = 0.95)
TapikaBB <- hr_isopleths(dbbmm$Tapika, level = 0.95)
ThreeinoneBB <- hr_isopleths(dbbmm$Threeinone, level = 0.95)

TwoBB <- hr_isopleths(dbbmm$Two, level = 0.95)

st_area(AjaliBB) / 1e6 #' Gave error - see above
st_area(BastardBB) / 1e6 #' 4975.883km2
st_area(ChickenBB) / 1e6 #' 7605.377km2
st_area(DutchBB) / 1e6 #' 13099.35km2

st_area(EasyBB) / 1e6 #' 10999.74km2
st_area(FrancosBB) / 1e6 #' 964.3538km2
st_area(MelakoBB) / 1e6 #' 15753.95km2
st_area(MpoleBB) / 1e6 #' 115.9325km2

st_area(RainBB) / 1e6 #' 6263.624km2
st_area(SantaBB) / 1e6 #' 3885.365km2
st_area(TapikaBB) / 1e6 #' 2535.175km2
st_area(ThreeinoneBB) / 1e6 #' 4773.387km2

st_area(TwoBB) / 1e6 #' 4857.122km2

#' load in the cleaned version which is in Albers Equal Area
merged_Africa_tranform <- read_sf("shapefile//merged_Africa_protected_clean.shp")
st_crs(merged_Africa_tranform)

#' what's the area of overlap with the protected areas?
#' this is at 95%
intersection_1_95 <-
  st_intersection(AjaliBB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_1_95)) / 1e6 #' error - see above

intersection_2_95 <-
  st_intersection(BastardBB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_2_95)) / 1e6 #' 4972.636

intersection_3_95 <-
  st_intersection(ChickenBB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_3_95)) / 1e6 #' 6937.709

intersection_4_95 <-
  st_intersection(DutchBB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_4_95)) / 1e6 #' 6854.667

intersection_5_95 <-
  st_intersection(EasyBB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_5_95)) / 1e6 #' 5037.327

intersection_6_95 <-
  st_intersection(FrancosBB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_6_95)) / 1e6 #' 82.46754

intersection_7_95 <-
  st_intersection(MelakoBB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_7_95)) / 1e6 #' 12782.54

intersection_8_95 <-
  st_intersection(MpoleBB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_8_95)) / 1e6 #' 0, yes zero!

intersection_9_95 <-
  st_intersection(RainBB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_9_95)) / 1e6 #' 1495.001

intersection_10_95 <-
  st_intersection(SantaBB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_10_95)) / 1e6 #' 2344.834

intersection_11_95 <-
  st_intersection(TapikaBB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_11_95)) / 1e6 #' 265.5762

intersection_12_95 <-
  st_intersection(ThreeinoneBB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_12_95)) / 1e6 #' 1835.658

intersection_13_95 <-
  st_intersection(TwoBB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_13_95)) / 1e6 #' 4253.291
