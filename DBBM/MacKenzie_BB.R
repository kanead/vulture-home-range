####' Vulture comparative analysis
####' Mackenzie tracks
####' Raw data
####' 17 IDs, 4 WH, 13 WB
####' "6032wh" "6220wh" "6484wh" "6485wh"

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
data_path <- "data_mackenzie"   # path to the data

files <- dir(data_path, pattern = "*.csv") # get file names
length(files)

mydata <- files %>%
  # read in all the files, appending the path before the filename
  map( ~ read_csv(file.path(data_path, .))) %>%
  reduce(rbind)

mydata$long <- as.numeric(mydata$long)
mydata$lat <- as.numeric(mydata$lat)

#' set time zone to UTC
mydata
mydata$time <-
  as.POSIXct(mydata$time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
mydata
attr(mydata$time, "tzone")

#' drop NAs
summary(mydata)
mydata <- mydata %>% drop_na()
summary(mydata)

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

#' remove the erroneous points that occur in the future
length(mydata$time)
mydata <- mydata %>% filter(time < "2020-06-24 12:00:00")
length(mydata$time)

# check the minimum time and the maximum time
min_time <- mydata %>% group_by(id) %>% slice(which.min(time))
data.frame(min_time)

max_time <- mydata %>% group_by(id) %>% slice(which.max(time))
data.frame(max_time)

#' determine the length of time each bird was tracked for
duration <-
  difftime(max_time$time, min_time$time, units = "days")
duration

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
writeRaster(dbbmm, filename='brownian_bridges/mackenzie_dbbm.tif', overwrite=TRUE)
writeRaster(dbbmm, filename='brownian_bridges/mackenzie_dbbm', overwrite=TRUE)

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
st_area(hr_isopleths(dbbmm$X5403wb, level = 0.95)) / 1e6 #' 22751.71 
st_area(hr_isopleths(dbbmm$X5404wb, level = 0.95)) / 1e6 #' 41404.65 

#' load in the cleaned version which is in Albers Equal Area
merged_Africa_tranform <- read_sf("shapefile//merged_Africa_protected_clean.shp")
st_crs(merged_Africa_tranform)

#' what's the area of overlap with the protected areas?
#' this is at 95%
#' should be 9248.909 for X5403wb
#' test it here
intersection_1_95 <-
  st_intersection(hr_isopleths(dbbmm$X5403wb, level = 0.95),
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_1_95)) / 1e6 #' 9248.909 

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

write.csv(export_data, file = "summary/brownian/mackenzie_bb.csv")

####' MANUAL WAY ----

#' convert the brownian bridge raster to vector and calculate its size
#' need to specify the level
X5403wbBB <- hr_isopleths(dbbmm$X5403wb, level = 0.95) 
X5404wbBB <- hr_isopleths(dbbmm$X5404wb, level = 0.95)
X5784wbBB <- hr_isopleths(dbbmm$X5784wb, level = 0.95)
X5785wbBB <- hr_isopleths(dbbmm$X5785wb, level = 0.95)

X5786wbBB <- hr_isopleths(dbbmm$X5786wb, level = 0.95)
X5787wbBB <- hr_isopleths(dbbmm$X5787wb, level = 0.95)
X5788wbBB <- hr_isopleths(dbbmm$X5788wb, level = 0.95)
X5789wbBB <- hr_isopleths(dbbmm$X5789wb, level = 0.95)

X5863wbBB <- hr_isopleths(dbbmm$X5863wb, level = 0.95)
X5864wbBB <- hr_isopleths(dbbmm$X5864wb, level = 0.95)
X6032whBB <- hr_isopleths(dbbmm$X6032wh, level = 0.95)
X6217wbBB <- hr_isopleths(dbbmm$X6217wb, level = 0.95)

X6218wbBB <- hr_isopleths(dbbmm$X6218wb, level = 0.95)
X6219wbBB <- hr_isopleths(dbbmm$X6219wb, level = 0.95)
X6220whBB <- hr_isopleths(dbbmm$X6220wh, level = 0.95)
X6484whBB <- hr_isopleths(dbbmm$X6484wh, level = 0.95)

X6485whBB <- hr_isopleths(dbbmm$X6485wh, level = 0.95)

st_area(X5403wbBB) / 1e6 #' 33202.79
st_area(X5404wbBB) / 1e6 #' 36081.17 
st_area(X5784wbBB) / 1e6 #' 4236.93 
st_area(X5785wbBB) / 1e6 #' 13414.66

st_area(X5786wbBB) / 1e6 #' 45264.45
st_area(X5787wbBB) / 1e6 #' 93500.51
st_area(X5788wbBB) / 1e6 #' 64334.26
st_area(X5789wbBB) / 1e6 #' 22918.64

st_area(X5863wbBB) / 1e6 #' ERROR
st_area(X5864wbBB) / 1e6 #' ERROR
st_area(X6032whBB) / 1e6 #' ERROR
st_area(X6217wbBB) / 1e6 #' 4261.331

st_area(X6218wbBB) / 1e6 #' 65855.15
st_area(X6219wbBB) / 1e6 #' 45598.2
st_area(X6220whBB) / 1e6 #' 11106.48
st_area(X6484whBB) / 1e6 #' 6076.156

st_area(X6485whBB) / 1e6 #' ERROR

#' load in the cleaned version which is in Albers Equal Area
merged_Africa_tranform <- read_sf("shapefile//merged_Africa_protected_clean.shp")
st_crs(merged_Africa_tranform)

#' what's the area of overlap with the protected areas?
#' this is at 95%
intersection_1_95 <-
  st_intersection(X5403wbBB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_1_95)) / 1e6 #' 14073.19 

intersection_2_95 <-
  st_intersection(X5404wbBB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_2_95)) / 1e6 #' 14539.34

intersection_3_95 <-
  st_intersection(X5784wbBB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_3_95)) / 1e6 #' 3520.918 

intersection_4_95 <-
  st_intersection(X5785wbBB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_4_95)) / 1e6 #' 9527.709

intersection_5_95 <-
  st_intersection(X5786wbBB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_5_95)) / 1e6 #' 5717.3

intersection_6_95 <-
  st_intersection(X5787wbBB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_6_95)) / 1e6 #' 45618.65

intersection_7_95 <-
  st_intersection(X5788wbBB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_7_95)) / 1e6 #' 35957.11

intersection_8_95 <-
  st_intersection(X5789wbBB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_8_95)) / 1e6 #' 3633.178

intersection_9_95 <-
  st_intersection(X5863wbBB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_9_95)) / 1e6 #' ERROR

intersection_10_95 <-
  st_intersection(X5864wbBB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_10_95)) / 1e6 #' ERROR

intersection_11_95 <-
  st_intersection(X6032whBB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_11_95)) / 1e6 #' ERROR

intersection_12_95 <-
  st_intersection(X6217wbBB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_12_95)) / 1e6 #' 3956.887

intersection_13_95 <-
  st_intersection(X6218wbBB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_13_95)) / 1e6 #' 35254.96

intersection_14_95 <-
  st_intersection(X6219wbBB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_14_95)) / 1e6 #' 11914.23

intersection_15_95 <-
  st_intersection(X6220whBB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_15_95)) / 1e6 #' 9437.231

intersection_16_95 <-
  st_intersection(X6484whBB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_16_95)) / 1e6 #' 0, YES ZERO

intersection_17_95 <-
  st_intersection(X6485whBB,
                  merged_Africa_tranform$geometry)
sum(st_area(intersection_17_95)) / 1e6 #' ERROR
