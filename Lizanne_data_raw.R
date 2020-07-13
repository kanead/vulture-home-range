####' Vulture comparative analysis
####' Lizanne tracks - Lizanne Roxburgh
####' 
####' Raw data 

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

####' load the data ----
data_path <- "data_lizanne"   # path to the data

files <- dir(data_path, pattern = "*.csv") # get file names
length(files)

mydata <- files %>%
  # read in all the files, appending the path before the filename
  map( ~ read_csv(file.path(data_path, .))) %>%
  reduce(rbind)

mydata$long <- as.numeric(mydata$long)
mydata$lat <- as.numeric(mydata$lat)

#' Lizanne tracks are in UTC
head(mydata)
mydata$time <-
  as.POSIXct(mydata$time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
head(mydata)

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
  )  %>%
  nest(-"id")
trk1

#' make sure the nested objects match to that saved in trk1
filter(day_trk, id == "VULT01") %>% summarise(length = length(x_))
filter(day_trk, id == "VULT02") %>% summarise(length = length(x_))

#' summarise the sampling rate
data_summary <-
  day_trk %>% nest(-id) %>% mutate(sr = map(data, summarize_sampling_rate)) %>%
  amt::select(id, sr) %>% unnest %>% arrange(id)
data_summary

#' alternative approach to check sampling rate
day_trk <- day_trk %>%
  group_by(id) %>%
  mutate(timeDiff = c(NA, difftime(tail(t_,-1), head(t_,-1), units = "mins")))
day_trk %>% group_by(id) %>% summarise(
  mean = mean(timeDiff, na.rm = T),
  median = median(timeDiff, na.rm = T),
  max = max(timeDiff, na.rm = T)
)

####' load shape file ----
merged_Africa = read_sf("shapefile//merged_Africa_protected.shp")

#' export Alber Equal Area version of the protected areas shape file
st_crs(merged_Africa) <- 4326
st_crs(merged_Africa)

#' load in the cleaned version which is in Albers Equal Area
merged_Africa_tranform <- read_sf("shapefile//merged_Africa_protected_clean.shp")
st_crs(merged_Africa_tranform)

# st_write(merged_Africa_tranform,  "C:\\Users\\Adam\\Documents\\Science\\Methods & Stats\\GIS\\GIS R code\\Merged\\merged_Africa_protected_albers.shp")

#' plot the protected areas shape file
#' ggplot(merged_Africa) + geom_sf()

####' measure overlap with protected areas on raw data ----
#' how many of the points fall within the protected area shape file
#' this is based on lat long coords
coordinates(day_trk) <- c("x_", "y_")
proj4string(day_trk) <- CRS("+init=epsg:4326")
sf_pts <- st_as_sf(day_trk)

#' do it by bird ID
point_list <-
  split(sf_pts, sf_pts$id)  # split points into list by id

#' can check the raw number of points that fall within a PA
#sf_ov_id <-
#  sapply(split(sf_pts, sf_pts$id), function(x)
#    st_intersects(x, merged_Africa))

#' calculate the number of points overlapping
#for (i in 1:length(sf_ov_id)) {
#  sum_points <- sum(unlist(unclass(sf_ov_id[i])))
#  print(sum_points)
#}

#' calculate the proportion of points overlapping
#for (i in 1:length(sf_ov_id)) {
#  prop_points <-
#    sum(unlist(unclass(sf_ov_id[i]))) / length(point_list[[i]]$t_)
#  print(prop_points)
#}

#' alternative code for proportion
res_prop <- lapply(point_list, function(x)
  length(unlist(st_intersects(x, merged_Africa))) / nrow(x))
unlist(res_prop)

####' home range analyis ----
#' this is based on the data in albers equal area
#' transform back into a track
trk2 <- unnest(trk1) %>% 
  mk_track(
    .,
    .x = x_,
    .y = y_,
    .t = t_,
    id = id,
    crs = CRS(
      "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
    )
  )

#' first get the areas using amt
trk2 <- arrange(trk2, id, t_)

#' KDE 
kde <- trk2 %>% nest(-id) %>%
  mutate(kdearea = map(data, ~ hr_kde(., levels = c(0.5, 0.95)) %>% hr_area)) %>%
  dplyr::select(id, kdearea) %>% unnest()

kde$area <-  kde$area / 1000000
kde_95 <- kde %>% filter(level == 0.95) %>% arrange(id)
kde_95
kde_50 <- kde %>% filter(level == 0.5) %>% arrange(id)
kde_50

#' here for mcp 
mcps <- trk2 %>% nest(-id) %>%
  mutate(mcparea = map(data, ~ hr_mcp(., levels = c(0.5, 0.95)) %>% hr_area)) %>%
  dplyr::select(id, mcparea) %>% unnest()

mcps$area <- mcps$area / 1000000
mcp_95 <- mcps %>% filter(level == 0.95) %>% arrange(id)
mcp_95
mcp_50 <- mcps %>% filter(level == 0.5) %>% arrange(id)
mcp_50
#' now get the shapes for KDE
kde_shape_1 <- trk2 %>% filter(id == "VULT01") %>%
  hr_kde(., levels = c(0.95))
kde_shape_2 <- trk2 %>% filter(id == "VULT02") %>%
  hr_kde(., levels = c(0.95))

#' check that areas match
hr_area(kde_shape_1) / 1000000
hr_area(kde_shape_2) / 1000000

#' plot the KDEs
plot(kde_shape_1$ud)
plot(kde_shape_2$ud)

#' what are the smoothing parameters?
kde_shape_1$h[1]
kde_shape_2$h[1]

#####' measure overlap with protected areas ----
#' 95% KDE
L_95 <- lapply( unique( trk2$id ), function(x) {
  track_id <-  trk2[ trk2$id == x, ]
  track_object <- track_id %>% hr_kde(., levels = c(.95)) %>% hr_isopleths(.)
  area_totals <- track_object %>% st_intersection( merged_Africa_tranform ) %>% st_area()
  sum( area_totals ) / 1e6
}) 
#set names
names(L_95) <- unique( trk2$id )

L_95

#' 50% KDE
L_50 <- lapply( unique( trk2$id ), function(x) {
  track_id <-  trk2[ trk2$id == x, ]
  track_object <- track_id %>% hr_kde(., levels = c(.5)) %>% hr_isopleths(.)
  area_totals <- track_object %>% st_intersection( merged_Africa_tranform ) %>% st_area()
  sum( area_totals ) / 1e6
}) 
#set names
names(L_50) <- unique( trk2$id )

L_50

L_95[1]
L_50[1]

#' export the rasters based on the raw data points
#' these are in Albers Equal Area https://epsg.io/102022
#' writeRaster(kde_shape_1$ud, "KDE_170948_95_raw.tif")
#' writeRaster(kde_shape_2$ud, "KDE_170948_95_raw.tif")

#####' export summary stats ----
#' combine the summary stats
data_summary$duration <- duration
data_summary$min_time <- min_time$time
data_summary$max_time <- max_time$time
data_summary$kde_95_raw <- kde_95$area
data_summary$mcps_95_raw <- mcp_95$area
data_summary$kde_50_raw <- kde_50$area
data_summary$mcps_50_raw <- mcp_50$area
data_summary$kde_95_overlap <- unlist(as.numeric(L_95))
data_summary$kde_50_overlap <- unlist(as.numeric(L_50))
data_summary$species<-ifelse((data_summary$id == "VULT02") | 
                               (data_summary$id == "VULT05") | 
                               (data_summary$id == "VULT07") |
                               (data_summary$id == "VULT09") |
                               (data_summary$id == "VULT10") |
                               (data_summary$id == "VULT16") |
                               (data_summary$id == "VULT19") |
                               (data_summary$id == "VULT11"), "wb", "cv")
data_summary$study <- "Lizanne"
data_summary$overlap_raw <- unlist(res_prop)
data_summary$region <- "south"
data_summary

#' can export this data summary
write.csv(data_summary, file = "summary/lizanne_data_summary.csv", row.names = FALSE)
