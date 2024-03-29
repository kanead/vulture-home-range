####' Vulture comparative analysis
####' Schabo tracks
####' Reg data
####' all Cape vultures, 16 IDs

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

####' load the data ----
data_path <- "data_schabo"   # path to the data

files <- dir(data_path, pattern = "*.csv") # get file names
length(files)

mydata <- files %>%
  # read in all the files, appending the path before the filename
  map( ~ read_csv(file.path(data_path, .))) %>%
  reduce(rbind)

mydata$long <- as.numeric(mydata$long)
mydata$lat <- as.numeric(mydata$lat)

#' Tracks are assumed to be in UTC
head(mydata)
mydata$time <-
  as.POSIXct(mydata$time, format = "%d/%m/%Y %H:%M", tz = "UTC")
head(mydata)

attr(mydata$time, "tz")

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

#' Now it is easy to calculate day/night with either movement track
trk <- trk %>% time_of_day()

#' can remove the night time points as follows:
day_trk <- filter(trk, tod_ == "day") %>% arrange(id, t_)

#' summarise the sampling rate
data_summary <-
  day_trk %>% nest(-id) %>% mutate(sr = map(data, summarize_sampling_rate)) %>%
  amt::select(id, sr) %>% unnest %>% arrange(id)
data_summary

####' data regularization ----

#' measure the time difference between points for each bird ID using dplyr
#' - Group your data by ID
#' - Compute time diffs between each timestamp in your group (the 1st time diff is NA)
#' - Create a new ID that counts no. of prior time gaps that are large 
#' - Split the ID into newID by using an underscore separator at large gaps

length(levels(as.factor(trk$id)))
#' specify the time difference to break the track at
time_difference <- 120 #' the units relate to those set in difftime
#' need to add the arrange function here otherwise the order gets messed up
trk2 <- day_trk %>%
  group_by(id) %>%
  mutate(timeDiff = c(NA, difftime(tail(t_,-1), head(t_,-1), units = "mins"))) %>%
  mutate(newID = paste(id, cumsum(!is.na(timeDiff) &
                                    timeDiff > time_difference), sep = "_")) %>% 
  arrange(id, t_) %>%
  ungroup()
head(trk2)
tail(trk2)

#' check the number of newIDs
length(levels(as.factor(trk2$newID)))

#' create a trajectory object using adehabitatLT
trk_ltraj <-
  as.ltraj(xy = trk2[, c("x_", "y_")],
           date = trk2$t_,
           id = trk2$newID)
head(trk_ltraj)

#' rediscretization of the trajectory
#'  time step we want for the rediscretization, in seconds
tstep <- 3600 # 3600 secs = 1 hour
newtr <- redisltraj(trk_ltraj, u = tstep, type = "time")
head(newtr[1])
head(newtr[2])
class(newtr)

#' convert to class data frame
trk3 <- ld(newtr)
head(trk3)
class(trk3$date)

#' group the IDs that were split if they had big gaps back together into their original ID structure
#' this involves accessing the name of the new ID that occurs before the underscore
trk3 <- separate(trk3,
                 col = id,
                 sep = "_",
                 into = c("ID", "NA"))
head(trk3)
levels(as.factor(trk3$ID))
length(levels(as.factor(trk3$ID)))

#' remove the resultant NA column that occurs after the split
trk3 <- dplyr::select(trk3, x, y, date, ID) %>% arrange(as.numeric(ID), date)
head(trk3)
tail(trk3)
class(trk3$date)
levels(as.factor(trk3$ID))

#' turn it back into a track objcet with lat/long
trk4 <-
  mk_track(
    trk3,
    .x = x,
    .y = y,
    .t = date,
    id = ID,
    crs = CRS("+init=epsg:4326")
  )
trk4 <- trk4 %>% arrange(id,t_)

#' save the lat-long track
trk_latlong <- trk4

####' load shape file ----
merged_Africa = read_sf("shapefile//merged_Africa_protected.shp")

#' load in the cleaned version which is in Albers Equal Area
merged_Africa_tranform <- read_sf("shapefile//merged_Africa_protected_clean.shp")
st_crs(merged_Africa_tranform)

####' measure overlap with protected areas on regularised data ----
#' how many of the points fall within the protected area shape file
#' this is based on lat long coords
coordinates(trk_latlong) <- c("x_", "y_")
proj4string(trk_latlong) <- CRS("+init=epsg:4326")
sf_pts <- st_as_sf(trk_latlong)

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
#' need data in Albers equal area


#' transform back into a track
trk_albers <-
  mk_track(
    trk4,
    .x = x_,
    .y = y_,
    .t = t_,
    id = id,
    crs = CRS("+init=epsg:4326"))  %>%
  transform_coords(
    sp::CRS( #' we can transform the CRS of the data to an equal area projection
      #' https://epsg.io/102022
      "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
    )
  )

#' first get the areas using amt
trk_albers <- arrange(trk_albers, id, t_)

#' export the regularised track 
# write.csv(x = trk_albers, file = "regularised/schabo_albers_reg.csv", row.names = FALSE)

#' KDE
kde <- trk_albers %>% nest(-id) %>%
  mutate(kdearea = map(data, ~ hr_kde(., levels = c(0.5, 0.95)) %>% hr_area)) %>%
  dplyr::select(id, kdearea) %>% unnest()

kde$area <-  kde$area / 1000000
kde_95 <- kde %>% filter(level == 0.95) %>% arrange(id)
kde_95
kde_50 <- kde %>% filter(level == 0.5) %>% arrange(id)
kde_50

#' here for mcp
mcps <- trk_albers %>% nest(-id) %>%
  mutate(mcparea = map(data, ~ hr_mcp(., levels = c(0.5, 0.95)) %>% hr_area)) %>%
  dplyr::select(id, mcparea) %>% unnest()

mcps$area <- mcps$area / 1000000
mcp_95 <- mcps %>% filter(level == 0.95) %>% arrange(id)
mcp_95
mcp_50 <- mcps %>% filter(level == 0.5) %>% arrange(id)
mcp_50


#' now get the shapes for KDE
kde_shape_1 <- trk_albers %>% filter(id == "Cerhu") %>%
  hr_kde(., levels = c(0.95))
kde_shape_2 <- trk_albers %>% filter(id == "Emma") %>%
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
L_95 <- lapply( unique( trk_albers$id ), function(x) {
  track_id <-  trk_albers[ trk_albers$id == x, ]
  track_object <- track_id %>% hr_kde(., levels = c(.95)) %>% hr_isopleths(.)
  area_totals <- track_object %>% st_intersection( merged_Africa_tranform ) %>% st_area()
  sum( area_totals ) / 1e6
}) 
#set names
names(L_95) <- unique( trk_albers$id )

L_95

#' 50% KDE
L_50 <- lapply( unique( trk_albers$id ), function(x) {
  track_id <-  trk_albers[ trk_albers$id == x, ]
  track_object <- track_id %>% hr_kde(., levels = c(.5)) %>% hr_isopleths(.)
  area_totals <- track_object %>% st_intersection( merged_Africa_tranform ) %>% st_area()
  sum( area_totals ) / 1e6
}) 
#set names
names(L_50) <- unique( trk_albers$id )

L_50

L_95[1]
L_50[1]

#' export the rasters based on the regularised data points
#' these are in Albers Equal Area https://epsg.io/102022
#' writeRaster(kde_shape_1$ud, "KDE_5011_95_reg.tif")
#' writeRaster(kde_shape_2$ud, "KDE_5019_95_reg.tif")

#####' export summary stats ----
#' combine the summary stats
#' arrange them in the right order first

data_summary <- read_csv("summary/schabo_data_summary.csv", col_names = T)
data_summary$kde_95_reg <- kde_95$area
data_summary$mcps_95_reg <- mcp_95$area
data_summary$kde_50_reg <- kde_50$area
data_summary$mcps_50_reg <- mcp_50$area
data_summary$overlap_reg <- unlist(res_prop)
data_summary$kde_95_overlap_reg <- unlist(as.numeric(L_95))
data_summary$kde_50_overlap_reg <- unlist(as.numeric(L_50))

#' can export this data summary
#' append the raw data file
write.table(data_summary, "summary/schabo_data_summary.csv", sep = ",", row.names=F)

