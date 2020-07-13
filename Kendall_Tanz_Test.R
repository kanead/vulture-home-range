#' Corinne Kendall Tanzania data test

#' I calculate the 50% and 95% KDE for bird ID #109018692
#' I do so using amt AND adehabitatHR
#' And I compare values from the full track to values with just diurnal points

#' remove all
rm(list = ls())
graphics.off()

#' Load the required packages
library(tidyverse)
library(amt)
library(SDLfilter)
library(adehabitatHR)
library(sp)

####' load the data ----
data_path <- "data_kendall_tanz"   # path to the data

files <- dir(data_path, pattern = "*.csv") # get file names
length(files)

mydata <- files %>%
  # read in all the files, appending the path before the filename
  map(~ read_csv(file.path(data_path, .))) %>%
  reduce(rbind)

#' make sure the coordinates are recognised as numbers
mydata$long <- as.numeric(mydata$long)
mydata$lat <- as.numeric(mydata$lat)

#' Tracks are in UTC
head(mydata)
mydata$time <-
  as.POSIXct(mydata$time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
head(mydata)

#' verify time zone
attr(mydata$time, "tzone")

#' pull out one id to work with
mydata <- filter(mydata, id == "#109018692")

#' turn it into a track
#' transform from lat long to albers
trk <-
  mk_track(
    mydata,
    .x = long,
    .y = lat,
    .t = time,
    crs = CRS("+init=epsg:4326")
  )  %>%
  transform_coords(
    sp::CRS(
      "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
    )
  )
trk

#' calculate home range area for 50% and 95% KDE
kde_shape_1 <- trk %>%
  hr_kde(., levels = c(0.5, 0.95))
hr_area(kde_shape_1) / 1e6 #' area is in km2

# 5668.289 km2 for 50% KDE
# 21908.184 km2 for 95% KDE

####' compare to adehabitatHR, an alternative means to measure HR ----
# Set the coordinate reference system (CRS)
mydata.sp <- dplyr::select(trk, x_, y_)

# Define the coordinates
coordinates(mydata.sp) <- c("x_", "y_")

proj4string(mydata.sp) <-
  CRS(
    "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  )

kernel.ref <-
  kernelUD(mydata.sp, h = "href")  # href = the reference bandwidth
image(kernel.ref) # plot

# measure the size of the kernels for 50 and 95%
mydata.kernel.poly_50 <-
  getverticeshr(kernel.ref, percent = 50, unout = "km2")
mydata.kernel.poly_95 <-
  getverticeshr(kernel.ref, percent = 95, unout = "km2")

#' show the areas
print(mydata.kernel.poly_50)
print(mydata.kernel.poly_95)

#' 5512.846 km2 for 50% KDE
#' 22612.29 km2 for 95% KDE

####' try with cleaned, diurnal data ----

#' clean the data
#' Check for duplicated observations (ones with same lat, long, timestamp,
#'  and individual identifier).
ind2 <- mydata %>% dplyr::select(time, long, lat) %>%
  duplicated
sum(ind2)

#' remove them
mydata$dups <- ind2
mydata <- filter(mydata, dups == "FALSE")
mydata

#' drop the NAs
mydata <- mydata %>% drop_na(long, lat)

#' filter extreme data based on a speed threshold based on vmax which is km/hr
#' time needs to be labelled DateTime for these functions to work
names(mydata)[names(mydata) == 'time'] <- 'DateTime'
SDLfilterData <-
  ddfilter.speed(data.frame(mydata), vmax = 100, method = 1)
length(SDLfilterData$DateTime)
head(SDLfilterData)

#' rename everything as before
mydata <- SDLfilterData
names(mydata)[names(mydata) == 'DateTime'] <- 'time'

#' turn the cleaned data into a trk
trk_day <-  mk_track(
  mydata,
  .x = long,
  .y = lat,
  .t = time,
  crs = CRS("+init=epsg:4326")
)
trk_day

#' order by date
trk_day <- trk_day %>% arrange(t_)

#' Now it is easy to calculate day/night with either movement track
trk_day <- trk_day %>% time_of_day()

#' can remove the night time points as follows:
trk_day <- filter(trk_day, tod_ == "day") %>% arrange(t_)

#' again convet coords into albers equal area so units are in metres
trk_day_albers <- trk_day %>%
  transform_coords(
    sp::CRS(
      #' we can transform the CRS of the data to an equal area projection
      #' https://epsg.io/102022
      "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
    )
  )
trk_day_albers

#' calculate home range area for 50% and 95% KDE for the diurnal points
kde_shape_2 <- trk_day_albers %>%
  hr_kde(., levels = c(0.5, 0.95))
hr_area(kde_shape_2) / 1e6 #' area is in km2

# 5978.05 km2 for 50% KDE
# 22646.75 km2 for 95% KDE

####' again compare to adehabitatHR, an alternative means to measure HR ----
# Set the coordinate reference system (CRS)
mydata.sp <- dplyr::select(trk_day_albers, x_, y_)

# Define the coordinates
coordinates(mydata.sp) <- c("x_", "y_")

proj4string(mydata.sp) <-
  CRS(
    "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  )

kernel.ref <-
  kernelUD(mydata.sp, h = "href")  # href = the reference bandwidth
image(kernel.ref) # plot

# measure the size of the kernels for 50 and 95%
mydata.kernel.poly_50 <-
  getverticeshr(kernel.ref, percent = 50, unout = "km2")
mydata.kernel.poly_95 <-
  getverticeshr(kernel.ref, percent = 95, unout = "km2")

#' show the areas
print(mydata.kernel.poly_50)
print(mydata.kernel.poly_95)

# 5798.922 km2 for 50% KDE
# 23364.24 km2 for 95% KDE

#' export track with diurnal points
diurnal_109018692 <- dplyr::select(trk_day, t_, x_, y_)

#' rename columns
diurnal_109018692 <- diurnal_109018692 %>% rename(time = t_)
diurnal_109018692 <- diurnal_109018692 %>% rename(long = x_)
diurnal_109018692 <- diurnal_109018692 %>% rename(lat = y_)

#' write the csv file
write.csv(diurnal_109018692, "diurnal_109018692.csv", row.names = F)
