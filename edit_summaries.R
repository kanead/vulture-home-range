#' Combine summaries

#' remove all
rm(list = ls())
graphics.off()

#' Load the required packages
library(tidyverse)

####' load the data ----
monthly <-
  read_csv("summary//modified summaries//monthly_data_summary.csv",
           col_names = T)
yearly <-
  read_csv("summary//modified summaries//yearly_data_summary.csv",
           col_names = T)
breeding <-
  read_csv("summary/modified summaries/breeding_data_summary.csv",
           col_names = T)

head(monthly)
head(yearly)
head(breeding)

####' final clean ----
#' Some of Buechley's data is actually located in Southern Africa 
#' and needs to have region changed to south:

south_buechley <- c(
  "Fringilla",
  "Lemba",
  "Lizzy",
  "Mubanga",
  "Precision",
  "Timbavati",
  "WBVUSA01",
  "WBVUSA02"
)

#' do it for yearly data
filter(yearly, study == "Buechley") %>%
  group_by(id) %>%
  slice(1) %>%
  dplyr::select(id, study, region)

yearly$region <-
  ifelse(yearly$id %in% south_buechley, "south", yearly$region)

filter(yearly, study == "Buechley") %>%
  group_by(id) %>%
  slice(1) %>%
  dplyr::select(id, study, region)

#' now for monthly data
filter(monthly, study == "Buechley") %>%
  group_by(bird) %>%
  slice(1) %>%
  dplyr::select(bird, study, region)

monthly$region <-
  ifelse(monthly$bird %in% south_buechley, "south", monthly$region)

filter(monthly, study == "Buechley") %>%
  group_by(bird) %>%
  slice(1) %>%
  dplyr::select(bird, study, region)

#' remove #834450834 and #834451702 from Kendall Tanz data
#' for yearly data
length(levels(as.factor(yearly$id)))

`%notin%` <- Negate(`%in%`)
yearly <- yearly[yearly$id %notin% c("#834450834", "#834451702"),]

length(levels(as.factor(yearly$id)))

#' for monthly data
length(levels(as.factor(monthly$bird)))

monthly <- monthly[monthly$bird %notin% c("#834450834", "#834451702"),]

length(levels(as.factor(monthly$bird)))

filter(monthly, bird == "#834450834")
filter(monthly, bird == "#834451702")

####' add the age column to the monthly data using ids ----
#' this is called id in the yearly data and bird in the monthly data
monthly$age <- yearly$age[match(monthly$bird, yearly$id)]
monthly$age

####' add the breeding data the monthly data summary ----
#' here for the monthly data
monthly$breeding_days <-
  breeding$time[match(monthly$id, breeding$id)]

monthly %>% dplyr::select(id, breeding_days, study)

#####' determine whether breeding according to age and breeding_days ----
#' immature birds hanging around an area could affect this so we need to remove them
#' first for monthly data
monthly$breeder <-
  if_else(monthly$breeding_days > 8 & monthly$age == "adult", 1, 0)

#' stick on whether the bird was breeding to the yearly data
#' first subset to the data of the birds that were breeding
breeding_birds <- monthly %>% dplyr::select(bird, breeder) %>% 
dplyr::filter(breeder == 1)
#' and extract their names
breeding_birds <- levels(as.factor(breeding_birds$bird))

#' now add that on to the yearly data
yearly$breeder <- 
  ifelse(yearly$id %in% breeding_birds, 1, 0)

#' NAs appear when the record is outside the breeding season or when the bird dropped out
#' of the analysis going from yearly to monthly

#' extract the Gyps vultures
levels(as.factor(yearly$species))
yearly <- yearly %>% filter(species == "cv" | species == "wb" | species == "rv")
monthly <- monthly %>% filter(species == "cv" | species == "wb" | species == "rv")

####' make some plots ----
#' http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization

ggplot(data = yearly, mapping = aes(x = kde_95_raw, fill = age)) + geom_histogram() + 
  facet_wrap( ~species)

ggplot(data = yearly, mapping = aes(x = kde_95_raw, fill = region)) + geom_histogram() + 
  facet_wrap( ~species)

ggplot(data = yearly, mapping = aes(x = kde_95_raw, fill = as.factor(breeder))) + geom_histogram() + 
  facet_wrap( ~species)

head(monthly[,1])
head(yearly[,1])

#####' export these data ----
write.csv(monthly, file = "summary//modified summaries//monthly_w_breeding.csv", row.names = FALSE)
write.csv(yearly, file = "summary//modified summaries//yearly_w_breeding.csv", row.names = FALSE)
