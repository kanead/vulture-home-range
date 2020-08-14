#' Combine summaries

#' remove all
rm(list = ls())
graphics.off()

#' Load the required packages
library(tidyverse)
library(lme4)
library(brms)
library(glmmTMB)
library(jtools)
library(emmeans)

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
bb_monthly <-
  read_csv("summary//modified summaries//combined_month_reg_bb_raster.csv",
           col_names = T)
bb_yearly <-
  read_csv("summary//modified summaries//combined_reg_bb_raster.csv",
           col_names = T)

#' note that the brownian bridge data are based on regularised tracks with specifed rasters

head(monthly)
head(yearly)
head(breeding)
head(bb_monthly)
head(bb_yearly)

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

#' remove #834450834 and #834451702 from Kendall Tanz data and AM267 from Kerri
#' for yearly data
length(levels(as.factor(yearly$id)))

`%notin%` <- Negate(`%in%`)
yearly <- yearly[yearly$id %notin% c("#834450834", "#834451702", "AM267"),]

length(levels(as.factor(yearly$id)))

#' for monthly data
length(levels(as.factor(monthly$bird)))

monthly <-
  monthly[monthly$bird %notin% c("#834450834", "#834451702", "AM267"),]

length(levels(as.factor(monthly$bird)))

#' these should show as empty
filter(monthly, bird == "#834450834")
filter(monthly, bird == "#834451702")
filter(monthly, bird == "AM267")


#' extract the Gyps vultures
levels(as.factor(yearly$species))
yearly <-
  yearly %>% filter(species == "cv" |
                      species == "wb" | species == "rv")
monthly <-
  monthly %>% filter(species == "cv" |
                       species == "wb" | species == "rv")

####' add the age column to the monthly data using ids ----
#' this is called id in the yearly data and bird in the monthly data
monthly$age <- yearly$age[match(monthly$bird, yearly$id)]
monthly$age

####' add the breeding data to the monthly data summary ----
#' here for the monthly data
monthly$breeding_days <-
  breeding$time[match(monthly$id, breeding$id)]

monthly %>% dplyr::select(id, breeding_days, study)

#####' determine breeding according to age and breeding_days ----
#' what is the maximum value at one site for known breeders?
filter(breeding, recorded_breeder == "yes") %>%
  group_by(bird) %>%
  summarise(max_time = max(time))

#' alternative way
filter(breeding, recorded_breeder == "yes") %>% 
group_by(bird) %>%
slice(which.max(time))

#' check if the birds hang around an area for more than a month
filter(breeding, recorded_breeder == "yes", species == "wb" | species == "cv") %>% 
  arrange(desc(time)) %>% 
  group_by(bird) %>% 
  slice(1:3) %>% 
  print(n=nrow(.))

#' look at what the immature birds are doing
filter(breeding, age == "imm", species == "wb" | species == "cv") %>% 
  arrange(desc(time)) %>% 
  group_by(bird) %>% 
  slice(1:3) %>% 
  print(n=nrow(.))
  

#' define the breeders by age, season, species, region
#' first for monthly data
#' create a column with month for the monthly data
monthly$month <- basename(monthly$id)
monthly$month <- as.numeric(monthly$month)

dplyr::select(monthly, id, month)

#' create a variable that gives potential times for breeding season
#' do it for each species group

monthly_cv <- monthly %>% dplyr::filter(species == "cv")
monthly_rv <- monthly %>% dplyr::filter(species == "rv")
monthly_wbe <-
  monthly %>% dplyr::filter(species == "wb" & region == "east")
monthly_wbs <-
  monthly %>% dplyr::filter(species == "wb" & region == "south")


monthly_cv$breeding_season <- if_else(monthly_cv$age == "adult" &
                                        monthly_cv$month > 3,
                                      1, 0)

monthly_rv$breeding_season <- if_else(monthly_rv$age == "adult",
                                      1, 0)

monthly_wbe$breeding_season <- if_else(monthly_wbe$age == "adult" &
                                         monthly_wbe$month < 10 |
                                         monthly_wbe$month > 11,
                                       1,
                                       0)

monthly_wbs$breeding_season <- if_else(monthly_wbs$age == "adult" &
                                         monthly_wbs$month > 3 &
                                         monthly_wbs$month < 11,
                                       1,
                                       0)

monthly <- rbind(monthly_cv, monthly_rv, monthly_wbe, monthly_wbs)
monthly

#' verify potential breeding months for cape
filter(monthly, breeding_season == "1") %>%
  filter(species == "cv" & age == "adult") %>%
  dplyr::select(breeding_season, species, month, region, age) %>%
  print(n = nrow(.))

#' for ruppells
filter(monthly, breeding_season == "1") %>%
  dplyr::select(breeding_season, species, month, region) %>%
  filter(species == "rv") %>% print(n = nrow(.))

#' for eastern wbs
filter(monthly, breeding_season == "1") %>%
  dplyr::select(breeding_season, species, month, region) %>%
  filter(species == "wb" & region == "east") %>% print(n = nrow(.))

#' for southern wbs
filter(monthly, breeding_season == "1") %>%
  dplyr::select(breeding_season, species, month, region) %>%
  filter(species == "wb" & region == "south") %>% print(n = nrow(.))


#' check if any of the birds were hanging around in an area for a long period
#' during these potential breeding months
monthly$breeder <- if_else(monthly$breeding_days > 7 &
                             monthly$breeding_season == "1", 1, 0)
monthly$breeder
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

#' add the brownian bridge values to the monthly data
monthly$bb <-
  bb_monthly$bb_area_95[match(monthly$id, bb_monthly$ID)]
#' add the overlap of brownian bridge areas with PAs to the monthly data
monthly$bb_overlap <-
  bb_monthly$bb_area_95_overlap[match(monthly$id, bb_monthly$ID)]

#' add the brownian bridge values to the yearly data
yearly$bb <-
  bb_yearly$bb_area_95[match(yearly$id, bb_yearly$ID)]
#' add the overlap of brownian bridge areas with PAs to the monthly data
yearly$bb_overlap <-
  bb_yearly$bb_area_95_overlap[match(yearly$id, bb_yearly$ID)]

#' how do the various home range metrics compare?
#' on a yearly basis
cor.test(yearly$kde_95_reg, yearly$mcps_95_reg)
cor.test(yearly$kde_95_reg, yearly$bb)
cor.test(yearly$mcps_95_reg, yearly$bb)

#' compare them with boxplots
boxplot(yearly[, c("mcps_95_reg", "bb")])
boxplot(yearly[, c("kde_95_reg", "bb")])

#' and on a monthly basis?
cor.test(monthly$kde_95_reg, monthly$mcps_95_reg)
cor.test(monthly$kde_95_reg, monthly$bb)
cor.test(monthly$mcps_95_reg, monthly$bb)

#' how many values per species for all data?
yearly %>%
  group_by(species) %>%
  summarise(count = n())

#' how many values per species for the monthly data?
monthly %>%
  group_by(species) %>%
  summarise(count = n())

####' make some plots ----
#' http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization

#' these are based on KDE
ggplot(data = yearly, mapping = aes(x = kde_95_raw, fill = age)) + geom_histogram() +
  facet_wrap( ~ species)

ggplot(data = yearly,
       mapping = aes(x = kde_95_raw, fill = region)) + geom_histogram() +
  facet_wrap( ~ species)

ggplot(data = yearly,
       mapping = aes(x = kde_95_raw, fill = as.factor(breeder))) + geom_histogram() +
  facet_wrap( ~ species)

#' same plots but based on brownian bridges
filter(yearly, age == "adult" | age == "imm") %>%
  ggplot(data = ., mapping = aes(x = bb, fill = age)) + geom_histogram() +
  facet_wrap( ~ species)

filter(yearly, age == "adult" | age == "imm") %>%
  ggplot(data = ., mapping = aes(x = bb, fill = region)) + geom_histogram() +
  facet_wrap( ~ species)

filter(yearly, age == "adult" | age == "imm") %>%
  ggplot(data = ., mapping = aes(x = bb, fill = as.factor(breeder))) + geom_histogram() +
  facet_wrap( ~ species)

#' take a look at overlap with PAs
filter(yearly, age == "adult" | age == "imm" & species == "wb") %>%
  ggplot(data = .,
         mapping = aes(x = as.factor(region) , y = bb_overlap)) + geom_boxplot()

#' include wet and dry season?
#' Southern Africa Dry Season = April to October - 4 - 10
#' East Africa Dry Season = June to September - 6 - 9

monthly$season <-
  if_else (
    monthly$month > 3 &
      monthly$month < 11 & monthly$region == "south",
    "dry",
    if_else (
      monthly$month < 4 &
        monthly$month > 10 & monthly$region == "south",
      "wet",
      if_else(
        monthly$month > 5 &
          monthly$month < 10 &
          monthly$region == "east",
        "dry",
        "wet"
      )
    )
  )

#' make sure it worked
#' should be 4 to 10
filter(monthly, region == "south", season == "dry") %>% dplyr::select(month) %>% print(n = nrow(.))
#' should be 6 to 9
filter(monthly, region == "east", season == "dry") %>% dplyr::select(month) %>% print(n = nrow(.))

####' models on yearly data ----
#' base the data on only birds with a known age
yearly_model <-
  filter(yearly, age == "adult" | age == "imm" & duration > 180)

#' create a variable that combine breeding status and age
#' 3 levels, immature, breeding adult & non-breeding adult
#' 0 is immature
#' 1 is breeding adult
#' 2 is non-breeding adult
yearly_model$age_breed <-
  if_else(yearly_model$age == "adult" &
            yearly_model$breeder == 0,
          2,
          yearly_model$breeder)
yearly_model$age_breed

#' how many values per species for this reduced dataset?
yearly_model %>%
  group_by(species) %>%
  summarise(count = n())

#' can plot this
ggplot(data = yearly_model, mapping = aes(x = as.factor(age_breed), y = bb)) + geom_boxplot() +
  stat_summary(
    fun = mean,
    colour = "darkred",
    geom = "point",
    shape = 18,
    size = 3,
    show.legend = FALSE
  )

#' run some summary stats on the bb size by species and region
yearly_model %>%
  group_by(species, region) %>%
  summarise(
    mean = mean(bb, na.rm = T),
    median = median(bb, na.rm = T),
    sd = sd(bb, na.rm = T),
    min = min(bb, na.rm = T),
    max = max(bb, na.rm = T)
  )

#' can create a new species variable that splits eastern and southern WBs

yearly_model <- yearly_model %>%
  mutate(population = case_when(
    region == "east" & species == "wb" ~ paste0(species, "e"),
    TRUE ~ as.character(species)
  ))

dplyr::select(yearly_model, species, population, bb, region)

#' run a model with brownian bridge as the dependent
#' The log-linked gamma GLM specification is identical to exponential regression
#' https://stats.stackexchange.com/questions/96972/how-to-interpret-parameters-in-glm-with-family-gamma
#' https://rpubs.com/kaz_yos/glm-Gamma
#' https://stats.stackexchange.com/questions/161216/backtransform-coefficients-of-a-gamma-log-glmm
#' https://stats.stackexchange.com/questions/431120/how-to-interpret-parameters-of-glm-output-with-gamma-log-link

m1 <-
  glm(
    # bb ~ as.factor(age_breed) + species + region,
    bb ~ as.factor(age_breed) + population, 
    data = yearly_model,
    family = Gamma(link = log)
  )
summary(m1)
plot(m1)

exp(coef(m1))

#' compare a model of bb ~ as.factor(age_breed) to this and the values
#' should be very similar
yearly_model %>%
  group_by(age_breed) %>%
  summarise(meanbb = mean(bb, na.rm = T))

#' try a Bayesian framework
bm1 <-
  brms::brm(
    # bb ~ as.factor(age_breed) + species + region,
    bb ~ as.factor(age_breed) + population,
    data = yearly_model,
    family = Gamma(link = log),
    iter = 5000,
    chains = 4,
    cores = 4
  )
summary(bm1)
plot(bm1)

#' run a model with the proportion overlap as the dependent variable on the yearly data
yearly_model$overlap <-
  round(yearly_model$bb_overlap / yearly_model$bb, 3)
summary(yearly_model$overlap)
yearly_model$overlap
length(yearly_model$overlap)

#' need to rescale values to avoid 1s and 0s for beta distribution
transform01 <- function(x) {
  (x * (length(x) - 1) + 0.5) / (length(x))
}

yearly_model$overlap_mod <- transform01(yearly_model$overlap)
summary(yearly_model$overlap_mod)

#' overlap model using glmmTMB
m2 <-
  glmmTMB(
    # overlap_mod ~ as.factor(age_breed) + species + region,
    overlap_mod ~ as.factor(age_breed) + population,
    family = list(family = "beta", link = "logit"),
    data = yearly_model
  )
summary(m2)

bm2 <-
  brms::brm(
    # overlap_mod ~ as.factor(age_breed) + species + region,
    overlap_mod ~ as.factor(age_breed) + population,
    data = yearly_model,
    family = "beta",
    iter = 5000,
    chains = 4,
    cores = 4
  )
summary(bm2)
plot(bm2)

####' models on monthly data ----
monthly_model <- filter(monthly, age == "adult" | age == "imm")

#' again create a new species variable that splits eastern and southern WBs

monthly_model <- monthly_model %>%
  mutate(population = case_when(
    region == "east" & species == "wb" ~ paste0(species, "e"),
    TRUE ~ as.character(species)
  ))

dplyr::select(monthly_model, species, population, bb, region) %>% 
  dplyr::filter(species == "wb") %>% print(n = nrow(.))

#' run some summary stats on the bb size by species and region
monthly_model %>%
  group_by(species, region) %>%
  summarise(
    mean = mean(bb, na.rm = T),
    median = median(bb, na.rm = T),
    sd = sd(bb, na.rm = T),
    min = min(bb, na.rm = T),
    max = max(bb, na.rm = T)
  )

#' again create a variable that combine breeding status and age
#' 3 levels, immature, breeding adult & non-breeding adult
#' 0 is immature
#' 1 is breeding adult
#' 2 is non-breeding adult

monthly_model$age_breed <-
  if_else(monthly_model$age == "adult" &
            monthly_model$breeder == 0,
          2,
          monthly_model$breeder)
monthly_model$age_breed

monthly_model$season <- as.factor(monthly_model$season)

#' included season here 
m3 <-
  glmer(
    # bb ~ as.factor(age_breed) + species + as.factor(season) + region +
    bb ~ as.factor(age_breed) + population + season +
      (1 | bird),
    data = monthly_model,
    family = Gamma(link = log)
  )
summary(m3)

#' again try a Bayesian framework on the same model
bm3 = brms::brm(
  # bb ~ as.factor(age_breed) + species + as.factor(season) + region +
  bb ~ as.factor(age_breed) + population + season +
    (1 | bird),
  data = monthly_model,
  family = Gamma(link = log),
  iter = 5000,
  chains = 4,
  cores = 4
)

summary(bm3)
plot(bm3)

#' run a model with the proportion overlap as the dependent variable on the monthly data
monthly_model$overlap <-
  round(monthly_model$bb_overlap / monthly_model$bb, 3)
summary(monthly_model$overlap)
monthly_model$overlap
length(monthly_model$overlap)

#' need to rescale values to avoid 1s and 0s for beta distribution
transform01 <- function(x) {
  (x * (length(x) - 1) + 0.5) / (length(x))
}

monthly_model$overlap_mod <- transform01(monthly_model$overlap)
summary(monthly_model$overlap_mod)

m4 <-
  glmmTMB(
    # overlap_mod ~ as.factor(age_breed) + species + as.factor(season) + region + 
    overlap_mod ~ as.factor(age_breed) + population + season +
      (1 | bird),
    data = monthly_model,
    family = list(family = "beta", link = "logit")
  )
summary(m4)

#' again try a Bayesian framework on the same model
bm4 = brms::brm(
  # overlap_mod ~ as.factor(age_breed) + species + as.factor(season) + region +
  overlap_mod ~ as.factor(age_breed) + population + season +
    (1 | bird),
  data = monthly_model,
  family = "beta",
  iter = 5000,
  chains = 4,
  cores = 4
)

summary(bm4)
plot(bm4)

#' compare them all here for ease
summary(m1) # yearly bb 
summary(m2) # yearly overlap bb
summary(m3) # monthly bb
summary(m4) # monthly overlap bb

summ(m1) # yearly bb 
summ(m2) # yearly overlap bb
summ(m3) # monthly bb
summ(m4) # monthly overlap bb

####' post hoc analysis using emmeans ----
#' https://garthtarr.github.io/meatR/emmeans.html
marginal1 = emmeans(m1, ~ population, by = "age_breed")
marginal2 = emmeans(m2, ~ population, by = "age_breed")
marginal3 = emmeans(m3, ~ population, by = "age_breed")
marginal4 = emmeans(m4, ~ population, by = "age_breed")

pairs(marginal1) 
pairs(marginal2)
pairs(marginal3) 
pairs(marginal4) 

plot(marginal1) + ggtitle("model 1") 
plot(marginal2) + ggtitle("model 2") 
plot(marginal3) + ggtitle("model 3") 
plot(marginal4) + ggtitle("model 4") 


#' check out the Ruppell's
filter(yearly, species == "rv") %>% 
  dplyr::select(study, species, id, age) %>% 
  print(n = nrow(.))

#####' export these data ----
#' simplify the data for export

monthly_export <-
  monthly %>% dplyr::select(
    id,
    bird,
    duration,
    species,
    age,
    study,
    region,
    kde_95_reg,
    mcps_95_reg,
    bb,
    bb_overlap,
    breeding_days,
    breeder
  )

yearly_export <-
  yearly %>% dplyr::select(
    id,
    duration,
    species,
    age,
    study,
    region,
    kde_95_reg,
    mcps_95_reg,
    bb,
    bb_overlap,
    `confirmed_breeding?`,
    breeder
  )

write.csv(monthly_export, file = "summary//modified summaries//monthly_w_breeding.csv", row.names = FALSE)
write.csv(yearly_export, file = "summary//modified summaries//yearly_w_breeding.csv", row.names = FALSE)
