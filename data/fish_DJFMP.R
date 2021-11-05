library(LTMRdata)
library(tidyverse)
library(lubridate)
library(ggmap)

DJFMP_Silverside <-LTMRdata::fish(sources="DJFMP", species="Menidia audens", size_cutoff=NULL,remove_unknown_lengths=FALSE)

DJFMP_Silverside_seine<-DJFMP_Silverside %>% filter(Method=="Beach seine")

str(DJFMP_Silverside_seine)


DJFMP_seine_sampling_effort <- DJFMP_Silverside_seine %>% mutate(Year=year(Date),Month=month(Date)) %>% group_by(Datetime,Station,Year,Month) %>%
  summarise(Silverside_count=sum(Count)) %>% mutate(sample_size=1) %>% group_by(Station,Year,Month) %>% summarise(sample_size=sum(sample_size))

#Map
DJFMP_Coords<-DJFMP_Silverside_seine %>% group_by(Station) %>% summarise(Latitude=mean(Latitude),Longitude=mean(Longitude))
DJFMP_Coords<-DJFMP_Coords[complete.cases(DJFMP_Coords), ]


###
buffer <- 0.05
coordDict = list(
  'minLat' = min(DJFMP_Coords$Latitude) - buffer,
  'maxLat' = max(DJFMP_Coords$Latitude) + buffer,
  'minLon' = min(DJFMP_Coords$Longitude) - buffer,
  'maxLon' = max(DJFMP_Coords$Longitude) + buffer
)

map_obj <- get_stamenmap(
  bbox = c(left = coordDict[['minLon']], bottom = coordDict[['minLat']], right = coordDict[['maxLon']], top = coordDict[['maxLat']]),
  zoom = 10,
  maptype = 'terrain'
)

# plot the map
map <- ggmap(map_obj) +
  theme_void() +
  geom_point(data = DJFMP_Coords, aes(x = Longitude, y = Latitude), shape = 21, fill = 'red', size = 3) +
  geom_text(data = DJFMP_Coords, aes(label = Station, x = Longitude, y = Latitude), vjust = 0, hjust = 0, size=2.5)

map

long_term_stations<- c("DS002S", "GS010E", "LP003E","MK004W", "MR010W", "MS001N","MS001A","OR003W","OR014W","SF014E","SJ001S", "SJ005N", "SJ032S","SJ041N","SJ051E", "SJ056E", "SJ058W", "SJ058E","SR012E","SR012W","SR014W",  "SR017E", "SR024E", "SR043W", "SR049E", "SS011N","TM001N", "WD002W","WD002E","XC001N" )

DJFMP_Coords_study_stations<-DJFMP_Coords %>% filter(Station %in% long_term_stations)

buffer <- 0.05
coordDict = list(
  'minLat' = min(DJFMP_Coords_study_stations$Latitude) - buffer,
  'maxLat' = max(DJFMP_Coords_study_stations$Latitude) + buffer,
  'minLon' = min(DJFMP_Coords_study_stations$Longitude) - buffer,
  'maxLon' = max(DJFMP_Coords_study_stations$Longitude) + buffer
)

map_obj <- get_stamenmap(
  bbox = c(left = coordDict[['minLon']], bottom = coordDict[['minLat']], right = coordDict[['maxLon']], top = coordDict[['maxLat']]),
  zoom = 10,
  maptype = 'terrain'
)

map <- ggmap(map_obj) +
  theme_void() +
  geom_point(data = DJFMP_Coords_study_stations, aes(x = Longitude, y = Latitude), shape = 21, fill = 'red', size = 3) +
  geom_text(data = DJFMP_Coords_study_stations, aes(label = Station, x = Longitude, y = Latitude), vjust = 0, hjust = 0, size=2.5)

map


DJFMP_seine_sampling_effort<-DJFMP_seine_sampling_effort %>% filter(Station %in% long_term_stations)
DJFMP_seine_sampling_effort_annual<- DJFMP_seine_sampling_effort %>% group_by(Year,Station) %>% summarise(sample_size=sum(sample_size))

#Plot effort across stations and years for DJFMP beach seine
ggplot(DJFMP_seine_sampling_effort_annual,aes(x=Year,y=as.factor(Station),fill=sample_size))+geom_tile()+
  scale_x_continuous(breaks=c(1970,1980,1990,2000,2010,2015,2020)) + labs(title= "DJFMP station sampling effort by year")


DJFMP_Coords_study_stations_subset<-DJFMP_Coords_study_stations %>% filter(Station %in% c("XC001N","TM001N","SR049E","SR043W","SR024E","SR017E","SR014W","SR012E","SJ041N","SJ005N","SJ001S","SF014E","MS001N","MR010W","MK004W","LP003E","GS010E","DS002S"))


map <- ggmap(map_obj) +
  theme_void() +
  geom_point(data = DJFMP_Coords_study_stations_subset, aes(x = Longitude, y = Latitude), shape = 21, fill = 'red', size = 3) +
  geom_text(data = DJFMP_Coords_study_stations_subset, aes(label = Station, x = Longitude, y = Latitude), vjust = 0, hjust = 0, size=2.5)

map


DJFMP_seine_sampling_effort_monthly<- DJFMP_seine_sampling_effort %>% filter(Station %in% DJFMP_Coords_study_stations_subset$Station)

#Plot effort across stations and years for midwater trawl
ggplot(DJFMP_seine_sampling_effort_monthly,aes(x=Year,y=Month,fill=sample_size))+geom_tile()+facet_wrap(~Station)+
  scale_x_continuous(breaks=c(1970,1980,1990,2000,2010,2015,2020))+ scale_y_continuous(breaks=c(1:12)) + labs(title= "DJFMP station sampling effort by month and year")

###Add SR012E!

DJFMP_Silverside_seine_subset<- DJFMP_Silverside_seine %>% mutate(Month=month(Date),Year=year(Date)) %>%
  filter(Station %in% DJFMP_Coords_study_stations_subset$Station & Month %in% c(1:5)) %>%
  group_by(Year, Month, Station, Datetime) %>% summarise(Catch_per_seine=sum(Count)) %>%
  group_by(Year, Month, Station) %>% summarise(Catch_per_seine=mean(Catch_per_seine)) %>%
  group_by(Year, Month) %>% summarise(Catch_per_seine=mean(Catch_per_seine)) %>%
  group_by(Year) %>% summarise(Catch_per_seine=mean(Catch_per_seine))

