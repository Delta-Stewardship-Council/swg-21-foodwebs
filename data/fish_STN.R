devtools::install_github("sbashevkin/LTMRdata")
library(LTMRdata)
library(tidyverse)
library(lubridate)
library(ggmap)

setwd("~/GitHub/swg-21-foodwebs")

#From Sam's LTMRdata package


#Load species
STN_Delta_Smelt <-LTMRdata::fish(sources="STN", species="Hypomesus transpacificus", size_cutoff=NULL,remove_unknown_lengths=FALSE)
STN_Longfin_Smelt <-LTMRdata::fish(sources="STN", species="Spirinchus thaleichthys", size_cutoff=NULL,remove_unknown_lengths=FALSE)
STN_Threadfin_Shad <-LTMRdata::fish(sources="STN", species="Dorosoma petenense", size_cutoff=NULL,remove_unknown_lengths=FALSE)
STN_American_Shad <-LTMRdata::fish(sources="STN", species="Alosa sapidissima", size_cutoff=NULL,remove_unknown_lengths=FALSE)
STN_Northern_Anchovy <-LTMRdata::fish(sources="STN", species="Engraulis mordax", size_cutoff=NULL,remove_unknown_lengths=FALSE)
STN_Pacific_Herring <-LTMRdata::fish(sources="STN", species="Clupea pallasii", size_cutoff=NULL,remove_unknown_lengths=FALSE)

#Load age-0 Striped Bass
#Remove age-1 Striped Bass and NOTE that we assume that unmeasured Striped Bass are age-0
STN_Striped_Bass <-LTMRdata::fish(sources="STN", species="Morone saxatilis", size_cutoff=NULL,remove_unknown_lengths=FALSE) %>% filter(is.na(Length)|Length<150)

str(STN_Delta_Smelt)
#Limit the data to just April to October (this was for baystudy, we aren't limiting the date extent)
STN_combined <- bind_rows(STN_Delta_Smelt,STN_Longfin_Smelt,STN_Threadfin_Shad,
                               STN_American_Shad,STN_Pacific_Herring,
                               STN_Northern_Anchovy,STN_Striped_Bass) %>%
  mutate(Month=month(Date)) #%>% filter(Month %in% c(4:10)) %>%
  #and only between San Pablo Bay and the confluence
  #https://nrm.dfg.ca.gov/FileHandler.ashx?DocumentID=185001&inline
 # mutate(Station=as.numeric(Station)) %>%
  #filter(Station>300) %>%
  #filter(Station<700|Station %in% c(736,837))

unique(STN_combined$Station)

#extract coords from smelt data frame
STNCoords<-STN_Delta_Smelt %>% group_by(Station) %>% summarise(Latitude=mean(Latitude),Longitude=mean(Longitude))
STNCoords<-STNCoords[complete.cases(STNCoords), ]

#not sure what this is
#STNCoords_studyregion<-STNCoords %>% filter(Station>300) %>%
 # filter(Station<700|Station %in% c(736,837))

###
buffer <- 0.05
coordDict = list(
  'minLat' = min(STNCoords$Latitude) - buffer,
  'maxLat' = max(STNCoords$Latitude) + buffer,
  'minLon' = min(STNCoords$Longitude) - buffer,
  'maxLon' = max(STNCoords$Longitude) + buffer
)

map_obj <- get_stamenmap(
  bbox = c(left = coordDict[['minLon']], bottom = coordDict[['minLat']], right = coordDict[['maxLon']], top = coordDict[['maxLat']]),
  zoom = 10,
  maptype = 'terrain'
)

# plot the map
map <- ggmap(map_obj) +
  theme_void() +
  geom_point(data = STNCoords, aes(x = Longitude, y = Latitude), shape = 21, fill = 'red', size = 3) +
  geom_point(data = STNCoords, aes(x = Longitude, y = Latitude), shape = 21, fill = 'yellow', size = 3)+
  geom_text(data = STNCoords, aes(label = Station, x = Longitude, y = Latitude), vjust = 0, hjust = 0, size=2.5)

map

##Sample size
STN_sample_size <- STN_combined %>% group_by(SampleID,Date,Month,Station,Method) %>%
  summarise(Latitude=mean(Latitude),Longitude=mean(Longitude)) %>% mutate(sample_size=1,Year=year(Date)) %>%
  group_by(Year,Station,Method,Latitude,Longitude) %>% summarise(sample_size=sum(sample_size))

#Plot effort across stations and years for midwater trawl
ggplot(STN_sample_size,aes(x=Year,y=as.factor(Station),fill=sample_size))+geom_tile()+
  scale_x_continuous(breaks=c(1970,1980,1990,2000,2010,2015,2020)) + labs(title= "STN")


#We probably need to remove site 326 and 325 since they're only sampled in one year
`%notin%` <- Negate(`%in%`)
BayStudyCoords_studyregion<-BayStudyCoords_studyregion %>% filter(Station %notin% c(324,326))

#And add regions from Bay Study for summarizing annual values
BayStudyCoords_studyregion<-BayStudyCoords_studyregion %>%
  mutate(Region = case_when(
    Station>=300&Station<400 ~ "San Pablo Bay",
    Station>=400&Station<=534 ~ "Suisun Bay",
    Station>534 ~ "Confluence"
  ))

#Map showing final list of stations
map2 <- ggmap(map_obj) +
  theme_void() +
  geom_point(data = BayStudyCoords_studyregion, aes(x = Longitude, y = Latitude, fill=as.factor(Region)), shape = 21, size = 3) +
  geom_text(data = BayStudyCoords_studyregion, aes(label = Station, x = Longitude, y = Latitude), vjust = 0, hjust = 0, size=2.5)+
  labs(fill=NULL)

map2

#Subset to just final stations
BayStudy_combined_derived<-BayStudy_combined %>% filter(Station %in% BayStudyCoords_studyregion$Station)
BayStudyCoords_studyregion$Station<-as.numeric(BayStudyCoords_studyregion$Station)
BayStudy_combined_derived<-left_join(BayStudy_combined_derived,BayStudyCoords_studyregion)

#Calculate final annual values by:
#1) Calculate biomass based on Kimmerer et al. 2005
#2) Averaging CPUE and biomass by region for each year
#3) Average across region for each year to get an annual value

BayStudy_combined_derived <- BayStudy_combined_derived %>%
  mutate(Length=ifelse(is.na(Length),0,Length)) %>%
  mutate(Year=year(Date)) %>%
  mutate(Biomass = case_when(
    Taxa=="Hypomesus transpacificus" ~ (0.0018*(Length^3.38))*Count,
    Taxa=="Spirinchus thaleichthys" ~ (0.0005*(Length^3.69))*Count,
    Taxa=="Dorosoma petenense" ~ (0.0072*(Length^3.16))*Count,
    Taxa=="Alosa sapidissima" ~ (0.0074*(Length^3.09))*Count,
    Taxa=="Engraulis mordax" ~ (0.0015*(Length^3.37))*Count,
    Taxa=="Clupea pallasii" ~ (0.0015*(Length^3.44))*Count,
    Taxa=="Morone saxatilis" ~ (0.0066*(Length^3.12))*Count))



BayStudy_annual_values <- BayStudy_combined_derived %>%
  group_by(Year,Region,Method,Taxa) %>%
  summarise(Biomass=mean(Biomass),Catch_per_tow=mean(Count)) %>%
  group_by(Year,Method,Taxa) %>%
  summarise(Biomass=mean(Biomass),Catch_per_tow=mean(Catch_per_tow)) %>%
  mutate(CommonName= case_when(
    Taxa=="Hypomesus transpacificus" ~ "DeltaSmelt",
    Taxa=="Spirinchus thaleichthys" ~ "LongfinSmelt",
    Taxa=="Dorosoma petenense" ~ "ThreadfinShad",
    Taxa=="Alosa sapidissima" ~ "AmericanShad",
    Taxa=="Engraulis mordax" ~ "NorthernAnchovy",
    Taxa=="Clupea pallasii" ~ "PacificHerring",
    Taxa=="Morone saxatilis" ~ "StripedBass_age0"))

#If you check the original dataset, there are no unmeasured fish flag
#Summarize biomass as you would normally


BayStudy_Midwater_annual_values_CPUE <- BayStudy_annual_values %>% filter(Method=="Midwater trawl") %>% subset(select=c(Year,CommonName,Catch_per_tow)) %>%
  pivot_wider(names_from =CommonName,values_from = Catch_per_tow,names_prefix="BayStudy_MidwaterTrawl_fish_catch_per_tow_") %>%
  mutate(BayStudy_MidwaterTrawl_fish_catch_per_tow_Estuarine_pelagic_forage_fishes=sum(BayStudy_MidwaterTrawl_fish_catch_per_tow_AmericanShad,
                                                                                       BayStudy_MidwaterTrawl_fish_catch_per_tow_ThreadfinShad,
                                                                                       BayStudy_MidwaterTrawl_fish_catch_per_tow_DeltaSmelt,
                                                                                       BayStudy_MidwaterTrawl_fish_catch_per_tow_LongfinSmelt,
                                                                                       BayStudy_MidwaterTrawl_fish_catch_per_tow_StripedBass_age0),
         BayStudy_MidwaterTrawl_fish_catch_per_tow_Marine_pelagic_forage_fishes=sum(BayStudy_MidwaterTrawl_fish_catch_per_tow_NorthernAnchovy,
                                                                                    BayStudy_MidwaterTrawl_fish_catch_per_tow_PacificHerring))


BayStudy_Otter_annual_values_CPUE <- BayStudy_annual_values %>% filter(Method=="Otter trawl") %>% subset(select=c(Year,CommonName,Catch_per_tow)) %>%
  pivot_wider(names_from =CommonName,values_from = Catch_per_tow,names_prefix="BayStudy_OtterTrawl_fish_catch_per_tow_") %>%
  mutate(BayStudy_OtterTrawl_fish_catch_per_tow_Estuarine_pelagic_forage_fishes=sum(BayStudy_OtterTrawl_fish_catch_per_tow_AmericanShad,
                                                                                    BayStudy_OtterTrawl_fish_catch_per_tow_ThreadfinShad,
                                                                                    BayStudy_OtterTrawl_fish_catch_per_tow_DeltaSmelt,
                                                                                    BayStudy_OtterTrawl_fish_catch_per_tow_LongfinSmelt,
                                                                                    BayStudy_OtterTrawl_fish_catch_per_tow_StripedBass_age0),
         BayStudy_OtterTrawl_fish_catch_per_tow_Marine_pelagic_forage_fishes=sum(BayStudy_OtterTrawl_fish_catch_per_tow_NorthernAnchovy,
                                                                                 BayStudy_OtterTrawl_fish_catch_per_tow_PacificHerring))



BayStudy_Midwater_annual_values_biomass <- BayStudy_annual_values %>% filter(Method=="Midwater trawl") %>% subset(select=c(Year,CommonName,Biomass)) %>%
  pivot_wider(names_from =CommonName,values_from = Biomass,names_prefix="BayStudy_MidwaterTrawl_fish_biomass_") %>%
  mutate(BayStudy_MidwaterTrawl_fish_biomass_Estuarine_pelagic_forage_fishes=sum(BayStudy_MidwaterTrawl_fish_biomass_AmericanShad,
                                                                                 BayStudy_MidwaterTrawl_fish_biomass_ThreadfinShad,
                                                                                 BayStudy_MidwaterTrawl_fish_biomass_DeltaSmelt,
                                                                                 BayStudy_MidwaterTrawl_fish_biomass_LongfinSmelt,
                                                                                 BayStudy_MidwaterTrawl_fish_biomass_StripedBass_age0),
         BayStudy_MidwaterTrawl_fish_biomass_Marine_pelagic_forage_fishes=sum(BayStudy_MidwaterTrawl_fish_biomass_NorthernAnchovy,
                                                                              BayStudy_MidwaterTrawl_fish_biomass_PacificHerring))


BayStudy_Otter_annual_values_biomass <- BayStudy_annual_values %>% filter(Method=="Otter trawl") %>% subset(select=c(Year,CommonName,Biomass)) %>%
  pivot_wider(names_from =CommonName,values_from = Biomass,names_prefix="BayStudy_OtterTrawl_fish_biomass_") %>%
  mutate(BayStudy_OtterTrawl_fish_biomass_Estuarine_pelagic_forage_fishes=sum(BayStudy_OtterTrawl_fish_biomass_AmericanShad,
                                                                              BayStudy_OtterTrawl_fish_biomass_ThreadfinShad,
                                                                              BayStudy_OtterTrawl_fish_biomass_DeltaSmelt,
                                                                              BayStudy_OtterTrawl_fish_biomass_LongfinSmelt,
                                                                              BayStudy_OtterTrawl_fish_biomass_StripedBass_age0),
         BayStudy_OtterTrawl_fish_biomass_Marine_pelagic_forage_fishes=sum(BayStudy_OtterTrawl_fish_biomass_NorthernAnchovy,
                                                                           BayStudy_OtterTrawl_fish_biomass_PacificHerring))

BayStudy_Otter_annual_values_biomass
BayStudy_Midwater_annual_values_CPUE
BayStudy_Otter_annual_values_CPUE

BayStudy_annual_values_final_Midwater<-full_join(BayStudy_Midwater_annual_values_CPUE,
                                                 BayStudy_Midwater_annual_values_biomass)
BayStudy_annual_values_final_Otter<-full_join(BayStudy_Otter_annual_values_CPUE,
                                              BayStudy_Otter_annual_values_biomass)
BayStudy_annual_values_final<-full_join(BayStudy_annual_values_final_Midwater,BayStudy_annual_values_final_Otter)

write.csv(BayStudy_annual_values_final,row.names=FALSE,file=file.path("data/annual_averages/fish_data_BayStudy.csv"))
