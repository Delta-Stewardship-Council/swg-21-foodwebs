#devtools::install_github("sbashevkin/LTMRdata")
library(LTMRdata)
library(tidyverse)
library(lubridate)
library(brms)

setwd("~/GitHub/swg-21-foodwebs")

#Load California Department of Fish and Wildlife
#Fall Midwater Trawl Data
#From Sam's LTMRdat package


#Load species
FMWT_Delta_Smelt <-LTMRdata::fish(sources="FMWT", species="Hypomesus transpacificus", size_cutoff=NULL,remove_unknown_lengths=FALSE)
FMWT_Longfin_Smelt <-LTMRdata::fish(sources="FMWT", species="Spirinchus thaleichthys", size_cutoff=NULL,remove_unknown_lengths=FALSE)
FMWT_Threadfin_Shad <-LTMRdata::fish(sources="FMWT", species="Dorosoma petenense", size_cutoff=NULL,remove_unknown_lengths=FALSE)
FMWT_American_Shad <-LTMRdata::fish(sources="FMWT", species="Alosa sapidissima", size_cutoff=NULL,remove_unknown_lengths=FALSE)
FMWT_Northern_Anchovy <-LTMRdata::fish(sources="FMWT", species="Engraulis mordax", size_cutoff=NULL,remove_unknown_lengths=FALSE)
FMWT_Pacific_Herring <-LTMRdata::fish(sources="FMWT", species="Clupea pallasii", size_cutoff=NULL,remove_unknown_lengths=FALSE)

#Load age-0 Striped Bass
#Remove age-1 Striped Bass
FMWT_Striped_Bass <-LTMRdata::fish(sources="FMWT", species="Morone saxatilis", size_cutoff=NULL,remove_unknown_lengths=FALSE) %>% filter(Length<150)

#Load index stations from Steve Slater (California Dept. of Fish and Wildlife)
FMWT_index_stations<-read.csv(file.path("data","FMWT index stations_SS_BM.csv"))
FMWT_index_stations_only<-FMWT_index_stations %>% filter(Index=="1")


#Limit FMWT data to just index stations
FMWT_combined <- bind_rows(FMWT_Delta_Smelt,FMWT_Longfin_Smelt,FMWT_Threadfin_Shad,FMWT_American_Shad,FMWT_Striped_Bass,FMWT_Northern_Anchovy,FMWT_Pacific_Herring) %>% filter(Station %in% unique(FMWT_index_stations_only$Station)) %>%
  #and only for September to December (survey 3,4,5,6)
  filter(Survey %in% c(3:6))

#* Convert fish length data into biomass using equation from Kimmerer et al. (2005)
#Kimmerer W, Avent SR, Bollens SM, Feyrer F, Grimaldo LF, Moyle PB, Nobriga M, Visintainer T. 2005. Variability in Length–Weight Relationships Used to Estimate Biomass of Estuarine Fish from Survey Data. Trans Am Fish Soc. 134(2):481–495. doi:10.1577/t04-042.1.

FMWT_combined <- FMWT_combined %>%
  mutate(Length=ifelse(is.na(Length),0,Length)) %>%
  mutate(Biomass = case_when(
    Taxa=="Hypomesus transpacificus" ~ (0.0018*(Length^3.38))*Count,
    Taxa=="Spirinchus thaleichthys" ~ (0.0005*(Length^3.69))*Count,
    Taxa=="Dorosoma petenense" ~ (0.0072*(Length^3.16))*Count,
    Taxa=="Alosa sapidissima" ~ (0.0074*(Length^3.09))*Count,
    Taxa=="Engraulis mordax" ~ (0.0015*(Length^3.37))*Count,
    Taxa=="Clupea pallasii" ~ (0.0015*(Length^3.44))*Count,
    Taxa=="Morone saxatilis" ~ (0.0066*(Length^3.12))*Count))


FMWT_combined_sum <- FMWT_combined %>% group_by(Station,Datetime,Survey,Taxa) %>%
  summarise(Biomass=sum(Biomass),Catch_per_tow=sum(Count)) %>%
  mutate(Year=year(Datetime),Region=as.numeric(substr(Station, 1, 1)))

FMWT_annual_values <- FMWT_combined_sum %>%
  group_by(Year,Region,Survey,Taxa) %>%
  summarise(Biomass=mean(Biomass),Catch_per_tow=mean(Catch_per_tow)) %>%
  group_by(Year,Survey,Taxa) %>%
  summarise(Biomass=mean(Biomass),Catch_per_tow=mean(Catch_per_tow)) %>%
  group_by(Year,Taxa) %>%
  summarise(Biomass=mean(Biomass),Catch_per_tow=mean(Catch_per_tow)) %>%
  mutate(CommonName= case_when(
    Taxa=="Hypomesus transpacificus" ~ "DeltaSmelt",
    Taxa=="Spirinchus thaleichthys" ~ "LongfinSmelt",
    Taxa=="Dorosoma petenense" ~ "ThreadfinShad",
    Taxa=="Alosa sapidissima" ~ "AmericanShad",
    Taxa=="Engraulis mordax" ~ "NorthernAnchovy",
    Taxa=="Clupea pallasii" ~ "PacificHerring",
    Taxa=="Morone saxatilis" ~ "StripedBass_age0"))


FMWT_annual_values_biomass <- FMWT_annual_values %>% select(Year,CommonName,Biomass) %>%
  pivot_wider(names_from =CommonName,values_from =Biomass,names_prefix="fish_biomass_") %>%
  mutate(fish_biomass_Estuarine_pelagic_forage_fishes=sum(fish_biomass_AmericanShad,
                                                          fish_biomass_ThreadfinShad,
                                                          fish_biomass_DeltaSmelt,
                                                          fish_biomass_LongfinSmelt,
                                                          fish_biomass_StripedBass_age0),
         fish_biomass_Marine_pelagic_forage_fishes=sum(fish_biomass_NorthernAnchovy,
                                                       fish_biomass_PacificHerring))


FMWT_annual_values_CPUE <- FMWT_annual_values %>% select(Year,CommonName,Catch_per_tow) %>%
  pivot_wider(names_from =CommonName,values_from = Catch_per_tow,names_prefix="fish_catch_per_tow_") %>%
  mutate(fish_catch_per_tow_Estuarine_pelagic_forage_fishes=sum(fish_catch_per_tow_AmericanShad,
                                                                fish_catch_per_tow_ThreadfinShad,
                                                                fish_catch_per_tow_DeltaSmelt,
                                                                fish_catch_per_tow_LongfinSmelt,
                                                                fish_catch_per_tow_StripedBass_age0),
         fish_catch_per_tow_Marine_pelagic_forage_fishes=sum(fish_catch_per_tow_NorthernAnchovy,
                                                             fish_catch_per_tow_PacificHerring))


write.csv(FMWT_annual_values_biomass,row.names=FALSE,file=file.path("data","annual_averages","fish_biomass_FMWT.csv"))

write.csv(FMWT_annual_values_CPUE,row.names=FALSE,file=file.path("data","annual_averages","fish_catch_per_tow_FMWT.csv"))


plot(FMWT_annual_values_CPUE$fish_catch_per_tow_Estuarine_pelagic_forage_fishes~FMWT_annual_values_biomass$fish_biomass_Estuarine_pelagic_forage_fishes)


####################----------------------------------------
####################----------------------------------------
###Junk code for potential use at a later time
FMWT_biomass_sum <- FMWT_biomass_derived %>% group_by(Station,Datetime,Survey) %>%
  summarise(Biomass=sum(Biomass)) %>%
  mutate(Year=year(Datetime),Station=as.numeric(Station)) %>%
  mutate(Station_fac=as.factor(Station),Year_fac=as.factor(Year),Season_fac=as.factor(Survey))

hist(FMWT_biomass_sum$Biomass)
hist(log((FMWT_biomass_sum$Biomass)+1))

FMWT_biomass_sum$log_Biomass<-log((FMWT_biomass_sum$Biomass)+1)


FMWT_biomass_sum<-as.data.frame(FMWT_biomass_sum)

model_estuarine_fish_biomass <- glmmTMB(as.integer(log_Biomass) ~ Year_fac + Season_fac + Station_fac,
                                        zi=~.,
                                        family=poisson, data = FMWT_biomass_sum)
summary(model_estuarine_fish_biomass)

model_estuarine_fish_biomass <- lmer(as.integer(log_Biomass) ~ Year_fac + Season_fac + (1|Station_fac), FMWT_biomass_sum)
summary(fm1)

output_dataset<-expand.grid(Year_fac=unique(FMWT_biomass_sum$Year_fac),Season_fac=unique(FMWT_biomass_sum$Season_fac))

output_dataset$Year_prediction<-predict(model_estuarine_fish_biomass,newdata=output_dataset,level=0)

iterations<-5e3
warmup<-iterations/4

model_estuarine_fish_biomass <- brm(as.integer(log_Biomass) ~ Year_fac + (1|Season_fac) + (1|Station_fac),
                                    family=poisson, data=FMWT_biomass_sum,
                                    prior=prior(normal(0,5), class="Intercept")+
                                      prior(normal(0,5), class="b")+
                                      prior(cauchy(0,5), class="sd"),
                                    chains=3, cores=3, control=list(max_treedepth=15),
                                    iter = iterations, warmup = warmup)
