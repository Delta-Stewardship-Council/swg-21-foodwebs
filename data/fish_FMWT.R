#devtools::install_github("sbashevkin/LTMRdata")
library(LTMRdata)
library(tidyverse)
library(lubridate)
library(brms)

setwd("~/GitHub/swg-21-foodwebs")

#Load California Department of Fish and Wildlife
#Fall Midwater Trawl Data
#For Delta Smelt
FMWT_Delta_Smelt <-LTMRdata::fish(sources="FMWT", species="Hypomesus transpacificus", size_cutoff=40)

#Load csv data from Steve Slater that indicates "index stations"
#index stations have been sampled for the whole time series
#"index station" is labeled as 1

FMWT_index_stations<-read.csv(file.path("data","FMWT index stations_SS_BM.csv"))
FMWT_index_stations_only<-FMWT_index_stations %>% filter(Index=="1")

#Limit FMWT data to just index stations
FMWT_Delta_Smelt_derived <- FMWT_Delta_Smelt %>% filter(Station %in% unique(FMWT_index_stations_only$Station)) %>%
  #and only for September to December (survey 3,4,5,6)
  filter(Survey %in% c(3:6)) %>%
  #summarize by station, add year
  group_by(Station,Datetime,Survey,Taxa) %>%
  summarise(CatchCount=sum(Count))%>%
  mutate(Year=year(Datetime))

FMWT_Delta_Smelt_derived$Station<-as.numeric(FMWT_Delta_Smelt_derived$Station)
FMWT_Delta_Smelt_derived$CatchCount<-as.integer(FMWT_Delta_Smelt_derived$CatchCount)
FMWT_Delta_Smelt_derived$Station_fac<-as.factor(FMWT_Delta_Smelt_derived$Station)
FMWT_Delta_Smelt_derived$Year_fac<-as.factor(FMWT_Delta_Smelt_derived$Year)
FMWT_Delta_Smelt_derived$Season_fac<-as.factor(FMWT_Delta_Smelt_derived$Survey)

#Check sample size across years
FMWT_sample_size <- FMWT_Delta_Smelt_derived %>% mutate(sample_size=1) %>%
  group_by(Year,Station) %>% summarise(sample_size=sum(sample_size)) %>%
  spread(Station,sample_size)

#Plot effort across years
ggplot(FMWT_Delta_Smelt_derived %>% mutate(sample_size=1) %>%
         group_by(Year,Station) %>% summarise(sample_size=sum(sample_size)) %>%
         mutate(Station=as.factor(Station))
       ,aes(x=Year,y=Station,fill=sample_size))+geom_tile()

# Model-based estimates --------------------------------------------------------------

iterations<-5000
warmup<-iterations/4
data_model<-FMWT_Delta_Smelt_derived

model_delta_smelt <- brm(as.integer(round(CatchCount)) ~ Year_fac + Season_fac + Station_fac,
                         family=poisson, data=data_model,
                         prior=prior(normal(0,5), class="Intercept")+
                           prior(normal(0,5), class="b"),
                         chains=3, cores=3, control=list(max_treedepth=15),
                         iter = iterations, warmup = warmup)

save(model_delta_smelt, file="FMWT_Delta_Smelt_Model.Rds", compress="xz")
