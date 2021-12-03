region_assigner<-function(data, analysis, plot=TRUE){

  require(sf)
  require(dplyr)

if(!analysis%in%c("monthly", "annual")){
  stop("analysis must be one of either 'monthly' or 'annual'")
}

  if(!is.logical(plot)){
    stop("plot must be a logical, either TRUE or FALSE")
  }

  if(!all(c("Latitude", "Longitude")%in%names(data))){
    stop("data must contain columns named 'Latitude' and 'Longitude'")
  }

  if("Region"%in%names(data)){
    stop("You can't have another column named 'Region' in your dataset. Please remove or rename that column before using this function")
  }

  regions<-readRDS("data/data_in/regions.Rds")

data_regions<-data%>%
  st_as_sf(coords=c("Longitude", "Latitude"), crs=4326, remove=FALSE)%>%
  st_join(regions)%>%
  st_drop_geometry()%>%
  {if(analysis=="annual"){
    mutate(., Region=if_else(Region=="Far West", NA_character_, Region))
  }else{
    .
  }}

if(plot){
  require(ggplot2)
  p<-ggplot()+
    geom_sf(data=regions, aes(fill=Region))+
    geom_point(data=data_regions%>%select(Longitude, Latitude, Region)%>%distinct(), aes(x=Longitude, y=Latitude, color=is.na(Region)))+
    scale_color_manual(breaks=c(TRUE, FALSE), values=c("black", "white"), name="Is station outside the regions?")
  print(p)
}

out<-data_regions%>%
  filter(!is.na(Region))

return(out)
}
