get_fish<-function(source, taxa, remove_cols=NULL){
  if(!require(deltafish)){
    stop('The deltafish package is requried, please install with `devtools::install_github("Delta-Stewardship-Council/deltafish")`')
  }
  left_join(open_survey()%>%
              filter(Source==source),
            open_fish()%>%
              filter(Taxa%in%taxa))%>%
    collect()%>%
    select(-any_of(remove_cols))%>%
    mutate(Date=ymd(Date, tz="America/Los_Angeles"),
           Datetime=ymd_hms(Datetime, tz="America/Los_Angeles"))
}

ldc<-function(LTMR, deltafish){

  nm_d<-setdiff(names(deltafish), names(LTMR))
  nm_l<-setdiff(names(LTMR), names(deltafish))

  deltafish<-deltafish%>%
    select(-all_of(nm_d))%>%
    arrange(Station, Latitude, Longitude, Date, Datetime, SampleID, Method, Taxa, Length)
  print(deltafish)

  LTMR<-LTMR%>%
    select(-all_of(nm_l))%>%
    arrange(Station, Latitude, Longitude, Date, Datetime, SampleID, Method, Taxa, Length)%>%
    relocate(names(deltafish))
  print(LTMR)
  waldo::compare(LTMR, deltafish, tolerance=1e-5)
}
