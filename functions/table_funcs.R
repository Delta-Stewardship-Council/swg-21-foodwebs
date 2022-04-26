coef_tabler<-function(..., name){
  models<-tibble::lst(...)
  tab<-purrr::map_dfr(models, ~as.data.frame(lavaan::standardizedsolution(.x)), .id = "Region")%>%
    dplyr::mutate(Region=ifelse(Region=="modfit1_dtr", Region, stringr::str_remove(Region, "_dtr")),
                  Region=dplyr::recode(Region, modfitFW="San Pablo", modfitW="Suisun", modfitS="San Joaquin", modfitN="Sacramento",
                                       modfitwest="Suisun", modfitnorth="Sacramento", modfitsouth="San Joaquin",
                                       modfit1="Original", modfit1_dtr="Detrended"),
                  Model=name)%>%
    dplyr::mutate(dplyr::across(c(est.std, se, pvalue), ~round(.x, 4)))%>%
    dplyr::filter(op!=":=")%>%
    dplyr::select(Model, Region, Response=lhs, op, Predictor=rhs, Effect=est.std, se, pvalue)

  # For annual noregion models
    if("Original"%in%tab$Region){
      tab<-tab%>%
        dplyr::mutate(Model=Region,
                      Region="Whole Estuary")
    }
  return(tab)
}
