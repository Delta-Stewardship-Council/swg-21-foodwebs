# read in all wq data, pivot to long
read_wq_data <- function(monthly = FALSE) {
  df <- discretewq::wq(Sources = c('EMP'))
  df <- tidyr::pivot_longer(df, cols = c(Temperature, Chlorophyll:TKN, Salinity), names_to = 'Analyte', values_to = 'Value')

  # subset by wanted nutrients
  df_sub <- df[df$Analyte %in% c('Chlorophyll', 'DissNitrateNitrite', 'DissAmmonia', 'Salinity', 'Secchi', 'Temperature', 'TotPhos'),]
  df_sub <- subset(df_sub, select = c('Date','Station','Latitude','Longitude','Analyte','Value'))

  # exclude dates where any of the analytes don't exist
  df_sub <- na.omit(df_sub)

  # if monthly, subset by when zoop starts
  if(monthly){
    df_sub <- subset(df_sub, Date >= '1995-01-01')
  }

  return(df_sub)
}

# combine wq stations that are related
combine_wq_stations <- function(df) {
  unique_stations <- unique(df$Station)

  if('C10' %in% unique_stations & 'C10A' %in% unique_stations){
    min_C10A_date <- min(df$Date[df$Station == 'C10A'])
    df <- subset(df, !(Station == 'C10' & Date >= min_C10A_date))
    df$Station[df$Station == 'C10'] <- 'C10A'
  }

  if('C3' %in% unique_stations & 'C3A' %in% unique_stations){
    max_C3_date <- max(df$Date[df$Station == 'C3'])
    df <- subset(df, !(Station == 'C3A' & Date <= max_C3_date))
    df$Station[df$Station == 'C3'] <- 'C3A'
  }

  if('MD10' %in% unique_stations & 'MD10A' %in% unique_stations){
    min_MD10A_date <- min(df$Date[df$Station == 'MD10A'])
    df <- subset(df, !(Station == 'MD10' & Date >= min_C10A_date))
    df$Station[df$Station == 'MD10'] <- 'MD10A'
  }

  return(df)
}

# create station map
create_station_map <- function(df, buffer_lvl = 0.05, zoom_lvl = 10){
  buffer <- buffer_lvl
  coordDict = list(
    'minLat' = min(df$Latitude) - buffer,
    'maxLat' = max(df$Latitude) + buffer,
    'minLon' = min(df$Longitude) - buffer,
    'maxLon' = max(df$Longitude) + buffer
  )

  map_obj <- get_stamenmap(
    bbox = c(left = coordDict[['minLon']], bottom = coordDict[['minLat']], right = coordDict[['maxLon']], top = coordDict[['maxLat']]),
    zoom = zoom_lvl,
    maptype = 'terrain'
  )

  map <- ggmap(map_obj) +
    theme_void() +
    geom_point(data = df, aes(x = Longitude, y = Latitude, label = Station), shape = 21, fill = 'red', size = 3) +
    geom_text(data = df, aes(label = Station, x = Longitude, y = Latitude), vjust = 0, hjust = 0)

  return(map)
}

check_temporal_coverage <- function(df){
  plt <- ggplot(df, aes(Date, Station)) +
    geom_point() +
    theme(axis.text.x = element_text(angle = 90))

  return(plt)
}
