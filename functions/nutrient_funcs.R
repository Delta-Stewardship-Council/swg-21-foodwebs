# read in all wq data, pivot to long
read_wq_data <- function(monthly = FALSE) {
  df <- discretewq::wq(Sources = c('EMP'))

  # temp corrections
  df_time <-readr::read_csv('data/data_in/Time_correction_PST.csv')
  df_time <- dplyr::rename(df_time, Time = Time_PST)

  df <- df %>%
    mutate(Time = round(as.integer(as.numeric(hms::as_hms(Datetime))), -1)) %>%
    left_join(df_time, by = c('Time', 'Month')) %>%
    mutate(Temperature = round(Temperature + Correction,2))

  # if monthly, subset by when zoop starts
  if(monthly){
    df <- subset(df, Date >= '1995-01-01')
  }

  return(df)
}

replace_rl <- function(df_wq, val) {
  # define variables
  col_val <- paste0('df_wq$', val)
  col_sign <- paste0('df_wq$', val, '_Sign')
  col_zip <- mapply(list, eval_txt(col_sign), eval_txt(col_val), SIMPLIFY=F)

  # replace unknown RLs with 0.01, since that's the closest RL chronologically
  col_rls <- lapply(col_zip, function(x) ifelse(x[[1]] == '<' & is.na(x[[2]]), 0.01, x[[2]]))
  col_rls <- unname(unlist(col_rls))
  df_wq[val] <- col_rls

  # replace RLs with simulated value
  set.seed(42)
  col_new <- lapply(col_zip, function(x) ifelse(!is.na(x[[2]]) & x[[1]] == '<', unif_analyte(x[[2]]), x[[2]]))
  col_new <- unname(unlist(col_new))

  return(col_new)
}

clean_df <- function(df){
  df <- subset(df, select = -c(DissAmmonia_Sign,DissNitrateNitrite_Sign,DissOrthophos_Sign))

  # pivot longer
  df <- tidyr::pivot_longer(df, cols = c(Temperature, Chlorophyll:TKN, Salinity), names_to = 'Analyte', values_to = 'Value')

  # subset by wanted nutrients
  df_sub <- df[df$Analyte %in% c('Chlorophyll', 'DissNitrateNitrite', 'DissAmmonia', 'Salinity', 'Secchi', 'Temperature', 'DissOrthophos'),]
  df_sub <- subset(df_sub, select = c('Date','Datetime','Station','Latitude','Longitude','Analyte','Value'))
  df_sub$Value <- round(df_sub$Value, 3)

  # exclude dates where any of the analytes don't exist
  df_sub <- na.omit(df_sub)

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


# helper funcs
unif_analyte <- function(rl){
  val <- stats::runif(1, min = 0.001, max = rl)
  val <- round(val, 3)

  return(val)
}

eval_txt <- function(txt){eval(parse(text = txt))}

