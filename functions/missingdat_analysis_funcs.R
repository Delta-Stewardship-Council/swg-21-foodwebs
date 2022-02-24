# ~~~~~~~~~~~
# create timeseries df w/ imputed vals
# ~~~~~~~~~~~

create_ts_df <- function(df, model_params, log_trans = FALSE, monthly) {
  # create matrix of timeseries w/ imputed values
  mat_ts <- create_ts_mat(df, model_params, log_trans)

  # convert to wide df
  df_ts <- as_tibble(mat_ts) %>%
    mutate(Date = df$Date, Analyte = df$Analyte) %>%
    select(Date, Analyte, colnames(mat_ts))

  if(monthly){
    df_ts <- subset(df_ts, Date >= '1995-01-01')
  }

  df_ts <- df_ts %>%
    pivot_longer(cols = colnames(mat_ts),
                 names_to = 'Station',
                 values_to = 'Imputed_values')

  # find missing vals
  df_missing <- df %>%
    pivot_longer(cols = colnames(mat_ts),
                 names_to = 'Station',
                 values_to = 'Raw_values') %>%
    mutate(Missing = ifelse(is.na(Raw_values), TRUE, FALSE))

  # join dfs
  df_ts <- df_ts %>%
    left_join(df_missing, by = c('Date','Analyte','Station')) %>%
    mutate(Imputed = ifelse(Missing == TRUE & Imputed_values > 0, TRUE, FALSE))

  return(df_ts)
}

create_ts_mat <- function(df, model_params, log_trans){
  # clean up df
  df_all <- df %>% arrange(Date)
  Year <- lubridate::year(df$Date)
  df_ts <- df_all[,-c(1,2)]

  # log transform
  if(log_trans){
    df_ts[colnames(df_ts)] <- log10(df_ts[colnames(df_ts)])
  }

  # create empty matrix to populate
  mat_ts <- matrix(NA, nrow(df_ts),(ncol(df_ts)))
  colnames(mat_ts) <- colnames(df_ts)

  for (i in 1:ncol(mat_ts)){
    print(colnames(mat_ts)[i])
    # create ts object
    ts_obj <- create_ts_obj(Year, df_ts, i)

    # interpolate missing values (if any)
    ts_obj <- interp_missing_dat(ts_obj, model_params)

    # log transform
    if(log_trans){
      ts_obj <- 10^(ts_obj)
    }

    # add station data to matrix
    mat_ts[,i] <- ts_obj
  }

  return(mat_ts)
}

create_ts_obj <- function(Year, df_ts, i){
  #create time-series object
  df_imp <- data.frame(Year = Year, y = df_ts[,i])

  ts_obj <- ts(df_imp[2], start= c(1995,1), end = c(2020,12), frequency = 12)

  return(ts_obj)
}

interp_missing_dat <- function(ts_obj, model_params, fit_return = FALSE){
  if(length(which(is.na(ts_obj))) > 0) {
    # fit ARIMA and impute missing values
    fit <- auto.arima(
      ts_obj,
      seasonal = model_params$seasonal,
      stationary = model_params$stationary,
      trace = model_params$trace,
      lambda = model_params$lambda
      )

    if(fit_return){
      return(fit)
    }

    ts_interp <- imputeTS::na_kalman(ts_obj, model = fit$model)

    # identify missing values to impute (and replace in the matrix)
    id.na <- which(is.na(ts_obj))

    # remove missing values at the begining and end of time series
    id.na <- remove_extra_nas(id.na, ts_obj)

    ts_obj[id.na] <- ts_interp[id.na]
  }
  return(ts_obj)
}

remove_extra_nas <- function(id.na, ts_obj){
  ts_split <- split(id.na, cumsum(c(1, diff(id.na) != 1)))  #split sequences of missing values
  last <- length(ts_obj)
  first <- 1

  # id first and last parts of timeseries
  is.first <- sapply(ts_split, function(x) length(which(x %in% first)))
  is.last <- sapply(ts_split, function(x) length(which(x %in% last)))

  if(sum(is.first) > 0 | sum(is.last) > 0){
    ts_split <- ts_split[-c(which(is.last == 1),which(is.first == 1))] #remove them from the list
    id.na <- unlist(ts_split)
  }

  return(id.na)
}


eval_fit <- function(df, fit_return = TRUE){
  # clean up df
  df_all <- df %>% arrange(Date)
  Year <- lubridate::year(df$Date)
  df_ts <- df_all[,-c(1,2)]

  for (i in 1:ncol(df_ts)){
    print(colnames(df_ts)[i])
    # create ts object
    ts_obj <- create_ts_obj(Year, df_ts, i)

    # interpolate missing values (if any)
    fit <- interp_missing_dat(ts_obj, model_params, fit_return = TRUE)

    if(i == 1){
      list_fit <- list(fit)
    } else {
      list_fit <- c(list_fit, list(fit))
    }
  }

  names(list_fit) = colnames(df_ts)[length(which(is.na(ts_obj))) > 0]
  return(list_fit)
}

station_arima_fit <- function(ts, model_params){
  model <- forecast::auto.arima(
    ts,
    seasonal = model_params$seasonal,
    stationary = model_params$stationary,
    trace = model_params$trace,
    lambda = model_params$lambda
  )

  return(model)
}
