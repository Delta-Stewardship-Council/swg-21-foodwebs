plt_pilot_model <- function(df, station){
  df_na <- df %>%
    select(Date, .data[[station]]) %>%
    filter(is.na(.data[[station]]))

  df_na[station] = 0

  plt <- ggplot() +
    geom_line(data = df, mapping = aes(Date, .data[[station]])) +
    geom_point(data = df_na, mapping = aes(Date, .data[[station]]), pch = 4, col = 'red', size = 3) +
    theme_bw()

  return(plt)
}

create_station_df <- function(df, station){
  df_station <- df %>%
    select(Date, .data[[station]]) %>%
    arrange(Date)

  return(df_station)
}

create_station_ts <- function(df, station){
  ts <- ts(pull(df, .data[[station]]), frequency = 12, start = c(1995,1), end=c(2020,12))

  return(ts)
}

plt_obs_and_pred <- function(df, ts, forecast, ylab){
  par(mar=c(2,4,1,1))
  plt <- plot(df$Date, ts, xlab = 'Date', ylab = ylab, lwd=2, type='l')
  plt <- plt + lines(df$Date, forecast$fitted, col = 'red')

  return(plt)
}

plt_obs_vs_pred <- function(ts, forecast){
  R_sq <- round(cor.test(ts, forecast$fitted, na.rm=T)$estimate^2, 2)
  plt <- plot(ts, forecast$fitted, xlab = 'Observed', ylab = 'Predicted', pch = '.')
  plt <- plt + abline(0, 1, lty = 2)
  plt <- plt + mtext(side=3,line=-2, adj=0.1, bquote(R^2 == .(R_sq)))

  return(plt)
}

plt_diagnos_obs_pred <- function(df, label){
  plt <- ggplot(df, aes(Date, Imputed_values, color = Imputed)) +
    geom_line(aes(Date, Imputed_values), col = 'gray60') +
    geom_point(size=1) +
    facet_wrap(Station~., ncol=3, scales = 'free_y') +
    scale_color_manual(values = c('gray60', 'red')) +
    labs(title = 'Replacing missing values with ARIMA and Kalman filter', subtitle=label) +
    theme_bw()

  return(plt)
}

model_sum <- function(model_fits){
  for (station in names(model_fits)){
    cat(paste('--------\n',station,'\n--------\n'))

    fit <- model_fits[station][[1]]
    # check summary
    print(summary(fit))

    # plot residuals versus fitted values (=check for heterosedasticity)
    # if problems you might want to try to transform the data first
    par(mar=c(4,4,4,4))
    plot(fit$fitted, fit$residuals,ylab="Residuals",xlab="Fitted values", main = station)

    # check the residuals
    print(forecast::checkresiduals(fit))

    # check several metrics of performance
    print(forecast::accuracy(object = fit))
  }
  return(NULL)
}
