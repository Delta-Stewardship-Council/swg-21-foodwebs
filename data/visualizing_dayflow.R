###merge dayflow datasets for timeperiod of interest###

###downloading daatasets##
#Dayflow Results 1970 - 1983#
data1 <- read.csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/a0a46a1d-bec5-4db9-b331-655e306860ba/download/dayflow-results-1970-1983.csv")
#Dayflow Results 1984 - 1996
data2 <- read.csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/cb04e626-9729-4105-af81-f6e5a37f116a/download/dayflow-results-1984-1996.csv")
#Dayflow Results 1997 - 2020
data3 <- read.csv("https://data.cnra.ca.gov/dataset/06ee2016-b138-47d7-9e85-f46fae674536/resource/21c377fe-53b8-4bd6-9e1f-2025221be095/download/dayflow-results-1997-2020.csv")

#fix "export" and "exports" that changed column names in different years
head(data1)
head(data2)
head(data3)
library(data.table)
setnames(data3, "EXPORTS", "EXPORT")

#combine dataframes
library(dplyr)
combo_dayflow = bind_rows(data1,data2, data3)

#converting calendar year to water year values and adding water year column
#months 10, 11, 12 = (calendar yr + 1). months 1-9, calendar yr = water yr
w.year <- combo_dayflow$Year
oct.nov.dec <- (combo_dayflow$Month) > 9
w.year[oct.nov.dec] <- w.year[oct.nov.dec] + 1
combo_dayflow$Water_year <- w.year

##average the delta-wide outflow per year for DSP first pass study##

#remove 1969 because there's no matching year it in water year, since sampling started in october.
combo_dayflow_CY<-combo_dayflow[-c(1:92), ]

#######average monthly flow##
combo_dayflow<-combo_dayflow[-c(1:92), ]
monthly_average_deltawide_flow<-combo_dayflow %>%
  group_by(Month) %>%
  summarise(value = mean(OUT), mean(TOT), mean(YOLO), mean(SAC), mean(MISC),
            mean(SJR), mean(XGEO),mean(EXPORT))
monthly_average_deltawide_flow<-rename(monthly_average_deltawide_flow,
                                       c("Mean_OUT" = "value"))


##average total outflow, inflow, Yolo, Sac, export by calendar year
annual_average_deltawide_flow<-combo_dayflow_CY %>%
  group_by(Year) %>%
  summarise(value = mean(OUT), mean(TOT), mean(YOLO), mean(SAC), mean(EXPORT))
#name column for flow
annual_average_deltawide_flow_CY<-rename(annual_average_deltawide_flow,
                                         c("Calendar_Year" = "Year","Mean_outflow_CY" =
                                             "value", "Mean_inflow_CY" = "mean(TOT)",
                                      "Mean_YOLO_CY" = "mean(YOLO)", "Mean_SAC_CY" =
                                        "mean(SAC)", "Mean_Export_CY" = "mean(EXPORT)"))

##average total outflow, inflow, Yolo, Sac, export by water year
WY_annual_average_deltawide_flow<-combo_dayflow %>%
  group_by(Water_year) %>%
  summarise(value = mean(OUT), mean(TOT), mean(YOLO), mean(SAC), mean(EXPORT))
#name column for flow
annual_average_deltawide_flow_WY<-rename(WY_annual_average_deltawide_flow,
                                         c("Mean_outflow_WY" = "value", "Mean_inflow_WY" =
                                             "mean(TOT)", "Mean_YOLO_WY" = "mean(YOLO)",
                                           "Mean_SAC_WY" = "mean(SAC)",
                                           "Mean_Export_WY" = "mean(EXPORT)"))
cy_wy_average_flow<- cbind(annual_average_deltawide_flow_CY,annual_average_deltawide_flow_WY)

library(readr)

write_csv(cy_wy_average_flow, file.path("annual_averages","annual_average_deltawide_flow.csv"))







################################
##########Plotting flow#########
################################

##summarize by month##
str(combo_dayflow)
#date_parse the date vector
library(lubridate)
combo_dayflow$Date <- mdy(combo_dayflow$Date)
#extract parts of date vales for month, yday
combo_dayflow$Month <- month(combo_dayflow$Date)
combo_dayflow$JDay <- yday(combo_dayflow$Date)

library(ggplot2)


#by year
ggplot(combo_dayflow, aes(Date, OUT)) + geom_line() + labs(x = "Date", y = "Flow")
#by Jdate and year
ggplot(combo_dayflow, aes(JDay, OUT, group=Year)) + geom_line() +
  labs(x = "Day of year", y = "Flow")

#by month
ggplot(combo_dayflow, aes(factor(Month), OUT)) +
  geom_boxplot() + labs(x = "Month", y = "Flow")

#log monthly flow
library(scales)
ggplot(combo_dayflow, aes(factor(Month), OUT)) +
  geom_boxplot() + labs(x = "Month", y = "Flow")+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x)) + annotation_logticks(sides = "l")

#data points against julian day.#
ggplot(combo_dayflow, aes(JDay, OUT)) + geom_point() + geom_smooth() +
  labs(x = "Julian Day",
  y = "Flow (cfs)") + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x)) +
  annotation_logticks(sides = "l")

