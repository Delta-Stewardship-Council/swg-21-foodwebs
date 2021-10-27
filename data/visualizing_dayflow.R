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
ggplot(combo_dayflow, aes(JDay, OUT)) + geom_point() + geom_smooth() + labs(x = "Julian Day",
                                                                            y = "Flow (cfs)") + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x)) +
  annotation_logticks(sides = "l")

##average the delta-wide outflow per year for DSP first pass study##

annual_average_deltawide_flow<-combo_dayflow %>%
  group_by(Year) %>%
  summarise(value = mean(OUT))
#raname column for flow
annual_average_deltawide_flow<-rename(annual_average_deltawide_flow,c("Mean_flow" = "value"))

library(readr)
write_csv(annual_average_deltawide_flow,
          file.path("annual_averages","annual_average_deltawide_flow.csv"))
