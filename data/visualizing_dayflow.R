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

#######average monthly flow#########

#monthly_average_deltawide_flow<-combo_dayflow %>%
 # group_by(Month) %>%
  #summarise(value = mean(OUT), mean(TOT), mean(YOLO), mean(SAC), mean(MISC),
   #         mean(SJR), mean(XGEO),mean(EXPORT))
#monthly_average_deltawide_flow<-rename(monthly_average_deltawide_flow,
 #                                      c("Mean_OUT" = "value"))

###selecting the correct sites by region:
monthly <- select(combo_dayflow_CY, c('Date','Month', 'Year','OUT', 'RIO', 'WEST' ) )

#OUT is west and farwest because there isn't higher resolution data for this area. RIO is North, and WEST is South.
monthly2<- monthly %>%
  group_by(Year, Month) %>%
  summarise(West = mean(OUT), Farwest = mean(OUT), North = mean(RIO), South = mean(WEST))


##convert from wide to long format
##need to rename the wide columns to just the region for readability


library(reshape2)

monthly2_long<-melt(monthly2, id.vars=c("Year", "Month"),
                              variable.name = "Region",
                              value.name = "Flow")


library(readr)

write_csv(monthly2_long, file.path("data/monthly_averages","monthly_average_flow_byregion.csv"))


##########average annual flow########
head(combo_dayflow_CY)
##average total outflow, inflow, Yolo, Sac, export by calendar year
CY_annual_average_deltawide_flow<-combo_dayflow_CY %>%
  group_by(Year) %>%
  summarise(West = mean(OUT), Farwest = mean(OUT), North = mean(RIO), South = mean(WEST))


##average total outflow, inflow, Yolo, Sac, export by water year
WY_annual_average_deltawide_flow<-combo_dayflow %>%
  group_by(Water_year) %>%
  summarise(meanflow_West_WY = mean(OUT), meanflow_Farwes_WY = mean(OUT), meanflow_North_WY = mean(RIO), meanflow_South_WY = mean(WEST))



cy_wy_average_flow<- cbind(CY_annual_average_deltawide_flow,WY_annual_average_deltawide_flow)

##convert from wide to long format
#remove water_year since it's the same year for both calendar and water yr.

cy_wy_average_flow<-subset(cy_wy_average_flow, select=-c(Water_year))

library(reshape2)

CY_annual_average_deltawide_flow_long<-melt(CY_annual_average_deltawide_flow, id.vars=c("Year"),
     variable.name = "Region",
     value.name = "Flow")


library(readr)

write_csv(CY_annual_average_deltawide_flow_long, file.path("data/annual_averages","annual_average_deltawide_flow_byregion.csv"))







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

