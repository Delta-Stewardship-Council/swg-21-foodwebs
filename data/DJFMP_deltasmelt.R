# DJFMP_deltasmelt.R
# NCEAS-DSP Food Web Group
# SCM 20211025

# Create annual Delta Smelt average count data across all DJFMP sampling locations 1976-2020
# Final df of above described: avg_count_Y


#########################
##### Online source #####

# https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=244&revision=7


#########################
#### Format Workspace ###

rm(list=ls())
library(dplyr)
library(lubridate)
library(ggplot2)

#########################
######### Data ##########

# Load data
DJFMP_1976_2020_orig <- read.csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.7&entityid=a3e94e8f0cf6f675d716151c1ca17b4f")
#DJFMP_locations <- read.csv("C:/Users/smanugian/Desktop/NCEAS_DSP/FoodWebGroup/ExploreData_DJFMP/DJFMP_Site_Locations - Copy.csv")

#########################
######## Explore ########

# backup
DJFMP_1976_2020 <- DJFMP_1976_2020_orig

# pull Delta Smelt
smelt_ind <- which(DJFMP_1976_2020$OrganismCode == "DSM")
DJFMP_1976_2020 <- DJFMP_1976_2020[smelt_ind, ]

# which sites got smelt? 39
smelt_sites <- unique(DJFMP_1976_2020$Location)

# expand file with time components
DJFMP_1976_2020 <- DJFMP_1976_2020 %>%
  # Character to date, add month year and day
  mutate(Date = as.Date(SampleDate, format = "%m/%d/%Y")) %>%
  mutate(day = day(Date)) %>%
  mutate(month = month(Date)) %>%
  mutate(year = year(Date))

# summary number of rows for each month for each year by location
count_stats_M_Y_Location <- DJFMP_1976_2020 %>%
  group_by(month, year, Location) %>%
  summarise(n = n())

# summary number of rows for each year
count_stats_Y<- DJFMP_1976_2020 %>%
  group_by(year) %>%
  summarise(n = n())

# summarize total count (sum of counts) for each year by location
sum_count_Y_Location <- DJFMP_1976_2020 %>%
  group_by(year, Location) %>%
  summarise(total_count = sum(Count))



############################################
######## First Pass "Final Product" ########

# summarize total average total count catch for each year
avg_count_Y <- DJFMP_1976_2020 %>%
  group_by(year) %>%
  summarise(Avg_Annual_DScatch = mean(Count))

# # save
#write.csv(avg_count_Y, "C:/Users/smanugian/Desktop/NCEAS_DSP/FoodWebGroup/ExploreData_DJFMP/annual_avg_DS_catch.csv", row.names=FALSE)



# #########################
# ########## Viz ##########
#
# # Function for ticks
# number_ticks <- function(n) {function(limits) pretty(limits, n)}
#
# # Plot
# ggplot(data = DJFMP_1976_2020) +
#   geom_tile(aes(x = month, y = year)) +
#   #  scale_y_continuous(breaks = c(1975, 2021)) +
#   scale_y_continuous(breaks = number_ticks(11)) +
#   scale_x_continuous(breaks = number_ticks(12))
#
#
# # add lat lon values from the DJFMP_locations dataframe to the count_stats_M_Y_Station dataframe for mapping symbology viz
# count_stats_M_Y_Station$Latitude_location <- 0
# count_stats_M_Y_Station$Longitude_location <- 0
# count_stats_M_Y_Station$MethodCode <- 0

