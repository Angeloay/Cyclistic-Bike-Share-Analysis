#load packages
install.packages("tidyverse")
install.packages("skimr")
library(tidyverse)
library(dplyr)
library(lubridate)
library(janitor)
library(skimr)

#setup working directory and import the 12 csv files 
setwd("~/Project1")

tripdata_202103 <- read_csv("202103-divvy-tripdata.csv")
tripdata_202104 <- read_csv("202104-divvy-tripdata.csv")
tripdata_202105 <- read_csv("202105-divvy-tripdata.csv")
tripdata_202106 <- read_csv("202106-divvy-tripdata.csv")
tripdata_202107 <- read_csv("202107-divvy-tripdata.csv")
tripdata_202108 <- read_csv("202108-divvy-tripdata.csv")
tripdata_202109 <- read_csv("202109-divvy-tripdata.csv")
tripdata_202110 <- read_csv("202110-divvy-tripdata.csv")
tripdata_202111 <- read_csv("202111-divvy-tripdata.csv")
tripdata_202112 <- read_csv("202112-divvy-tripdata.csv")
tripdata_202201 <- read_csv("202201-divvy-tripdata.csv")
tripdata_202202 <- read_csv("202202-divvy-tripdata.csv")

#combining the data into one dataframe. 
full_tripdata <- rbind(tripdata_202103, tripdata_202104, tripdata_202105, tripdata_202106, 
                       tripdata_202107, tripdata_202108, tripdata_202109, tripdata_202110, 
                       tripdata_202111, tripdata_202112, tripdata_202201, tripdata_202202)

full_tripdata1 <- full_tripdata %>% rename(trip_id = ride_id, bike_type = rideable_type, 
                                           start_time = started_at, end_time = ended_at, 
                                           start_latitude = start_lat, start_longitude = start_lng, 
                                           end_latitude = end_lat, end_longitude = end_lng, 
                                           member_type = member_casual)

#change start time into date format, add columns for years, month,  day and day of the week.
full_tripdata1$date <- as.Date(full_tripdata1$start_time)
full_tripdata1$year <- format(as.Date(full_tripdata1$date), "%Y")
full_tripdata1$month <- format(as.Date(full_tripdata1$date), "%m")
full_tripdata1$day <- format(as.Date(full_tripdata1$date), "%d")
full_tripdata1$day_of_the_week <- weekdays(full_tripdata1$date)

#Determine trip length and filter out trip length 60 seconds or more. 
full_tripdata1$trip_length <- difftime(full_tripdata1$end_time,full_tripdata1$start_time, units = "mins")

View(full_tripdata1)

full_tripdata2 <- full_tripdata1 %>%
  filter(trip_length >= 1)

#find the max, mean and median different between members and causal riders
aggregate(full_tripdata2$trip_length ~ full_tripdata2$member_type, FUN = max)
aggregate(full_tripdata2$trip_length ~ full_tripdata2$member_type, FUN = mean)
aggregate(full_tripdata2$trip_length ~ full_tripdata2$member_type, FUN = median)


#final
final_tripdata <- full_tripdata2 %>%
  select(start_station_name, end_station_name, start_latitude, start_longitude,
         end_latitude, end_longitude, member_type, date, day_of_the_week, trip_length) %>%
  drop_na(start_station_name)
write_csv(final_tripdata, "final_tripdata.csv")

#count members
member_count <- full_tripdata2 %>%
  count(member_type, sort = TRUE,) %>%
  rename (member_count = n )
write_csv(member_count, "member_count.csv")

#number of rides, show number of rides by rider type and average duration  
weekly_ride <- full_tripdata2 %>%
  mutate(weekday = wday(start_time, label = TRUE)) %>%
  group_by(member_type, date, weekday)  %>%
  summarise(number_of_rides = n(), average_ride_duration = mean(trip_length)) %>%
  arrange(member_type, weekday)
write_csv(weekly_ride, "data_weekly_ride.csv")

#mapping distinct stations names to coordinates
distinct_station <- full_tripdata2 %>%
  select (start_station_name:end_longitude) %>%
  distinct(start_station_id, .keep_all = TRUE)
write_csv(distinct_station, "distinct_station.csv")

#start station most used by each type of rider for mapping
data_start_station <- full_tripdata2 %>%
  group_by(start_station_name, member_type) %>%
  filter(complete.cases(start_station_name)) %>%
  summarise(number_of_riders = n()) %>%
  arrange(number_of_riders)
write_csv(data_start_station , "data_start_station.csv")

#end station most docked by each type of rider for mapping
data_end_station <- full_tripdata2 %>%
  group_by(end_station_name, member_type)%>%
  filter(complete.cases(end_station_name))%>%
  summarise(number_of_riders = n()) %>%
  arrange(number_of_riders)
write_csv(data_end_station, "data_end_station.csv")

#number of unreturn bikes by members_type
unreturn_bikes <- full_tripdata2 %>%
  select(start_station_name, end_station_name, member_type) %>%
  drop_na(start_station_name) %>%
  filter(is.na(end_station_name)) %>%
  count(member_type) %>%
  rename(number_of_unreturn_bikes = n)
write_csv(unreturn_bikes, "unreturn_bikes.csv")

#bike station trips base on members 
monthly_trips <- full_tripdata2 %>%
  select(date, start_station_name, end_station_name, member_type, trip_length) %>%
  drop_na(start_station_name, end_station_name)
write_csv(monthly_trips, "monthly_trips.csv")

#Time rides 
time_trip <- full_tripdata2 %>%
  select(start_time, member_type, date, day_of_the_week, trip_length)
write_csv(time_trip, "time_trip.csv")