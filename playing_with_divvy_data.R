library(tidyverse)
library(lubridate)

#make sure you have the divvy trips data setup

#charting Divvy membership status by Starting station
#this graph is pretty ugly so yeah
members_by_station <- Divvy_Trips_2020_Q1_data %>%
  group_by(start_station_name) %>%
  mutate(rides = n())

#there is something fishy going on with HQ QR - it's a clear outlier
HQ <- Divvy_Trips_2020_Q1_data %>%
  filter(start_station_name == "HQ QR")

#there's a lot of funkiness here! negative time, lots of rides...
HQ <- HQ %>%
  mutate(difference_time = difftime(ended_at, started_at)) %>%
  arrange(desc(difference_time))

#SOLUTION: IT IS THE BIKE WAREHOUSE
#IGNORE 'HQ QR' in our analysis


#getting top 50, split by member status, starting station, and ending station
station_summary <- Divvy_Trips_2020_Q1_data %>%
  group_by(start_station_name, end_station_name, member_casual) %>%
  summarize(rides = n()) %>%
  #the top_n is a little funky with all the different groupings, I don't think it's actually working
  #top_n(50) %>%
  arrange(desc(rides))

#a very bad graph of station popularity by member/casual
ggplot(members_by_station, aes(start_station_name)) +
  geom_bar() +
  facet_wrap(~ member_casual)

