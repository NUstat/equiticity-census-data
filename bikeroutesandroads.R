library(tidyverse)
library(rgdal)
library(broom)

bikeRouteData <- readOGR("data/Bike Routes/geo_export_5acdd40a-defd-4e38-bc51-9d35252ce617.shp")
bikeRouteDataFort <- tidy(bikeRouteData)
bikeRouteDataFort %>% ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "green", fill = "white") +
  theme_void()

roadsData <- readOGR("data/tl_2019_17_prisecroads/tl_2019_17_prisecroads.shp")
roadsDataFort <- tidy(roadsData)
roadsDataChi <- roadsDataFort %>% 
  filter(long > -87.82, lat > 41.64, lat < 42.03)
roadsDataChi %>% 
  filter(piece == 1) %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "red", fill = "white") +
  theme_void()

ggplot() +
  geom_polygon(data = roadsDataChi,
               aes(x = long, y = lat, group = group),
               color = "red", fill = "white") +
  geom_polygon(data = bikeRouteDataFort,
               aes(x = long, y = lat, group = group),
               color = "green", fill = "white") +
  theme_void()
