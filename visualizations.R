library(tidyverse)
library(patchwork)
library(rgdal)
library(broom)

acs_geo <- read_rds("data/acs_geo.rds")

p1 <- ggplot(acs_geo) + 
  geom_sf(aes(fill = black_perc)) + 
  coord_sf() +
  scale_fill_viridis_c() +
  theme_classic() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank()) +
  labs(fill = "% Black")

p2 <- ggplot(acs_geo) + 
  geom_sf(aes(fill = hisp_perc)) + 
  coord_sf() +
  scale_fill_viridis_c() +
  theme_classic() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank()) +
  labs(fill = "% Hispanic/Latino")

p3 <- ggplot(acs_neighborhoods) + 
  geom_sf(aes(fill = white_perc)) + 
  coord_sf() +
  scale_fill_viridis_c() +
  theme_classic() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank()) +
  labs(fill = "% White")

p4 <- ggplot(acs_neighborhoods) + 
  geom_sf(aes(fill = asian_perc)) + 
  coord_sf() +
  scale_fill_viridis_c() +
  theme_classic() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank()) +
  labs(fill = "% Asian")

(p1+p2) / (p3+p4)


p5<- ggplot(acs_neighborhoods) + 
  geom_sf(aes(fill = unemp_perc)) + 
  coord_sf() +
  scale_fill_viridis_c() +
  theme_classic() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank()) +
  labs(fill = "% Unemployed")


p6 <- ggplot(acs_neighborhoods) + 
  geom_sf(aes(fill = MEDINC)) + 
  coord_sf() +
  scale_fill_viridis_c() +
  theme_classic() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank()) +
  labs(fill = "Median Income")


p7 <- ggplot(acs_neighborhoods) + 
  geom_sf(aes(fill = avg_hh_size)) + 
  coord_sf() +
  scale_fill_viridis_c() +
  theme_classic() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank()) +
  labs(fill = "Average Household Size")


p8 <- ggplot(acs_neighborhoods) + 
  geom_sf(aes(fill = bach_perc)) + 
  coord_sf() +
  scale_fill_viridis_c() +
  theme_classic() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank()) +
  labs(fill = "% with a College Degree")


(p5+p6) / (p7+p8)



p9 <- ggplot(acs_geo, aes(geometry = geometry)) + 
  geom_sf(aes(fill = station_pop_ratio)) + 
  coord_sf() +
  scale_fill_viridis_c() +
  theme_classic() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank()) +
  labs(fill = "# of Divvy Stations \n to Population Ratio")


p10 <- ggplot(acs_geo, aes(geometry = geometry)) + 
  geom_sf(aes(fill = bike_area)) + 
  coord_sf() +
  scale_fill_viridis_c() +
  theme_classic() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank()) +
  labs(fill = "# of Divvy Station's \n to Area Ratio")


p11 <- ggplot(acs_geo, aes(geometry = geometry)) + 
  geom_sf(aes(fill = not_eng_perc)) + 
  coord_sf() +
  scale_fill_viridis_c() +
  theme_classic() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank()) +
  labs(fill = "% That Speak Language \n Other than English at Home")

p12 <- ggplot(acs_geo, aes(geometry = geometry)) + 
  geom_sf(aes(fill = walk_bike_perc)) + 
  coord_sf() +
  scale_fill_viridis_c() +
  theme_classic() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank()) +
  labs(fill = "% That Walk or \n Bike to Work")


(p9 + p10) / (p11 + p12)

#########################################

#Getting road and bike path data ready

bikeRouteData <- readOGR("data/Bike Routes/geo_export_5acdd40a-defd-4e38-bc51-9d35252ce617.shp")

bikeRouteDataFort <- tidy(bikeRouteData)

roadsData <- readOGR("data/tl_2019_17_prisecroads/tl_2019_17_prisecroads.shp")

roadsDataFort <- tidy(roadsData)

roadsDataChi <- roadsDataFort %>% 
  filter(long > -87.82, lat > 41.64, lat < 42.03)



############################################### 

#Visualizations from presentation

#Index map

ggplot() + 
  geom_sf(data = acs_geo,aes( fill = index, geometry = geometry)) + 
  coord_sf() +
  scale_fill_viridis_c(labels = c("Low", "", "", "","High")) +
  annotate("text", x = -87.87, y = 41.79, label = "Marginalization \n and Socioeconomic \n Hardship Index", size = 4) +
  theme_classic() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(),
        legend.title = element_blank(),
        aspect.ratio = 1,
        legend.position = c(.23,.2))


#Index map with bike routs

ggplot() + 
  geom_sf(data = acs_geo,aes( fill = index, geometry = geometry)) + 
  coord_sf() +
  scale_fill_viridis_c(labels = c("Low", "", "", "","High")) +
  geom_path(data = bikeRouteDataFort,
            aes(x = long, y = lat, group = group),
            color = "white",
            size = .702) +
  annotate("text", x = -87.87, y = 41.79, label = "Marginalization \n and Socioeconomic \n Hardship Index", size = 4) +
  theme_classic() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(),
        legend.title = element_blank(),
        aspect.ratio = 1,
        legend.position = c(.23,.2))

comm_dens_geo <- read_rds("data/comm_dens_geo")


#Divvy connectivity

ggplot() + 
  geom_sf(data = comm_dens_geo, aes(fill = avg_in_2_mi_radius, geometry = geometry)) + 
  coord_sf() +
  scale_fill_viridis_c() +
  annotate("text", x = -87.87, y = 41.78, label = "Avg # of Stations \n in a 2 mi Radius", size = 4) +
  theme_classic() +
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        legend.title = element_blank(),
        aspect.ratio = 1,
        legend.position = c(.23,.2))



