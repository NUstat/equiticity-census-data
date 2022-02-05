library(tidyverse)
library(tidycensus)
library(rgdal)
library(rgeos)
library(sp)
library(magrittr)
library(raster)
library(sf)
library(httr)
library(XML)
library(viridis)
library(patchwork)
library(openxlsx)



bike_loc <- read_csv("data/Divvy_Bicycle_Stations.csv") %>%
  dplyr::select(Longitude, Latitude, everything()) 

neighborhoods <- read_sf("neighborhoods/geo_export_9c969155-1ad2-4497-b263-d7a54abec1f1.shp")

acs_0 <- read_csv("data/ACS_Chicago_Agg.csv")

acs <- acs_0[ , colSums(is.na(acs_0)) < nrow(acs_0)] 

neighborhoods$area_num_1 <- as.numeric(neighborhoods$area_num_1)

acs_neighborhoods <- full_join(neighborhoods, acs, by = c("area_num_1" = "GEOID")) 


acs_neighborhoods <- acs_neighborhoods %>%
  mutate(black_perc = (BLACK / TOT_POP)*100,
         hisp_perc = (HISP / TOT_POP)*100,
         unemp_perc = (UNEMP / TOT_POP) * 100,
         white_perc = (WHITE/TOT_POP)*100,
         asian_perc = (ASIAN/TOT_POP)*100,
         avg_hh_size = POP_HH / TOT_HH,
         bach_perc = (BACH / TOT_POP)*100,
         not_eng_perc = (NOT_ENGLISH/TOT_POP) * 100)

p1 <- ggplot(acs_neighborhoods) + 
  geom_sf(aes(fill = black_perc)) + 
  coord_sf() +
  scale_fill_viridis_c() +
  theme_classic() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank()) +
  labs(fill = "% Black")

ggplot(acs_neighborhoods) + 
  geom_sf(aes(fill = black_perc)) +
  geom_point(data = bike_loc, aes(x = Longitude, y = Latitude), alpha = .2, color = "red") +
  coord_sf() +
  scale_fill_viridis_c() +
  theme_classic() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank()) +
  labs(fill = "% Black")


ggplot(acs_neighborhoods) + 
  geom_sf(aes(fill = TOT_POP)) + 
  coord_sf() +
  geom_point(data = bike_loc, aes(x = Longitude, y = Latitude), alpha = .2, color = "red") +
  scale_fill_viridis_c() +
  theme_classic() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank())



p2 <- ggplot(acs_neighborhoods) + 
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


ggplot(acs_neighborhoods) + 
  geom_sf(aes(fill = black_prop)) + 
  coord_sf() +
  scale_fill_viridis_c() +
  theme_classic() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank())



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

#test_data_done$for_sum <- 1

try <- test_data_done %>%
  group_by(neighborhood) %>% 
  summarize(n = n(), num_docks_in_service = sum(`Docks in Service`)) 

try <- try[-1,]

map_try <- full_join(try, acs_neighborhoods, by = c("neighborhood" = "community"))


map_try[is.na(map_try)] <- 0

map_try <- map_try %>%
  mutate(station_pop_ratio = n/TOT_POP)


p9 <- ggplot(map_try, aes(geometry = geometry)) + 
  geom_sf(aes(fill = station_pop_ratio)) + 
  coord_sf() +
  scale_fill_viridis_c() +
  theme_classic() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank()) +
  labs(fill = "# of Divvy Stations \n to Population Ratio")


url <- "https://en.wikipedia.org/wiki/Community_areas_in_Chicago"

r <- GET(url)

doc <- readHTMLTable(doc=content(r, "text"))

pop_table <- doc[1]$`Chicago community areas by number, population, and area[8]`
pop_table <- tail(pop_table, 78)
pop_table <- as_tibble(head(pop_table, 77))

colnames(pop_table) <- c("num", "neighborhood", "pop", "area_sq_mi", "area_sq_km", "density_sq_mi", "density_sq_km")
pop_table$neighborhood <- toupper(pop_table$neighborhood)
pop_table$neighborhood[32] <- "LOOP"
pop_table$neighborhood[76] <- "OHARE"

pop_data <- full_join(pop_table, map_try, by = c("neighborhood" = "neighborhood"))
pop_data$num <- parse_number(pop_table$num)
pop_data$pop <- parse_number(pop_table$pop)
pop_data$area_sq_mi <- parse_number(pop_table$area_sq_mi)
pop_data$area_sq_km <- parse_number(pop_table$area_sq_km)
pop_data$density_sq_mi <- parse_number(pop_table$density_sq_mi)
pop_data$density_sq_km <- parse_number(pop_table$density_sq_km)

pop_data <- pop_data %>%
  mutate(bike_area = n/area_sq_mi,
         bike_density = n/density_sq_mi)

p10 <- ggplot(pop_data, aes(geometry = geometry)) + 
  geom_sf(aes(fill = bike_area)) + 
  coord_sf() +
  scale_fill_viridis_c() +
  theme_classic() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank()) +
  labs(fill = "# of Divvy Station's \n to Area Ratio")


p11 <- ggplot(pop_data, aes(geometry = geometry)) + 
  geom_sf(aes(fill = not_eng_perc)) + 
  coord_sf() +
  scale_fill_viridis_c() +
  theme_classic() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank()) +
  labs(fill = "% That Speak Language \n Other than English at Home")

pop_data <- pop_data %>%
  mutate(walk_bike_perc = (WALK_BIKE/TOT_POP)*100)


p12 <- ggplot(pop_data, aes(geometry = geometry)) + 
  geom_sf(aes(fill = walk_bike_perc)) + 
  coord_sf() +
  scale_fill_viridis_c() +
  theme_classic() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank()) +
  labs(fill = "% That Walk or \n Bike to Work")


(p9+p10) / (p11+p12)

names(pop_data)[names(pop_data) == 'n'] <- 'num_of_divvy_stations'

for_nicole <- pop_data %>%
  dplyr::select(neighborhood, pop, num_of_divvy_stations, num_docks_in_service, area_sq_mi)

pop_data_to_write <- pop_data %>%
  dplyr::select(-geometry)

wb <- createWorkbook()

addWorksheet(wb, sheetName = "Sheet1")

writeData(wb, sheet = 1, x = pop_data_to_write)

saveWorkbook(wb, file = "combined_data_sans_geometry.xlsx")


