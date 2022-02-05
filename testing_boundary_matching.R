library(tidyverse)
library(rgeos)
library(sp)
library(rgdal)
library(magrittr)
library(raster)
library(sf)

test_data <- read_csv("data/Divvy_Bicycle_Stations.csv") %>%
  dplyr::select(Longitude, Latitude, everything())

neighborhood_map <- read_sf("neighborhoods/geo_export_9c969155-1ad2-4497-b263-d7a54abec1f1.shp")

test_data_sf <- st_as_sf(test_data, coords = c('Longitude', 'Latitude'), crs = st_crs(neighborhood_map))

test_data <- test_data_sf %>% 
  mutate(intersection = as.integer(st_intersects(geometry, neighborhood_map)),
         neighborhood = if_else(is.na(intersection), '', neighborhood_map$community[intersection]))



test_data_done <- tibble(test_data)

tract_map <- read_sf("census_tracts/geo_export_c8ec6a1a-b3b4-490e-9319-f32777c20c5b.shp")

chicago_join <- st_join(neighborhood_map, tract_map, left = FALSE, largest = TRUE)


census_match_map <- read_csv("data/CensusTractsTIGER2010.csv")

tract_to_neighborhood <- function(tract_data, tract_num_column){
  tract_data <- tract_data %>%
                  add_column(neighborhood = NA)
  for (i in 1:nrow(tract_data)){
    row_num <- which(rownames(census_match_map) == tract_num_column[i]) 
    neighborhood_num <- census_match_map$COMMAREA_N[row_num]
    tract_data$neighborhood[i] <- neighborhood_num

  }
  return(tract_data)
}
