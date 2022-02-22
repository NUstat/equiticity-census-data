library(tidyverse)
library(rgeos)
library(sp)
library(rgdal)
library(magrittr)
library(raster)
library(sf)



#First, read in the data with the coordinates you want to match to geographical areas (bike data, traffic stops, etc...)

point_data <- read_csv("data/Divvy_Bicycle_Stations.csv")

#Then read in the shape file that outlines your geographic units (neighborhoods, census tracts, etc...)
geo_data <- read_sf("neighborhoods/geo_export_9c969155-1ad2-4497-b263-d7a54abec1f1.shp")


#I tried to get the neighborhood part to change but it made things too complicated.  So if, for example,  you are working 
#with census tracts, you should switch all instances of neighborhood to tract so it makes more sense. 


#column_name should be the name of the column (string) in geo_data that contains the name of the geographical area you are using


#assign_if_not_in should be a string that is using in the geographical area column if the point from point_data does
#not fall into any of the geographical areas.  For example, if you are looking at bike stations and neighborhoods,
#if the station is in Evanston the neighborhood column will contain assign_if_not_in for that row


#long_lat_vec should be a vector of the names of your longitude and latitude columns, in that order.  For example,
#long_lat_vec = c("Longitude, "Latitude")



map_point_to_geo <- function(point_data, geo_data, column_name, assign_if_not_in, long_lat_vec){
  
  point_data_sf <- st_as_sf(point_data, coords = long_lat_vec, crs = st_crs(geo_data))
  
  #intersection column 
  point_data <- point_data_sf %>% 
    mutate(intersection = as.integer(st_intersects(geometry, geo_data)),
           neighborhood = if_else(is.na(intersection), '', geo_data[[column_name]][intersection])) 
  
  point_data_done <- tibble(point_data) %>%
    mutate(neighborhood = ifelse(is.na(intersection), assign_if_not_in, neighborhood)) %>%
    dplyr::select(-intersection)
  
  return(point_data_done)
}


#Example of calling function

final_data <- map_point_to_geo(point_data, geo_data, "neighborhood", "Evanston", c("Longitude", "Latitude"))


