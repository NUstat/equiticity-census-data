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
library(corrplot)
library(janitor)



# Reading in community area shapefile
#This data was obtained from https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Community-Areas-current-/cauq-8yn6
neighborhoods <- read_sf("neighborhoods/geo_export_9c969155-1ad2-4497-b263-d7a54abec1f1.shp")

#Reading in data from Chicago Data Snapshots
#This data was obtained from https://datahub.cmap.illinois.gov/dataset/community-data-snapshots-raw-data/resource/8c4e096e-c90c-4bef-9cf1-9028d094296e
#Note that there is an error in the TOT_POP column, population doesn't match the data in the Chicago
#Data Snpshots pdf and is in decimals

acs_0 <- read_csv("data/ACS_Chicago_Agg.csv")

pop_fixed <- read_csv("data/population_fixed.csv")

acs_01 <- full_join(acs_0, pop_fixed, by = c("GEOID" = "comm_num"))

#Getting rid of empty columns
acs <- acs_01[ , colSums(is.na(acs_01)) < nrow(acs_01)] 

#Creating percentage variables
acs <- acs %>%
  mutate(black_perc = (BLACK / pop_fixed)*100,
         hisp_perc = (HISP / pop_fixed)*100,
         unemp_perc = (UNEMP / pop_fixed) * 100,
         white_perc = (WHITE/pop_fixed)*100,
         asian_perc = (ASIAN/pop_fixed)*100,
         avg_hh_size = POP_HH / TOT_HH,
         bach_perc = (BACH / pop_fixed)*100,
         not_eng_perc = (NOT_ENGLISH/pop_fixed) * 100)


#Turning community area identifier numbers into numeric type
neighborhoods$area_num_1 <- as.numeric(neighborhoods$area_num_1)

#Joining ACS data with mapping data
acs_neighborhoods <- full_join(neighborhoods, acs, by = c("area_num_1" = "GEOID")) 

#Reading in Divvy station location data and putting longitude and latitude columns first
#This data was obtained from https://data.cityofchicago.org/Transportation/Divvy-Bicycle-Stations-All-Map/bk89-9dk7
test_data <- read_csv("data/Divvy_Bicycle_Stations.csv") %>%
  dplyr::select(Longitude, Latitude, everything())

#Reading in community area shapefile
#This data was obtained from https://datahub.cmap.illinois.gov/dataset/community-data-snapshots-raw-data/resource/8c4e096e-c90c-4bef-9cf1-9028d094296e
neighborhood_map <- read_sf("neighborhoods/geo_export_9c969155-1ad2-4497-b263-d7a54abec1f1.shp")

#Changing to an sf object
test_data_sf <- st_as_sf(test_data, coords = c('Longitude', 'Latitude'), crs = st_crs(neighborhood_map))

#Mapping stations to community areas
test_data <- test_data_sf %>% 
  mutate(intersection = as.integer(st_intersects(geometry, neighborhood_map)),
         neighborhood = if_else(is.na(intersection), '', neighborhood_map$community[intersection]))


#Turning into a tibble
test_data_done <- tibble(test_data)

#Grouping by community area to find number of stations and docks in service for each community area
try <- test_data_done %>%
  group_by(neighborhood) %>% 
  summarize(n = n(), num_docks_in_service = sum(`Docks in Service`)) 

#Getting rid of first row which has Evanston data
try <- try[-1,]

#Joining bike data to the ACS and community area map data
map_try <- full_join(try, acs_neighborhoods, by = c("neighborhood" = "community"))

#Replacing missing values with zero
map_try[is.na(map_try)] <- 0


#Reading in community area density, area, and pop data from Wikipedia and turning into a tibble
url <- "https://en.wikipedia.org/wiki/Community_areas_in_Chicago"

r <- GET(url)

doc <- readHTMLTable(doc=content(r, "text"))

pop_table <- doc[1]$`Chicago community areas by number, population, and area[8]`
pop_table <- tail(pop_table, 78)
pop_table <- as_tibble(head(pop_table, 77))

#Making better column names and fixing some punctuation issues
colnames(pop_table) <- c("num", "neighborhood", "pop", "area_sq_mi", "area_sq_km", "density_sq_mi", "density_sq_km")
pop_table$neighborhood <- toupper(pop_table$neighborhood)
pop_table$neighborhood[32] <- "LOOP"
pop_table$neighborhood[76] <- "OHARE"

#Joining wiki data with previous data table and turning columns that should be numeric into numeric
pop_data <- full_join(pop_table, map_try, by = c("neighborhood" = "neighborhood"))
pop_data$num <- parse_number(pop_table$num)
pop_data$pop <- parse_number(pop_table$pop)
pop_data$area_sq_mi <- parse_number(pop_table$area_sq_mi)
pop_data$area_sq_km <- parse_number(pop_table$area_sq_km)
pop_data$density_sq_mi <- parse_number(pop_table$density_sq_mi)
pop_data$density_sq_km <- parse_number(pop_table$density_sq_km)

#Adding ratio variables, #bike stations / population, #docks in service to population
pop_data <- pop_data %>%
  mutate(station_pop_ratio = n/pop_fixed,
         dock_pop_ratio = num_docks_in_service/pop_fixed)

#Creating some ratio variables: # bike stations to area in sq mi and # bike stations to population density
pop_data <- pop_data %>%
  mutate(bike_area = n/area_sq_mi,
         bike_density = n/density_sq_mi)


#Changing column name to be more descriptive
names(pop_data)[names(pop_data) == 'n'] <- 'num_of_divvy_stations'


#Reading in census tract-level ACS data
acs_tract <- read_csv("data/census_team_2015_2019_dat.csv")

#Grabbing ID number and corresponding community area number from census tract map data
#This data was obtained from https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Census-Tracts-2010/5jrd-6zik
tract_neighborhoods <- read_csv("data/CensusTractsTIGER2010.csv") %>%
  dplyr::select(TRACTCE10, COMMAREA)

#Grabbing disability related columns from census tract data
acs_disabled <- acs_tract %>%
  dplyr::select(`census tract code`, ends_with("with a disability (estimate)"))

#Summing counts split by age and sex into one column with the total count of disabled people
acs_disabled$tot_disabled <- rowSums(acs_disabled[, 2:13])

#Doing the same thing but with the non-disabled people columns
acs_non_disabled <- acs_tract %>%
  dplyr::select(`census tract code`, ends_with("no disability (estimate)"))

acs_non_disabled$tot_non_disabled <- rowSums(acs_non_disabled[, 2:13])

#Joining the two into a single disability frame
acs_dis <- full_join(acs_disabled, acs_non_disabled, by = c("census tract code" = "census tract code"))

#Joining disability data with rest of ACS tract-level data and turning count into percent
acs_tract_disabled <- full_join(acs_tract, acs_dis) %>%
  mutate(disabled_perc = (tot_disabled / (tot_disabled + tot_non_disabled)) * 100)

#Modifying ID number for joining
acs_tract_disabled$id_num = str_sub(acs_tract_disabled$`census geographic identifier`, -11)

#Reading in census tract shapefile
#This data was obtained from https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Census-Tracts-2010/5jrd-6zik
map_tracts <- read_sf("census_tracts/geo_export_c8ec6a1a-b3b4-490e-9319-f32777c20c5b.shp")

#Joining census tract shape file with ACS w/disability data, creating a variable for percent of 18-35 year olds that are disabled
acs_disabled_0 <- left_join(map_tracts, acs_tract_disabled, by = c("geoid10" = "id_num")) %>%
  mutate(young_disabled_adult_perc = ((`female: 18 to 34 years: with a disability (estimate)` + `male: 18 to 34 years: with a disability (estimate)`) / (`female: 18 to 34 years (estimate)` + `male: 18 to 34 years (estimate)`))*100)

#Joining with tract to neighborhood correspondence table
acs_disabled_1 <- full_join(acs_disabled_0, tract_neighborhoods, by = c("tractce10" = "TRACTCE10"))

#Reading in ACS data on families living below the poverty line
#This data was obtained from the census bureau data portal
pov <- read_csv("data/acs_poverty.csv")

#Modifying ID number for joining
pov$geo_id = str_sub(pov$geoid, -11)

#Joining ACS w/ disability data to poverty data
acs_disabled <- left_join(acs_disabled_1, pov, by = c("geoid10" = "geo_id"))

#Turning joined table into a tibble, selecting desired columns, and grouping census tracts into community areas
acs_neighborhoods_disabled <- acs_disabled %>%
  as_tibble() %>%
  dplyr::select(commarea, tot_disabled, ends_with("with a disability (estimate)")) %>%
  group_by(commarea) %>%
  summarise_each(list(sum))

#Joining with wikipedia data and cleaning up column names
acs_agg_0 <- full_join(acs_neighborhoods_disabled, pop_data, by = c("commarea" = "area_numbe")) %>%
  clean_names()

#Turning into a tibble and creating percent variables

acs_agg <- acs_agg_0 %>%
  as_tibble() %>%
  mutate(non_white_perc = ((pop_fixed - white) / pop_fixed) * 100, 
         disabled_perc = (tot_disabled / pop_fixed) * 100,
         single_parent_perc = (ct_sp_wchild / tot_hh) * 100,
         not_very_well_eng_perc = (ling_iso / pop_fixed) * 100,
         no_internet_perc = (no_internet / tot_hh) * 100,
         unemp_perc = (unemp / pop_fixed) * 100,
         renting_hh_perc = (rent_occ_hu / tot_hh) * 100,
         less_hs_perc = (lt_hs / pop_fixed) * 100,
         no_vech_perc = (no_veh / tot_hh) * 100,
         walk_bike_perc = (walk_bike / pop_fixed) * 100,
         bach_perc = (bach / pop_fixed) * 100) %>%
  dplyr::select(!c(geometry, area, area_num_1, comarea, comarea_id, perimeter, geog))


#Selecting desired variables from poverty-level data table and cleaning column names
poverty <- read_csv("data/poverty_data.csv") %>%
  dplyr::select(GEOID, TRACTA, AMC3E001, AMC3E002, AMC3E003, AMC3E004, AMC3E005, AMC3E006, 
                AMC3E007, AMC3E008, AMC3E009, AMC3E010, AMC3E011, AMC3E012, AMC3E013) %>%
  clean_names()

#Modifying tract ID number for joining
poverty$geo_id_join = str_sub(poverty$geoid, -11)

#Joining to tract-neighborhood correspondence table and cleaning column names
acs_agg_pov <- left_join(tract_neighborhoods, poverty, by = c("TRACTCE10" = "tracta")) %>%
  clean_names()

#Turning into a tibble, selecting desired variables, and aggregating by community area
acs_agg_p <- acs_agg_pov %>%
  as_tibble() %>%
  dplyr::select(commarea, amc3e001, amc3e002, amc3e003, amc3e004, amc3e005, amc3e006, 
                amc3e007, amc3e008, amc3e009, amc3e010, amc3e011, amc3e012, amc3e013) %>%
  group_by(commarea) %>%
  summarise_each(list(sum))

#Turning community area ID number into numeric
acs_agg$commarea <- parse_number(acs_agg$commarea)

#Joining aggregated poverty data with other aggregated ACS data and creating a variable for
#percent of families living below the poverty line
acs_final <- full_join(acs_agg_p, acs_agg, by = c("commarea" = "commarea")) %>%
  mutate(under_pov_perc = ((amc3e002 + amc3e003 + amc3e004) / amc3e001) * 100)

#Reading in data on how people commute to work and cleaning column names
#This data was obtained from the census bureau data portal
commute <- read_csv("data/commute.csv") %>%
  dplyr::select(starts_with("Estimate"), id) %>%
  clean_names()

#Turning tract ID number into numeric
commute$id_num <- parse_number(str_sub(commute$id, -11))

#Reading in community area to tract correspondence data and cleaning column names
tract_neigh <- read_csv("data/CensusTractsTIGER2010.csv") %>%
  dplyr::select(GEOID10, COMMAREA) %>%
  clean_names()

#Joining correspondence table to commuting data and cleaning column names
acs_commute <- left_join(tract_neigh, commute, by = c("geoid10" = "id_num")) %>%
  clean_names() 

#Selecting desired variables and aggregating by community area
acs_commute <- acs_commute %>%
  as_tibble() %>%
  dplyr::select(commarea, estimate_total, estimate_total_car_truck_or_van) %>%
  group_by(commarea) %>%
  summarise_each(list(sum))

#Joining ACS data to commuting data, creating variable for percent of commuters that
#commute by car, truck, or van
acs_done <- full_join(acs_commute, acs_final, by = c("commarea" = "commarea")) %>%
  mutate(car_truck_commute_perc = (estimate_total_car_truck_or_van / estimate_total) * 100)

#Reading in ACS tract level data with info on what industries people work in and
#selecting variables that likely cover office jobs

# We tried this but due to hazy variable definitions, we often ended up with percetnages
# over 100.  Leaving this code in but commented out in case whoever picks this us
# next has a better idea of how to fix it
# 
# acs_new <- read_csv("data/census_team_2015_2019_dat_new.csv") %>%
#   dplyr::select(`census geographic identifier`,`occupation total (estimate)`,
#                 `wholesale trade (estimate)`, `retail trade (estimate)`, `information (estimate)`,
#                 `finance and insurance, and real estate, and rental and leasing (estimate)`,
#                 `professional, scientific, and management, and administrative, and waste management services (estimate)`,
#                 `educational services, and health care and social assistance (estimate)`,
#                 `public administration (estimate)`, 
#                  `management, business, science, and arts occupations (estimate)`,
#                  `sales and office occupations (estimate)`)
# 
# #Creating a variable for a rough estimate of office jobs
# acs_new$office_jobs <- rowSums(acs_new[, 3:9])
# 
# #Modifying tract ID number for joining
# acs_new$id_num = str_sub(acs_new$`census geographic identifier`, -11)
# 
# #Turning ID number to numeric
# acs_new$id_num <- parse_number(acs_new$id_num)
# 
# #Joining with tract to community area correspondence table and cleaning column names
# acs_done <- left_join(tract_neigh, acs_new, by = c("geoid10" = "id_num")) %>%
#   clean_names() 
# 
# #Turning into tibble, selecting desired variables, and aggregating by community area
# acs_done <- acs_done %>%
#   as_tibble() %>%
#   dplyr::select(commarea, office_jobs, occupation_total_estimate) %>%
#   group_by(commarea) %>%
#   summarise_each(list(sum))

#Joining with commuting data
# acs_done <- full_join(acs_done, acs_commute, by = c("commarea" = "commarea"))



#Factor analysis with relevant variables with factor loadings > .3
fac <- factanal(~non_white_perc + disabled_perc + no_vech_perc + renting_hh_perc +
                  single_parent_perc + no_internet_perc + bach_perc +
                  unemp_perc + under_pov_perc + medinc, 
                factors = 1, data = acs_done, scores = "regression")


#Adding column for the index
acs_done$index <- fac$scores

#Turning into tibble and normalizing index
acs_done_for_csv <- acs_done %>%
  as_tibble() %>%
  mutate(index = (index - min(index)) / (max(index) - min(index))) %>%
  dplyr::select(-c(num)) %>%
  rename(tot_families = amc3e001, 
         inc_pov_lt_.5 = amc3e002, inc_pov_.5_to_.74 = amc3e003, inc_pov_.75_to_.99 = amc3e004, 
         inc_pov_1_to_1.24 = amc3e005, inc_pov_1.25_to_1.49 = amc3e006, inc_pov_1.5_to_1.74 = amc3e007, 
         inc_pov_1.75_to_1.84 = amc3e008, inc_pov_1.85_to_1.99 = amc3e009, inc_pov_2_to_2.99 = amc3e010, 
         inc_pov_3_to_3.99 = amc3e011, inc_pov_4_to_4.99 = amc3e012, inc_pov_5_and_up = amc3e013,
         msh_index = index, population = pop_fixed, total_commuters = estimate_total, 
         total_commuters_car_truck_or_van = estimate_total_car_truck_or_van,
         comm_area_name = neighborhood)

#Getting rid of "estimate" suffix
colnames(acs_done) <- gsub("estimate","", colnames(acs_done))

comm_dens <- read_csv("data/comm_density_2021.csv") %>%
  dplyr::select(-c(X1))



acs_done_dens <- full_join(acs_done_for_csv, comm_dens, by = c("comm_area_name" = "community_area")) %>%
  head(-1)


#Writing to csv file
wb <- createWorkbook()

addWorksheet(wb, sheetName = "Sheet1")

writeData(wb, sheet = 1, x = acs_done_dens)

saveWorkbook(wb, file = "curated_data/final_demographic_data_no_geometry.xlsx")



#################

#Adding geometry back in to demographic data and saving as rds

acs_geo <- full_join(acs_done_dens, neighborhoods, by = c("commarea" = "area_num_1")) %>%
  dplyr::select(-c(shape_area.y, shape_len.y)) %>%
  rename(shape_area = shape_area.x, shape_len = shape_len.x)

write_rds(acs_geo, "curated_data/final_demographic_data_with_geometry.rds")


#Adding mapping geometry to divvy connectivity data and writing to rds

comm_dens_geo <- full_join(neighborhoods, comm_dens, by = c("community" = "community_area"))

write_rds(comm_dens_geo, "curated_data/final_demographic_data_with_geometry_and connectivity.rds")

#Joining density/connectivity divvy data to CMAP Community Data Snapshots and writing to excel file
snapshot_connectivity <- full_join(acs_0, comm_dens, by = c("GEOG" = "community_area"))

wb <- createWorkbook()

addWorksheet(wb, sheetName = "Sheet1")

writeData(wb, sheet = 1, x = snapshot_connectivity)

saveWorkbook(wb, file = "curated_data/snapshots_with_connectivity.xlsx")

#Creating dataset used by viz & comm team by changing variable names, making some
#proportion variables, and selected the desired columns

acs_data <- read_rds("curated_data/final_demographic_data_with_geometry.rds")

for_viz_0 <- acs_data %>%
  mutate(prop_white = white/pop, non_white = (pop - white), 
         prop_non_white = non_white / pop, prop_poverty = under_pov_perc/100,
         prop_disabled = tot_disabled / pop) %>%
  rename(community_id = commarea, num_stations = num_of_divvy_stations,
         num_stations_capita = station_pop_ratio, num_bikes = num_docks_in_service,
         num_bikes_capita = dock_pop_ratio, median_income = medinc, msh_index_score = msh_index,
         inc_gt_150k = inc_gt_150, disabled = tot_disabled, single_parent_hh = ct_sp_wchild)

for_viz <- for_viz_0 %>%
  dplyr::select(community_id, community, geometry, num_stations, num_stations_capita, 
                num_bikes, num_bikes_capita, population, white, prop_white,
                non_white, prop_non_white, median_income, inc_lt_25k, inc_25_50k, 
                inc_50_75k, inc_75_100k, inc_100_150k, inc_gt_150k, prop_poverty, disabled,
                prop_disabled, msh_index_score, tot_hh, unemp, unemp_perc, non_white_perc, disabled_perc,
                no_veh, no_vech_perc, rent_occ_hu, renting_hh_perc, single_parent_hh, 
                single_parent_perc, no_internet, no_internet_perc, bach, bach_perc,
                under_pov_perc)

#Writing viz&comm data to rds

write_rds(for_viz, "curated_data/selected_data_for_viz_com_fixed.rds")

for_viz_csv <- for_viz %>%
  dplyr::select(-c(geometry))

#Writing viz&comm data to xlsx without the mapping geometry column

wb <- createWorkbook()

addWorksheet(wb, sheetName = "Sheet1")

writeData(wb, sheet = 1, x = for_viz_csv)

saveWorkbook(wb, file = "curated_data/for_viz_no_geometry.xlsx")


