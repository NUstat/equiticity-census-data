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

census_api_key("e4e8307d78310acf72101521411813bf40c2d83f")

bike_loc <- read_csv("data/Divvy_Bicycle_Stations.csv") %>%
  dplyr::select(Longitude, Latitude, everything()) 

neighborhoods <- read_sf("neighborhoods/geo_export_9c969155-1ad2-4497-b263-d7a54abec1f1.shp")

acs_0 <- read_csv("data/ACS_Chicago_Agg.csv")

acs <- acs_0[ , colSums(is.na(acs_0)) < nrow(acs_0)] 

neighborhoods$area_num_1 <- as.numeric(neighborhoods$area_num_1)

acs_neighborhoods <- full_join(neighborhoods, acs, by = c("area_num_1" = "GEOID")) 


test_data <- read_csv("data/Divvy_Bicycle_Stations.csv") %>%
  dplyr::select(Longitude, Latitude, everything())

neighborhood_map <- read_sf("neighborhoods/geo_export_9c969155-1ad2-4497-b263-d7a54abec1f1.shp")

test_data_sf <- st_as_sf(test_data, coords = c('Longitude', 'Latitude'), crs = st_crs(neighborhood_map))

test_data <- test_data_sf %>% 
  mutate(intersection = as.integer(st_intersects(geometry, neighborhood_map)),
         neighborhood = if_else(is.na(intersection), '', neighborhood_map$community[intersection]))



test_data_done <- tibble(test_data)



acs_neighborhoods <- acs_neighborhoods %>%
  mutate(black_perc = (BLACK / TOT_POP)*100,
         hisp_perc = (HISP / TOT_POP)*100,
         unemp_perc = (UNEMP / TOT_POP) * 100,
         white_perc = (WHITE/TOT_POP)*100,
         asian_perc = (ASIAN/TOT_POP)*100,
         avg_hh_size = POP_HH / TOT_HH,
         bach_perc = (BACH / TOT_POP)*100,
         not_eng_perc = (NOT_ENGLISH/TOT_POP) * 100)


#test_data_done$for_sum <- 1

try <- test_data_done %>%
  group_by(neighborhood) %>% 
  summarize(n = n(), num_docks_in_service = sum(`Docks in Service`)) 

try <- try[-1,]

map_try <- full_join(try, acs_neighborhoods, by = c("neighborhood" = "community"))


map_try[is.na(map_try)] <- 0

map_try <- map_try %>%
  mutate(station_pop_ratio = n/TOT_POP,
         dock_pop_ratio = num_docks_in_service/TOT_POP)




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



names(pop_data)[names(pop_data) == 'n'] <- 'num_of_divvy_stations'



acs_tract <- read_csv("data/census_team_2015_2019_dat.csv")

tract_neighborhoods <- read_csv("data/CensusTractsTIGER2010.csv") %>%
  dplyr::select(TRACTCE10, COMMAREA)

acs_disabled <- acs_tract %>%
  dplyr::select(`census tract code`, ends_with("with a disability (estimate)"))

acs_disabled$tot_disabled <- rowSums(acs_disabled[, 2:13])

acs_non_disabled <- acs_tract %>%
  dplyr::select(`census tract code`, ends_with("no disability (estimate)"))

acs_non_disabled$tot_non_disabled <- rowSums(acs_non_disabled[, 2:13])

acs_dis <- full_join(acs_disabled, acs_non_disabled, by = c("census tract code" = "census tract code"))

acs_tract_disabled <- full_join(acs_tract, acs_dis) %>%
  mutate(disabled_perc = (tot_disabled / (tot_disabled + tot_non_disabled)) * 100)

acs_tract_disabled$id_num = str_sub(acs_tract_disabled$`census geographic identifier`, -11)

map_tracts <- read_sf("census_tracts/geo_export_c8ec6a1a-b3b4-490e-9319-f32777c20c5b.shp")

acs_disabled_0 <- left_join(map_tracts, acs_tract_disabled, by = c("geoid10" = "id_num")) %>%
  mutate(young_disabled_adult_perc = ((`female: 18 to 34 years: with a disability (estimate)` + `male: 18 to 34 years: with a disability (estimate)`) / (`female: 18 to 34 years (estimate)` + `male: 18 to 34 years (estimate)`))*100)

acs_disabled_1 <- full_join(acs_disabled_0, tract_neighborhoods, by = c("tractce10" = "TRACTCE10"))


pov <- read_csv("data/acs_poverty.csv")

pov$geo_id = str_sub(pov$geoid, -11)

acs_disabled <- left_join(acs_disabled_1, pov, by = c("geoid10" = "geo_id"))
  

acs_neighborhoods_disabled <- acs_disabled %>%
  as_tibble() %>%
  dplyr::select(commarea, tot_disabled, ends_with("with a disability (estimate)")) %>%
  group_by(commarea) %>%
  summarise_each(list(sum))


acs_agg_0 <- full_join(acs_neighborhoods_disabled, pop_data, by = c("commarea" = "area_numbe")) %>%
  clean_names()







acs_agg_map <- acs_agg_0 %>%
  as_tibble() %>%
  mutate(non_white_perc = ((hisp + black + asian + other) / pop) * 100, 
         disabled_perc = (tot_disabled / pop) * 100,
         single_parent_perc = (ct_sp_wchild / tot_hh) * 100,
         not_very_well_eng_perc = (ling_iso / pop) * 100,
         no_internet_perc = (no_internet / tot_hh) * 100,
         unemp_perc = (unemp / pop) * 100,
         renting_hh_perc = (rent_occ_hu / tot_hh) * 100,
         less_hs_perc = (lt_hs / pop) * 100,
         no_vech_perc = (no_veh / tot_hh) * 100,
         walk_bike_perc = (walk_bike / pop) * 100)


write_rds(acs_agg_map, "combined_community_with_map.rds")


acs_agg <- acs_agg_0 %>%
  as_tibble() %>%
  mutate(non_white_perc = ((hisp + black + asian + other) / pop) * 100, 
         disabled_perc = (tot_disabled / pop) * 100,
         single_parent_perc = (ct_sp_wchild / tot_hh) * 100,
         not_very_well_eng_perc = (ling_iso / pop) * 100,
         no_internet_perc = (no_internet / tot_hh) * 100,
         unemp_perc = (unemp / pop) * 100,
         renting_hh_perc = (rent_occ_hu / tot_hh) * 100,
         less_hs_perc = (lt_hs / pop) * 100,
         no_vech_perc = (no_veh / tot_hh) * 100,
         walk_bike_perc = (walk_bike / pop) * 100) %>%
  dplyr::select(!c(geometry, area, area_num_1, comarea, comarea_id, perimeter, geog))




wb <- createWorkbook()

addWorksheet(wb, sheetName = "Sheet1")

writeData(wb, sheet = 1, x = acs_agg)

saveWorkbook(wb, file = "combined_community_feb_8.xlsx")


poverty <- read_csv("data/poverty_data.csv") %>%
  dplyr::select(GEOID, TRACTA, AMC3E001, AMC3E002, AMC3E003, AMC3E004, AMC3E005, AMC3E006, 
                AMC3E007, AMC3E008, AMC3E009, AMC3E010, AMC3E011, AMC3E012, AMC3E013) %>%
  clean_names()

poverty$geo_id_join = str_sub(poverty$geoid, -11)

acs_agg_pov <- left_join(tract_neighborhoods, poverty, by = c("TRACTCE10" = "tracta")) %>%
  clean_names()

acs_agg_p <- acs_agg_pov %>%
  as_tibble() %>%
  dplyr::select(commarea, amc3e001, amc3e002, amc3e003, amc3e004, amc3e005, amc3e006, 
                amc3e007, amc3e008, amc3e009, amc3e010, amc3e011, amc3e012, amc3e013) %>%
  group_by(commarea) %>%
  summarise_each(list(sum))

acs_agg$commarea <- parse_number(acs_agg$commarea)

acs_final <- full_join(acs_agg_p, acs_agg, by = c("commarea" = "commarea")) %>%
  mutate(under_pov_perc = ((amc3e002 + amc3e003 + amc3e004) / amc3e001) * 100)

# Switch HS for bachelors

# Throw out walk or bike to work

# Use people who commute by car

#


for_cor_0 <- pop_data %>%
  dplyr::select(CT_SP_WCHILD, LING_ISO, NO_INTERNET, NO_VEH, UNEMP, MEDINC, RENT_OCC_HU, INCPERCAP, LT_HS)

for_cor <- acs_agg %>%
  dplyr::select(non_white_perc, disabled_perc, single_parent_perc, not_very_well_eng_perc, no_internet_perc, unemp_perc,
                renting_hh_perc, less_hs_perc, no_vech_perc)

for_cor_1 <- acs_agg %>%
  dplyr::select(non_white_perc, disabled_perc, single_parent_perc, no_internet_perc, unemp_perc,
                renting_hh_perc, no_vech_perc, medinc, walk_bike_perc, not_very_well_eng_perc)

for_fact_0 <- acs_final %>%
  dplyr::select(non_white_perc, disabled_perc, no_vech_perc, renting_hh_perc, 
                single_parent_perc, not_very_well_eng_perc, no_internet_perc, 
                unemp_perc, bach, under_pov_perc, medinc)

for_fact <- acs_final %>%
  dplyr::select(non_white_perc, disabled_perc,
                single_parent_perc, no_internet_perc, 
                unemp_perc, bach, medinc)

fac <- factanal(for_fact, 1)

print(fac$loadings, cutoff = 0.3)

1-fac$uni

corrplot(cor(for_fact))


write_rds(acs_final, "acs_agg_with_pov.rds")

commute <- read_csv("data/commute.csv") %>%
  dplyr::select(starts_with("Estimate"), id) %>%
  clean_names()

commute$id_num <- parse_number(str_sub(commute$id, -11))



tract_neigh <- read_csv("data/CensusTractsTIGER2010.csv") %>%
  dplyr::select(GEOID10, COMMAREA) %>%
  clean_names()


acs_commute <- left_join(tract_neigh, commute, by = c("geoid10" = "id_num")) %>%
  clean_names() 


acs_commute <- acs_commute %>%
  as_tibble() %>%
  dplyr::select(commarea, estimate_total, estimate_total_car_truck_or_van) %>%
  group_by(commarea) %>%
  summarise_each(list(sum))

acs_commute <- full_join(acs_commute, acs_final, by = c("commarea" = "commarea")) %>%
  mutate(car_truck_commute_perc = (estimate_total_car_truck_or_van / estimate_total) * 100)


acs_new <- read_csv("data/census_team_2015_2019_dat_new.csv") %>%
  dplyr::select(`census geographic identifier`,`occupation total (estimate)`,
  `wholesale trade (estimate)`, `retail trade (estimate)`, `information (estimate)`,
   `finance and insurance, and real estate, and rental and leasing (estimate)`,
   `professional, scientific, and management, and administrative, and waste management services (estimate)`,
   `educational services, and health care and social assistance (estimate)`,
   `public administration (estimate)`, 
   `management, business, science, and arts occupations (estimate)`,
   `sales and office occupations (estimate)`)

acs_new$office_jobs <- rowSums(acs_new[, 3:11])




acs_new$id_num = str_sub(acs_new$`census geographic identifier`, -11)

acs_new$id_num <- parse_number(acs_new$id_num)

acs_done <- left_join(tract_neigh, acs_new, by = c("geoid10" = "id_num")) %>%
  clean_names() 


acs_done <- acs_done %>%
  as_tibble() %>%
  dplyr::select(commarea, office_jobs, occupation_total_estimate) %>%
  group_by(commarea) %>%
  summarise_each(list(sum))

acs_done <- full_join(acs_done, acs_commute, by = c("commarea" = "commarea")) %>%
   mutate(office_jobs_perc = (office_jobs / occupation_total_estimate) * 100)
# 
# 
# 
# (`wholesale trade (estimate)`, `retail trade (estimate)`, `information (estimate)`,
# `finance and insurance, and real estate, and rental and leasing (estimate)`,
# `professional, scientific, and management, and administrative, and waste management services (estimate)`,
# `educational services, and health care and social assistance (estimate)`,
# `arts, entertainment, and recreation, and accommodation and food services (estimate)`,
# `other services, except public administration (estimate)`,
# `public administration (estimate)`, `management, business, science, and arts occupations (estimate)`,
# `sales and office occupations (estimate)`


#################################


for_fact_com <- acs_done %>%
  dplyr::select(non_white_perc, disabled_perc, no_vech_perc, renting_hh_perc, 
              single_parent_perc, no_internet_perc, unemp_perc, bach, 
              under_pov_perc, medinc, office_jobs_perc)


fac <- factanal(for_fact_com, 1)

print(fac$loadings)

1-fac$uni

corrplot(cor(for_fact_com))


####################


# for_fact_com <- acs_done %>%
#   dplyr::select(non_white_perc, disabled_perc, no_vech_perc, renting_hh_perc, 
#                 single_parent_perc, no_internet_perc, bach,
#                 unemp_perc, under_pov_perc, medinc, office_jobs_perc)
# 
# 
# fac <- factanal(for_fact_com, 1, scores = "regression")
# 
# print(fac$loadings)
# 
# 1-fac$uni



#############

# for_fact_com <- acs_done %>%
#   dplyr::select(non_white_perc, disabled_perc, no_vech_perc, renting_hh_perc, 
#                 single_parent_perc, no_internet_perc, bach,
#                 unemp_perc, under_pov_perc, medinc, office_jobs_perc)
# 

fac <- factanal(~non_white_perc + disabled_perc + no_vech_perc + renting_hh_perc +
                single_parent_perc + no_internet_perc + bach +
                unemp_perc + under_pov_perc + medinc + office_jobs_perc, factors = 1, data = acs_done, scores = "regression")

acs_done$disadvantage_index <- fac$scores



#################


acs_geo <- full_join(acs_done, neighborhoods, by = c("commarea" = "area_num_1"))

ggplot(acs_geo) + 
  geom_sf(aes(fill = disadvantage_index, geometry = geometry)) + 
  coord_sf() +
  scale_fill_viridis_c() +
  theme_classic() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank()) +
  labs(fill = "Disadvantage Index")

