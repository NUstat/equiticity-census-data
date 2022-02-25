library(tidyverse)

acs_data <- read_rds("data/acs_geo.rds")

for_viz_0 <- acs_data %>%
  mutate(prop_white = white/pop, non_white = hisp + black + asian + other, 
         prop_non_white = non_white / pop, prop_poverty = under_pov_perc/100,
         prop_disabled = tot_disabled / pop) %>%
  rename(community_id = commarea, num_stations = num_of_divvy_stations,
         num_stations_capita = station_pop_ratio, num_bikes = num_docks_in_service,
         num_bikes_capita = dock_pop_ratio, population = pop, median_income = medinc,
         inc_gt_150k = inc_gt_150, disabled = tot_disabled, single_parent_hh = ct_sp_wchild)

for_viz <- for_viz_0 %>%
  dplyr::select(community_id, community, geometry, num_stations, num_stations_capita, 
         num_bikes, num_bikes_capita, population, white, prop_white,
         non_white, prop_non_white, median_income, inc_lt_25k, inc_25_50k, 
         inc_50_75k, inc_75_100k, inc_100_150k, inc_gt_150k, prop_poverty, disabled,
         prop_disabled, index, unemp, unemp_perc, non_white_perc, disabled_perc,
         no_veh, no_vech_perc, rent_occ_hu, renting_hh_perc, single_parent_hh, 
         single_parent_perc, no_internet, no_internet_perc, bach, bach_perc,
         under_pov_perc)


write_rds(for_viz, "data/selected_data_for_viz_com.rds")
