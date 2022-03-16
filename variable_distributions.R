library(tidyverse)
library(corrplot)

data <- read_csv("data/census_team_2015_2019_dat_new.csv")
data <- data %>% mutate(
  `some degree (estimate)` = `associate's degree (estimate)` + `bachelor's degree (estimate)` + `master's degree (estimate)` + `professional school degree (estimate)` + `doctorate degree (estimate)`,
  `approximate total travel time (estimate)` = `90 or more minutes (estimate)` * 90 +
    `60 to 89 minutes (estimate)` * 74.5 +
    `45 to 59 minutes (estimate)` * 52 +
    `40 to 44 minutes (estimate)` * 42 +
    `35 to 39 minutes (estimate)` * 37 +
    `30 to 34 minutes (estimate)` * 32.5 +
    `25 to 29 minutes (estimate)` * 27 +
    `20 to 24 minutes (estimate)` * 22 +
    `15 to 19 minutes (estimate)` * 17 +
    `10 to 14 minutes (estimate)` * 12 +
    `5 to 9 minutes (estimate)` * 7 +
    `less than 5 minutes (estimate)` * 5,
  `approximate total age (estimate)` = `male: under 5 years (estimate)` * 5 + 
    `male: 5 to 17 years (estimate)` * 11 + 
    `male: 18 to 34 years (estimate)` * 26 + 
    `male: 35 to 64 years (estimate)` * 49.5 + 
    `male: 65 to 74 years (estimate)` * 69.5 + 
    `male: 75 years and over (estimate)` * 70 +
    `female: under 5 years (estimate)` * 5 + 
    `female: 5 to 17 years (estimate)` * 11 + 
    `female: 18 to 34 years (estimate)` * 26 + 
    `female: 35 to 64 years (estimate)` * 49.5 + 
    `female: 65 to 74 years (estimate)` * 69.5 + 
    `female: 75 years and over (estimate)` * 70,
  `total disability (esimate)` = `male: under 5 years: with a disability (estimate)`+
    `male: 5 to 17 years: with a disability (estimate)`+
    `male: 18 to 34 years: with a disability (estimate)`+
    `male: 35 to 64 years: with a disability (estimate)`+
    `male: 65 to 74 years: with a disability (estimate)`+
    `male: 75 years and over: with a disability (estimate)`+
    `female: under 5 years: with a disability (estimate)`+
    `female: 5 to 17 years: with a disability (estimate)`+
    `female: 18 to 34 years: with a disability (estimate)`+
    `female: 35 to 64 years: with a disability (estimate)`+
    `female: 65 to 74 years: with a disability (estimate)`+
    `female: 75 years and over: with a disability (estimate)`,
  `1.01 or more occupants per room (estimate)` = `owner occupied: 2.01 or more occupants per room (estimate)` +
    `renter occupied: 2.01 or more occupants per room (estimate)`+
    `renter occupied: 1.01 to 1.50 occupants per room (estimate)`+
    `renter occupied: 1.51 to 2.00 occupants per room (estimate)`+
    `owner occupied: 1.01 to 1.50 occupants per room (estimate)`+
    `owner occupied: 1.51 to 2.00 occupants per room (estimate)`,
  `prop some degree (estimate)` = `some degree (estimate)` / `education total (estimate)`,
  `prop white (estimate)` = `white alone (estimate)` / `race total (estimate)`,
  `average travel time (estimate)` = `approximate total travel time (estimate)` / `travel total (estimate)`,
  `average age (estimate)` = `approximate total age (estimate)` / (`male (estimate)` + `female (estimate)`),
  `prop disability (estimate)` = `total disability (esimate)` / (`male (estimate)` + `female (estimate)`),
  `prop public transportation (estimate)` = `public transportation (excluding taxicab) (estimate)` / `transportation total (estimate)`,
  `prop in labor force (estimate)` = `in labor force (estimate)` / `employment total (estimate)`,
  `prop owner occupied (estimate)` = `owner occupied (estimate)` / `occupants total (estimate)`,
  `prop 1.01 or more occupants per room (estimate)` = `1.01 or more occupants per room (estimate)` / `occupants total (estimate)`,
  
)
# note that `approximate total travel time (estimate)` and `approximate total age (estimate)` are super duper rough and probably shouldn't be used for anything substantial.

#proportion with some degree
data %>% ggplot(aes(x = `prop some degree (estimate)`)) +
  geom_histogram() +
  theme_minimal()

#proportion races
data %>% ggplot(aes(x = `prop white (estimate)`)) +
  geom_histogram() +
  theme_minimal() +
  xlim(0, 1)
  
data %>% ggplot(aes(x = `black or african american alone (estimate)` / `race total (estimate)`)) +
  geom_histogram() +
  theme_minimal() +
  xlim(0, 1)

data %>% ggplot(aes(x = `american indian and alaska native alone (estimate)` / `race total (estimate)`)) +
  geom_histogram() +
  theme_minimal()

data %>% ggplot(aes(x = `asian alone (estimate)` / `race total (estimate)`)) +
  geom_histogram() +
  theme_minimal() +
  xlim(0, 1)

data %>% ggplot(aes(x = `native hawaiian and other pacific islander alone (estimate)` / `race total (estimate)`)) +
  geom_histogram() +
  theme_minimal()

#travel time to work
data %>% ggplot(aes(x = `average travel time (estimate)`)) +
  geom_histogram() +
  theme_minimal()

#median household income
data %>% ggplot(aes(x = `median household income in the past 12 months (in 2019 inflation-adjusted dollars) (estimate)`)) +
  geom_histogram() +
  theme_minimal()

#age
data %>% ggplot(aes(x = `average age (estimate)`)) +
  geom_histogram() +
  theme_minimal()

#disability
data %>% ggplot(aes(x = `prop disability (estimate)`)) +
  geom_histogram() +
  theme_minimal()

#transportation
data %>% ggplot(aes(x = `prop public transportation (estimate)`)) +
  geom_histogram() +
  theme_minimal()

data %>% ggplot(aes(x = `car, truck, or van (estimate)` / `transportation total (estimate)`)) +
  geom_histogram() +
  theme_minimal()

#employment
data %>% ggplot(aes(x = `prop in labor force (estimate)`)) +
  geom_histogram() +
  theme_minimal()

#occupants
data %>% ggplot(aes(x = `prop owner occupied (estimate)`)) +
  geom_histogram() +
  theme_minimal()

data %>% ggplot(aes(x = `prop 1.01 or more occupants per room (estimate)`)) +
  geom_histogram() +
  theme_minimal()

#prop white vs approx average travel time
data %>% ggplot(aes(x = `white alone (estimate)` / `race total (estimate)`, y = `approximate total travel time (estimate)` / `travel total (estimate)`)) +
  geom_point() +
  theme_minimal()

#prop white vs median household income
data %>% ggplot(aes(x = `white alone (estimate)` / `race total (estimate)`, y = `median household income in the past 12 months (in 2019 inflation-adjusted dollars) (estimate)`)) +
  geom_point() +
  theme_minimal()

#prop white vs disability
data %>% ggplot(aes(x = `white alone (estimate)` / `race total (estimate)`, y = `total disability (esimate)` / (`male (estimate)` + `female (estimate)`))) +
  geom_point() +
  theme_minimal()

#prop white vs transportation
data %>% ggplot(aes(x = `white alone (estimate)` / `race total (estimate)`, y = `public transportation (excluding taxicab) (estimate)` / `transportation total (estimate)`)) +
  geom_point() +
  theme_minimal()

#prop white vs employment
data %>% ggplot(aes(x = `white alone (estimate)` / `race total (estimate)`, y = `in labor force (estimate)` / `employment total (estimate)`)) +
  geom_point() +
  theme_minimal()

#prop white vs occupancy
data %>% ggplot(aes(x = `white alone (estimate)` / `race total (estimate)`, y = `owner occupied (estimate)` / `occupants total (estimate)`)) +
  geom_point() +
  theme_minimal()

#Exploring correlations

corr_matrix <- data %>% dplyr::select(c(`prop some degree (estimate)`,
                `prop white (estimate)`,
                `average travel time (estimate)`,
                `average age (estimate)`,
                `prop disability (estimate)`,
                `prop public transportation (estimate)`,
                `prop in labor force (estimate)`,
                `prop owner occupied (estimate)`,
                `prop 1.01 or more occupants per room (estimate)`,
                `median household income in the past 12 months (in 2019 inflation-adjusted dollars) (estimate)`)) %>% 
  cor(use = "complete.obs")


acs_final <- read_rds("curated_data/final_demographic_data_with_geometry.rds")


for_cor <- acs_final %>%
  dplyr::select(non_white_perc, disabled_perc, single_parent_perc, not_very_well_eng_perc, no_internet_perc, unemp_perc,
                renting_hh_perc, less_hs_perc, no_vech_perc)

for_cor_1 <- acs_final %>%
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


corrplot(cor(for_cor))
corrplot(cor(for_cor_1))
corrplot(cor(for_fact_0))
corrplot(cor(for_fact))
