#----------- setup
library(tidyverse)

setwd('E:/RA/Zang/DFM')

raw_path <- 'Raw Data/'
output_path <- 'Output Data/'

#----------- cleaning

# fertility
## calculate monthly fertility rate (per 1000 women) using yearly population
birth <- read_csv(paste0(raw_path, 'Birth_10_20_by_month.csv'))
fertility <- read_csv(paste0(raw_path, 'Fertility_10_20_by_year.csv')) 

fertility <- 
  fertility %>% 
  select(State, Year, `Female Population`)

fer_10_20 <- 
  birth %>% 
  left_join(fertility, by = c('State', 'Year')) %>% 
  mutate(Fertility = 1000 * Births / `Female Population`)

## prepare 2021 provisional data
birth_21 <- read_csv(paste0(raw_path, 'Birth_21.csv'))
## use 2020 population data for 2021
fem_pop_21 <- 
  fertility %>% 
  filter(State %in% birth_21$State, Year == 2020) %>% 
  select(!Year) 

fer_21 <- 
  birth_21 %>% 
  left_join(fem_pop_21, by = 'State') %>% 
  mutate(Fertility = 1000 * Births / `Female Population`)

fer_10_21 <- rbind(fer_10_20, fer_21) %>% 
              arrange(`State Code`)

write_csv(fer_10_21, 
          file = paste0(output_path, 'Fertility_10_21_by_month.csv'))

# mortality 
## calculate monthly mortality rate (per 100,000) using yearly population
death <- read_csv(paste0(raw_path, 'Death_10_20_by_month.csv'))
mortality <- read_csv(paste0(raw_path, 'Mortality_10_20_by_year.csv')) 

mortality <- 
  mortality %>% 
  select(State, Year, Gender, Population) %>% 
  pivot_wider(names_from = 'Gender', names_prefix = 'Pop_', 
              values_from = 'Population')

mor_10_20 <- 
  death %>% 
  pivot_wider(names_from = 'Gender', names_prefix = 'Deaths_', 
              values_from = 'Deaths') %>% 
  left_join(mortality, by = c('State', 'Year')) %>% 
  mutate(mor_female = 1e5 * Deaths_Female / Pop_Female, 
         mor_male = 1e5 * Deaths_Male / Pop_Male)

write_csv(mor_10_20, 
          file = paste0(output_path, 'Mortality_10_20_by_Month.csv'))

# GDP
## GDP by state: All industry total (Millions of current dollars) 
gdp <- read_csv(paste0(raw_path, 'GDP_10_21.csv'), skip = 4)

gdp_long <- 
  gdp %>% 
  slice(2:52) %>% 
  pivot_longer(cols = -c('GeoFips', 'GeoName'), names_to = 'Time', 
               values_to = 'GDP') %>% 
  separate('Time', into = c('Year', 'Quarter')) %>% 
  mutate(GeoFips = as.integer(str_remove(GeoFips, '000')),
         Quarter = str_remove(Quarter, 'Q')) %>% 
  rename(`State Code` = GeoFips, State = GeoName)

write_csv(gdp_long, 
          file = paste0(output_path, 'GDP_10_21_by_Quarter.csv'))
