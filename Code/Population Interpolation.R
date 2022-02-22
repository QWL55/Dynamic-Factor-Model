#----------- setup
library(tidyverse)
library(lubridate)
library(ggthemes)

setwd('E:/RA/Zang/DFM')

raw_path <- 'Raw Data/'
output_path <- 'Output Data/'

# load input data
birth <- read_csv(paste0(raw_path, 'Birth_10_20_by_month.csv'))
fertility <- read_csv(paste0(raw_path, 'Fertility_10_20_by_year.csv')) 
birth_21 <- read_csv(paste0(raw_path, 'Birth_21.csv'))

# interpolation function 
interpolate <- function(y, x, xout){
  ylong <- spline(y = y, x = x, xout = xout, method = 'natural')$y
  return(ylong)
}


# prepare idx in the population data
fertility <- 
  fertility %>% 
  select(State, Year, `Female Population`) %>% 
  group_by(State) %>% 
  mutate(idx = seq(6, 11*12, by = 12))

# merge 10-20 and 2021 data, interpolate/extrapolate data
fer_10_21 <- 
  birth %>% 
  rbind(birth_21) %>% 
  group_by(State) %>% 
  mutate(idx = 1:length(State)) %>% 
  left_join(fertility, by = c('State', 'Year', 'idx')) %>% 
  group_by(State) %>% 
  mutate(fem_pop_pred = interpolate(unique(na.omit(`Female Population`)),
                                    unique(fertility$idx), 
                                    xout=1:length(State)),
         fem_pop_pred = round(fem_pop_pred, 0),
         time = make_date(Year, `Month Code`),
         Fertility = 1000 * Births / fem_pop_pred)
# export to csv
write_csv(fer_10_21, 
          file = paste0(output_path, 'Pred_Fertility_10_21_by_month.csv'))
# visualize results for california
fer_10_21 %>% 
  filter(State == 'California') %>%
  ggplot(aes(time, fem_pop_pred)) +
  geom_point() +
  geom_point(aes(time, `Female Population`), color='red') + 
  labs(x = 'Time', y = 'Female Population (15 - 44)', 
       title = 'Predicted Female Population Data in California',
       subtitle= 'Red dots denote the observed data') + 
  theme_stata()
ggsave("Output Data/Fem_Pop_CA.png", width = 8, height = 8, units = "in")


# Interpolation for mortality Data
# mortality 
## calculate monthly mortality rate (per 100,000) using yearly population
death <- read_csv(paste0(raw_path, 'Death_10_20_by_month.csv'))
mortality <- read_csv(paste0(raw_path, 'Mortality_10_20_by_year.csv')) 

# prepare idx
mortality <- 
  mortality %>% 
  select(State, Year, Gender, Population) %>% 
  pivot_wider(names_from = 'Gender', names_prefix = 'Pop_', 
              values_from = 'Population') %>% 
  group_by(State) %>% 
  mutate(idx = seq(6, 11*12, by = 12))

# pivot data and perform interpolation
mor_10_20 <- 
  death %>% 
  pivot_wider(names_from = 'Gender', names_prefix = 'Deaths_', 
              values_from = 'Deaths') %>% 
  group_by(State) %>% 
  mutate(idx = 1:length(State),
         `Month Code` = as.integer(str_sub(`Month Code`, 
                                           start = 6))) %>% 
  left_join(mortality, by = c('State', 'Year', 'idx')) %>% 
  group_by(State) %>% 
  mutate(fem_pop_pred = interpolate(unique(na.omit(Pop_Female)),
                                    unique(mortality$idx), 
                                    xout=1:length(State)),
         fem_pop_pred = as.integer(fem_pop_pred),
         male_pop_pred = interpolate(unique(na.omit(Pop_Male)),
                                     unique(mortality$idx), 
                                     xout=1:length(State)),
         male_pop_pred = as.integer(male_pop_pred),
         mor_female = 1e5 * Deaths_Female / fem_pop_pred, 
         mor_male = 1e5 * Deaths_Male / male_pop_pred,
         time = make_date(Year, `Month Code`))
# visualization
mor_10_20 %>% 
  filter(State == 'Alabama') %>%
  ggplot(aes(time, fem_pop_pred)) +
  geom_point() +
  geom_point(aes(time, `Pop_Female`), color='red') + 
  labs(x = 'Time', y = 'Female Population', 
       title = 'Predicted Female Population Data in Alabama',
       subtitle= 'Red dots denote the observed data') + 
  theme_stata()
ggsave("Output Data/Fem_Pop_AL.png", width = 8, height = 8, units = "in")

mor_10_20 %>% 
  filter(State == 'Alabama') %>%
  ggplot(aes(time, male_pop_pred)) +
  geom_point() +
  geom_point(aes(time, `Pop_Male`), color='red') + 
  labs(x = 'Time', y = 'Male Population', 
       title = 'Predicted Male Population Data in Alabama',
       subtitle= 'Red dots denote the observed data') + 
  theme_stata()
ggsave("Output Data/Male_Pop_AL.png", width = 8, height = 8, units = "in")

write_csv(mor_10_20, 
          file = paste0(output_path, 'Pred_Mortality_10_20_by_Month.csv'))

