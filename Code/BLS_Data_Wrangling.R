#----------- setup
library(tidyverse)
library(rjson)
library(blsAPI)

setwd('E:/RA/Zang/DFM')

source('Code/Helper Functions.R')

raw_path <- 'Raw Data/'
output_path <- 'Output Data/'

# Construct State-Variable specific series ID
# see https://download.bls.gov/pub/time.series/la/la.area for detail
states <- read_csv(paste0(raw_path, 'Birth_10_20_by_month.csv')) %>% 
  mutate(ids = ifelse(`State Code` < 10, paste0('0', `State Code`), 
                       as.character(`State Code`))) %>% 
  select(State, ids) %>% unique() 

state_mapping <- list(
  id = states$ids,
  state = states$State
)

# all seasonal adjusted
# 3: unemployment rate
# 8: labor force participation rate

unemp_rate_id <- map_chr(.x=state_mapping$id, 
                         ~make_series_id(.x, series_identifier=3,
                                             series_type = 'LASST'))
labor_rate_id <- map_chr(.x=state_mapping$id, 
                         ~make_series_id(.x, series_identifier=8,
                                             series_type = 'LASST'))
avg_earn_id <- map_chr(.x=state_mapping$id, 
                         ~paste0('SMU', .x, '000000500000011'))

# set up API query
# API has a 50-series limit while we have 51 states
key <- '09423c12567c47b08b714210ebfd2efc'
start <- 2010
end <- 2021

query_unemp1 <- list(
  'seriesid'=unemp_rate_id[1:25],
  'startyear'=start,
  'endyear'=end,
  'registrationKey'=key)

query_unemp2 <- list(
  'seriesid'=unemp_rate_id[26:51],
  'startyear'=start,
  'endyear'=end,
  'registrationKey'=key)

query_unemp <- list(query_unemp1, query_unemp2)

query_labor1 <- list(
  'seriesid'=labor_rate_id[1:25],
  'startyear'=start,
  'endyear'=end,
  'registrationKey'=key)

query_labor2 <- list(
  'seriesid'=labor_rate_id[26:51],
  'startyear'=start,
  'endyear'=end,
  'registrationKey'=key)

query_labor <- list(query_labor1, query_labor2)

query_avg_earn1 <- list(
  'seriesid'=avg_earn_id[1:25],
  'startyear'=start,
  'endyear'=end,
  'registrationKey'=key)

query_avg_earn2 <- list(
  'seriesid'=avg_earn_id[26:51],
  'startyear'=start,
  'endyear'=end,
  'registrationKey'=key)

query_avg_earn <- list(query_avg_earn1, query_avg_earn2)

# query data 

df_unemp <- map_dfr(.x=query_unemp, 
                    ~load_process_data(query = .x, 
                                       state_mapping))
df_labor <- map_dfr(.x=query_labor, 
                    ~load_process_data(query = .x, 
                                       state_mapping))
df_avg_earn <- map_dfr(.x=query_avg_earn, 
                    ~load_process_data(query = .x, 
                                       state_mapping))

## Process results

df_unemp_clean <- clean_df(df_unemp)
write_csv(df_unemp_clean, 
          file = paste0(output_path, 'Unemploy_10_21_by_Month.csv'))

df_labor_clean <- clean_df(df_labor)
write_csv(df_labor_clean, 
          file = paste0(output_path, 'Labor_Par_10_21_by_Month.csv'))

df_avg_earn_clean <- clean_df(df_avg_earn)
write_csv(df_avg_earn_clean, 
          file = paste0(output_path, 'Avg_Earn_10_21_by_Month.csv'))
