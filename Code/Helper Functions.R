# list of helper functions in data wrangling

# setup
library(tidyverse)
library(rjson)
library(blsAPI)

# generate BLS series id 
make_series_id <- function(state_code, series_identifier, 
                           series_type){
  series_id <- paste0(series_type, state_code, '000000000000',
                      series_identifier)
  return(series_id)
}


# helper function to convert json to dataframe
apiDF <- function(data, state, state_code){
  df <- data.frame(year=character(),
                   period=character(),
                   periodName=character(),
                   value=character(),
                   state=character(),
                   state_code=character(),
                   stringsAsFactors=FALSE)
  
  
  i <- 0
  for(d in data){
    i <- i + 1
    results <- unlist(d)[c('year', 'period', 'periodName', 'value')]
    var_names <- names(results)
    result_vec <- c(results, state, state_code)
    if (length(result_vec) < 6){
      print(result_vec)
    }
    names(result_vec) <- c(var_names, "state", "state_code") 
    df[i, ] <- result_vec 
  }
  return(df)
}

# function to perform the whole querying and processing step
load_process_data <- function(query, state_mapping){
  # query: list of query parameter
  # state_mapping: list of state-fips code mapping
  response <- blsAPI(query, api_version = 2)
  json <- fromJSON(response)
  
  result.df <- data.frame(year=character(),
                   period=character(),
                   periodName=character(),
                   value=character(),
                   state=character(),
                   state_code=character(),
                   stringsAsFactors=FALSE)
  
  for (i in 1:length(query$seriesid)){
    id <- query$seriesid[i]
    state_code <- str_extract(id, '[^A-Z][0-9]')
    idx <- which(state_mapping$id == state_code)
    
    state <- state_mapping$state[idx]
    temp_df <- apiDF(json$Results$series[[i]]$data, state, state_code)
    result.df <- bind_rows(result.df, temp_df)
  }
  return(result.df)
}

# clean datasets
clean_df <- function(df){
  df_clean <- df %>% 
    mutate(year = as.integer(year),
           value = as.numeric(value), 
           state_code = as.integer(state_code)) %>% 
    arrange(state_code, year, period)
  return(df_clean)
}