
# Script used to source the data for Kaggle Competitions using API
# Packages used:

library(tidyverse)
library(httr)
library(jsonlite)

# Read API Key via JSON file

apikey <- rjson::fromJSON(file = "kaggle.json")

apibaseurl <- "https://www.kaggle.com/"

# Examine the competition list datasets (page 1)

path <- "api/v1/competitions/list"

kaggle_get <- GET(url = apibaseurl, path = path, authenticate(apikey$username, apikey$key))

kaggle_get$status_code

kaggle_competition_list <- kaggle_get$content %>% 
  rawToChar() %>% 
  fromJSON() %>% 
  as_tibble()

# Titanic

# Find data files attached to Titanic Competition (5th dataset)

data_path <- paste0("api/v1/competitions/data/list/", kaggle_competition_list$id[5])

data_get <- GET(url = apibaseurl, path = data_path, authenticate(apikey$username, apikey$key))

data_get$status_code

titanic_datafiles <- data_get$content %>% rawToChar() %>% fromJSON()

# Download data files

titanic_data <- list()

for (i in 1:nrow(titanic_datafiles)){
  
  path <- paste0("api/v1/competitions/data/download/", 
                 kaggle_competition_list$id[5], "/", titanic_datafiles$name[i])
  
  getresult <- GET(url = apibaseurl, path = path, authenticate(apikey$username, apikey$key))
  
  titanic_data[i] <- getresult$content %>%
    rawToChar() %>% read_csv() %>% list()
}

gender_data <- titanic_data[[1]]
train_data <- titanic_data[[2]]
test_data <- titanic_data[[3]]

