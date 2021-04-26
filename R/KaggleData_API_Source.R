
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

# Data Analysis

train_data %>% str()

train_data %>% select(Pclass) %>% table()

## Split train_data into train and test data

library(caTools)
library(rpart)

set.seed(123)

train_dat <- train_data %>% sample_frac(.75)
test_dat <- anti_join(train_data, train_dat, by = 'PassengerId')

fit <- rpart(Survived~., data = train_dat %>% select(-Name, -Ticket, -Cabin), method = "class")

predicted <- predict(fit, test_dat, type = "class")

table_results <- table(test_dat$Survived, predicted)

prediction_results <- predict(fit, test_data, type = "class")

test_data$Survived <- prediction_results

# test_data %>%
#   select(PassengerId, Survived) %>%
#   write.csv("submission.csv", row.names = FALSE)




# Gender

train_data %>% 
  select(Sex, Survived) %>% 
  table() %>% 
  prop.table(1)

train_data %>% 
  select(Age) %>% 
  ggplot(aes(x = Age)) +
  geom_histogram()

train_data_V2 <- train_data %>% 
  mutate(Child = Age < 18)

