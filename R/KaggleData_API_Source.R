
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

# Natural Language Processing for Disaster Tweets

nlp_test <- read_csv("R/NLP/test.csv")
nlp_train <- read_csv("R/NLP/train.csv")

# Find out how many words in each text

library(tidytext)
library(randomForest)
library(corrr)


data(stop_words)

library("syuzhet")

nlp_train_split <- nlp_train %>% 
  unnest_tokens(word, text) %>% 
  # anti_join(stop_words) %>% 
  mutate(word2 = str_extract(word, "[a-z']+"))

nlp_train_sentiment <- nlp_train_split %>% 
  mutate(sentiment = get_sentiment(word, method="syuzhet")) %>% 
  group_by(id) %>% 
  summarise(sentiment = mean(sentiment))

nlp_train_freq <- nlp_train_split %>% 
  mutate(word2 = str_extract(word, "[a-z']+")) %>%
  group_by(word2) %>% 
  summarise(freq = n())

nlp_train_wordlength <- nlp_train_split %>% 
  mutate(wordlength = nchar(word)) %>% 
  group_by(id) %>% 
  summarise(meanwl = mean(wordlength),
            minwl = min(wordlength),
            maxwml = max(wordlength))


nlp_train_fe <- nlp_train_split %>% 
  left_join(nlp_train_freq, by = "word2") %>%
  group_by(id, target) %>% 
  summarise(nwords = n(),
            freqavg = mean(freq)) %>% 
  left_join(nlp_train_wordlength, by = "id") %>% 
  left_join(nlp_train_sentiment, by = "id") %>%
  ungroup() %>% 
  mutate(target = as.factor(target))
  
set.seed(123)

nlp_train_fe1 <- nlp_train_fe %>% ungroup() %>% slice_sample(n = round(.75*nrow(nlp_train_fe)))
nlp_train_fe2 <- anti_join(nlp_train_fe, nlp_train_fe1, by = 'id')


nlp_rf <- randomForest(target ~ ., data=nlp_train_fe1 %>% select(-id), 
                       ntree=100, mtry=2, importance=TRUE)

varImpPlot(nlp_rf)

nlp_rf_predict <- predict(nlp_rf,nlp_train_fe2)


table(nlp_rf_predict,nlp_train_fe2$target)

nlp_test_split <- nlp_test %>% 
  unnest_tokens(word, text) %>% 
  mutate(word2 = str_extract(word, "[a-z']+"))

nlp_test_sentiment <- nlp_test_split %>% 
  mutate(sentiment = get_sentiment(word, method="syuzhet")) %>% 
  group_by(id) %>% 
  summarise(sentiment = mean(sentiment))

nlp_test_freq <- nlp_test_split %>% 
  mutate(word2 = str_extract(word, "[a-z']+")) %>%
  group_by(word2) %>% 
  summarise(freq = n())

nlp_test_wordlength <- nlp_test_split %>% 
  mutate(wordlength = nchar(word)) %>% 
  group_by(id) %>% 
  summarise(meanwl = mean(wordlength),
            minwl = min(wordlength),
            maxwml = max(wordlength))

nlp_test_fe <- nlp_test_split %>% 
  left_join(nlp_test_freq, by = "word2") %>%
  group_by(id) %>% 
  summarise(nwords = n(),
            freqavg = mean(freq)) %>% 
  left_join(nlp_test_wordlength, by = "id") %>% 
  left_join(nlp_test_sentiment, by = "id") %>%
  ungroup()

nlp_rf_predicttest <- predict(nlp_rf,nlp_test_fe)

nlp_test$target <- nlp_rf_predicttest

nlp_test %>% 
  select(id, target) %>% 
  write.csv("R/NLP/submission.csv", row.names = FALSE)

