# Some useful libraries for the project

library(tidyverse)
library(lattice)
library(caret)
library(lubridate)


# Datasets reading

edx <- readRDS("C:/Users/roetti/Documents/MovieLens/edx.rds")
#edx <- readRDS("data/edx.rds")
validation <- readRDS("C:/Users/roetti/Documents/MovieLens/validation.rds")
#validation <- readRDS("data/validation.rds")


# Some preliminary and exploratory data analysis

head(edx)
nrow(edx)
n_distinct(edx$userId)
n_distinct(edx$movieId)
mean(edx$rating)


# Definition of the evaluation function: the Root of Mean Squared Errors

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))}


# Creation of train and test sets as partitions of edx
# (with test set required to not have any user or movie unknown in the train set)

set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1,
                                  p = 0.2, list = FALSE)

train_set <- edx[-test_index,]
test_set <- edx[test_index,]

test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")


# Definition of mu and first calculation fo the RMSE using it

mu <- mean(train_set$rating)

first_rmse <- RMSE(test_set$rating, mu)

rmse_results <- tibble(method = "Just the average", RMSE = first_rmse)
rmse_results %>% knitr::kable()


# Definition of b_i and their distribution

b_i <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()))

b_i %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))


# Second prediction using the b_i, calculation of the new RMSE

second_pred <- test_set %>% 
  left_join(b_i, by='movieId') %>%
  mutate(pred = mu + b_i) %>% .$pred

second_rmse <- RMSE(test_set$rating, second_pred)

rmse_results <- bind_rows(rmse_results,
                tibble(method="Movie Effect Model",  
                       RMSE = second_rmse))
rmse_results %>% knitr::kable()


# Histogram of the user ratings

train_set %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")


# Definition of b_u, third prediction using them, calculation of the new RMSE

b_u <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()))

third_pred <- test_set %>% 
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>% .$pred

third_rmse <- RMSE(test_set$rating, third_pred)

rmse_results <- bind_rows(rmse_results,
                tibble(method="Movie + User Effects Model",  
                       RMSE = third_rmse))
rmse_results %>% knitr::kable()


# Plot of the average ratings by genres

train_set %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Definition of b_g, fourth prediction using them, calculation of the new RMSE

b_g <- train_set %>% 
  left_join(b_i, by = "movieId") %>% 
  left_join(b_u, by = "userId") %>% 
  group_by(genres) %>% 
  summarise(b_g = sum(rating - b_i - b_u - mu)/(n()))

fourth_pred <- test_set %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  mutate(pred = mu + b_i + b_u + b_g) %>% .$pred

fourth_rmse <- RMSE(test_set$rating, fourth_pred)

rmse_results <- bind_rows(rmse_results,
                tibble(method = "Movie + User + Genres Effects Model",  
                       RMSE = fourth_rmse))
rmse_results %>% knitr::kable()


# Plot of the average weekly ratings over time

train_set %>% mutate(date = round_date(as_datetime(timestamp), unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()


# Definition of b_d, fifth prediction using them, calculation of the new RMSE

b_d <- train_set %>% 
  mutate(date=round_date(as_datetime(timestamp), unit = "week")) %>%
  left_join(b_i,by="movieId") %>% 
  left_join(b_u,by="userId") %>% 
  left_join(b_g,by="genres") %>%
  group_by(date) %>% 
  summarise(b_d=sum(rating - b_i - b_u - b_g - mu)/(n()))

fifth_pred <- test_set %>%
  mutate(date=round_date(as_datetime(timestamp), unit = "week")) %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  left_join(b_d, by = "date") %>%
  mutate(pred = mu + b_i + b_u + b_g + b_d) %>% .$pred

fifth_rmse <- RMSE(test_set$rating, fifth_pred)

rmse_results <- bind_rows(rmse_results,
                tibble(method = "Movie + User + Genres +Time Effects Model",  
                       RMSE = fifth_rmse))
rmse_results %>% knitr::kable()


# Model Regularization

lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>% 
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  b_g <- train_set %>% 
    left_join(b_i, by = "movieId") %>% 
    left_join(b_u, by = "userId") %>% 
    group_by(genres) %>% 
    summarise(b_g = sum(rating - b_i - b_u - mu)/(n()+l))
  b_d <- train_set %>% 
    mutate(date=round_date(as_datetime(timestamp), unit = "week")) %>%
    left_join(b_i,by="movieId") %>% 
    left_join(b_u,by="userId") %>% 
    left_join(b_g,by="genres") %>%
    group_by(date) %>% 
    summarise(b_d=sum(rating - b_i - b_u - b_g - mu)/(n()+l))
  pred <- test_set %>%
    mutate(date=round_date(as_datetime(timestamp), unit = "week")) %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    left_join(b_d, by = "date") %>%
    mutate(pred = mu + b_i + b_u + b_g + b_d) %>% .$pred
  return(RMSE(test_set$rating, pred))})

rmse_results <- bind_rows(rmse_results,
                tibble(method="Regularized Movie + User + Genres +Time Effects Model",  
                           RMSE = min(rmses)))
rmse_results %>% knitr::kable()


# Plot of the lambdas' performances and best lambda

qplot(lambdas, rmses)  
lambda <- lambdas[which.min(rmses)]
lambda


# Model training over the whole edx dataset

mu <- mean(edx$rating)
b_i <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))
b_u <- edx %>% 
  left_join(b_i, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))
b_g <- edx %>% 
  left_join(b_i, by = "movieId") %>% 
  left_join(b_u, by = "userId") %>% 
  group_by(genres) %>% 
  summarise(b_g = sum(rating - b_i - b_u - mu)/(n()+lambda))
b_d <- edx %>% 
  mutate(date=round_date(as_datetime(timestamp), unit = "week")) %>%
  left_join(b_i,by="movieId") %>% 
  left_join(b_u,by="userId") %>% 
  left_join(b_g,by="genres") %>%
  group_by(date) %>% 
  summarise(b_d=sum(rating - b_i - b_u - b_g - mu)/(n()+lambda))


# Model predictions on the validation dataset and final RMSE calculation

predicted_ratings <- validation %>%
  mutate(date=round_date(as_datetime(timestamp), unit = "week")) %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  left_join(b_d, by = "date") %>%
  mutate(pred = mu + b_i + b_u + b_g + b_d) %>%
  .$pred %>% replace_na(mu)

RMSE(validation$rating, predicted_ratings)

