# Olga Loginova
# HarvardX: PH125.9x - Capstone Part 1
# MovieLens Project 

#### Introduction ####

################################################################
# Creating Edx and Validation Sets 
################################################################
# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Making sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Adding rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

################################################################
# Assessment Criteria
################################################################

# Function that computes RMSE for the validation data set
RMSE <- function(predicted_ratings){
  sqrt(mean((validation$rating - predicted_ratings)^2))
}

# Maximum allowed RMSE value
max_rmse <- 0.86490

#### Data Analysis and Models ####

################################################################
# Data Preprocessing
################################################################

# First six rows to explore column names and row data
head(edx)

# Basic summary
summary(edx)

# Number of unique users who gave ratings and number of unique movies rated 
# (to see that not every user rated every movie)
edx %>% 
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

# Ratings distribution
edx %>% 
  ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.5, color = "black") +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
  scale_y_continuous(breaks = c(seq(0, 3000000, 500000))) +
  ggtitle("Rating Distribution")

# Movie effect
edx %>% 
  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  xlab("Ratings") +
  ylab("Movies") +
  ggtitle("Some movies rated more than others")

# User effect
edx %>% 
  count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  xlab("Ratings") +
  ylab("Users") +
  ggtitle("Some users are more active at rating than others")

# Average rating per user
edx %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black") +  
  xlab("Average rating") +
  ylab("Users") +
  ggtitle("Average ratings by users") +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5))

# Cleaning up the data, removing unused columns
edx <- edx %>% select(-timestamp, -title, -genres)
head(edx)

################################################################
# Models
################################################################

## Naive Model ##
# The model is based on average rating across all movies and users

# Edx rating means
mu <- mean(edx$rating)
mu 

# Prediction using means only
naive_rmse <- RMSE(mu)
naive_rmse

# Checking naive_rmse against max_rmse
naive_rmse < max_rmse

## Movie Effect Model ##

# Computing means
movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

# Prediction on the validation set
predicted_ratings <- mu + validation %>%
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)

# Checking Movie Effect Model rmse against max_rmse
movie_rmse <- RMSE(predicted_ratings)
movie_rmse < max_rmse

## Movie-User Effect Model ##

# Computing means
user_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

# Predicting on the validation set
predicted_ratings <- mu + validation %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = b_i + b_u) %>%
  pull(pred)

# Checking Movie-User Effect Model rmse against max_rmse
movie_user_rmse <- RMSE(predicted_ratings)
movie_user_rmse < max_rmse

## Regularised Movie-User Model ##

# Choosing the penalty terms using cross-validation
# Defining tuning parameter
lambdas <- seq(0, 10, 0.25)

# Getting the rmses vector for different lambdas
rmses <- sapply(lambdas, function(l){
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n() + l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n() + l))
  
  predicted_ratings <- validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings))
})

# Ploting rmses vs lambdas (for illustrative purposes)                                                        
qplot(lambdas, rmses)  

# Choosing the best lambda with min rmse                                                          
lambda <- lambdas[which.min(rmses)]
lambda

# Computing regularised estimates of b_i with lambda
movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n() + lambda), n_i = n())

# Computing regularised estimates of b_u with lambda
user_avgs <- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n() + lambda), n_u = n())

# Predicting ratings using regularisation
predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(prediction = mu + b_i + b_u) %>% 
  pull(prediction)

# Checking Regularised Movie-User Effect Model rmse against max_rmse 
reg_movie_user_rmse <- RMSE(predicted_ratings)
reg_movie_user_rmse
reg_movie_user_rmse < max_rmse

################################################################
# Results
################################################################

# Resulting tibble including the assessment
method <- c("Just the Average", "Movie Effect Model", "Movie + User Effects Model", "Regularised Movie + User Effect Model")
rmse_results <- c(naive_rmse, movie_rmse, movie_user_rmse, reg_movie_user_rmse)
diffs <- c(rmse_results < max_rmse)
results <- data.table(Method = method, RMSE = format(round(rmse_results, 6), nsmall = 6), BelowMaxRMSE = diffs)
results