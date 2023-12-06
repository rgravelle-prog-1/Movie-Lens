#
# 
#  MovieLens Capstone Project 
#    Robert Gravelle
#    Filename :    MovieLens-final
#
#     HarvardX Data Science Capstone
#   Professional Certificate PH125.9x
# 
#
#
#
#      DataScience Capstone
#       Harvard X via EdX
#


if(!require(ggplot2)) install.packages("ggplot2")
if(!require(devtools)) install.packages("devtools")
if(!require(benchmarkme)) install.packages("benchmarkme")
if(!require(rmarkdown)) install.packages("rmarkdown")
if(!require(lubridate)) install.packages("lubridate")
if(!require(scales)) install.packages("scales")
if(!require(parallel)) install.packages("parallel")
if(!require(stringr)) install.packages("stringr")
if(!require(kableExtra)) install.packages("kableExtra")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(caret)) install.packages("caret")

library(ggplot2)
library(benchmarkme)
library(rmarkdown)
library(lubridate)
library(scales)
library(parallel)
library(stringr)
library(kableExtra) 
library(tidyverse) 
library(caret)

# the next section is copied from edx:
##########################################################
# Create edx and final_holdout_test sets 
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


## Data Inspection and preprocessing

# CHeck to see is there is NAs in dataset
anyNA(edx)

edx[apply(is.na(edx), 1, any), ]

# Data inspections 
unique_genres <- unique(unlist(strsplit(edx$genres, "\\|")))
unique_genres

# inspect data and remove any of No genres
subset(edx, genres=="(no genres listed)")
edx$genres <- ifelse(edx$genres == "(no genres listed)", "Short", edx$genres)

# Data Extractions for year, release year
# Extract first few listed genres per movie in separate columns
edx$main_genre  <- sapply(strsplit(edx$genres, "\\|"), function(x) x[1])
edx$side1_genre <- sapply(strsplit(edx$genres, "\\|"), function(x) x[2])
edx$side2_genre <- sapply(strsplit(edx$genres, "\\|"), function(x) x[3])

# rated and release  year
edx <- edx %>%
  mutate(date=as_datetime(timestamp), yearrated=year(date))  
head(edx)

edx <- edx %>%
  mutate(releaseyear=as.numeric(str_extract(str_extract(title, "[/(]\\d{4}[/)]$"), regex("\\d{4}"))))
head(edx)

# Ratings
edx %>% 
  group_by(rating) %>% 
  summarise(perc = n()/nrow(edx)) %>% 
  ggplot(aes(rating, perc)) + 
  geom_col() + 
  theme_light() +
  labs(x="Rating", y="%", title = "Rating distribution overall")
# The majority of users tend to rate movies within the range of 3.5 to 4.2, surpassing the average ratings observed in the overall movie rating distribution

# Genres distibution
edx %>% 
  group_by(main_genre) %>%
  summarise(perc = n()/nrow(edx)) %>% 
  ggplot(aes(reorder(main_genre, -perc), perc)) +
  geom_col() +
  theme_light() +
  theme(axis.text.x = element_text(angle=90)) +
  labs(x="Genre", y="% of movies", title = "Distribution of genres")
# Most genres listed are Action, Comedy and Drama

# add side genres into account
edx %>%
  select(main_genre, side1_genre, side2_genre) %>%
  pivot_longer(cols=c(main_genre, side1_genre, side2_genre), values_to = "genre", values_drop_na = TRUE) %>%
  group_by(genre) %>%
  summarise(perc = n()/nrow(edx)) %>% 
  ggplot(aes(reorder(genre, -perc), perc)) +
  geom_col() +
  theme_light() +
  theme(axis.text.x = element_text(angle=90)) +
  labs(x="Genre", y="% of movies", title = "Distribution of genres (including subgenres)")


# Ratings per genre
edx %>% 
  group_by(main_genre) %>%
  summarise(avg_genre_rating = mean(rating)) %>%
  arrange(desc(avg_genre_rating)) %>%
  ggplot(aes(reorder(main_genre, -avg_genre_rating), avg_genre_rating)) +
  geom_col() +
  theme_light() +
  theme(axis.text.x = element_text(angle=90)) +
  labs(x="Genre", y="Average Rating", title = "Rating distribution overall")

# Genre ratings reveal a preference for "intellectual" movie genres, such as Film-Noir, Crime, and Drama, which consistently garner higher ratings compared to genres associated with entertainment, like Action, Fantasy, and Horror. This trend holds true when evaluating the average ratings of genre combinations, specifically those with more than 50,000 ratings.

edx %>% 
  group_by(genres) %>%
  summarize(n=n(), avg=mean(rating)) %>%
  filter(n >= 50000) %>% 
  mutate(genres = reorder(genres, -avg)) %>%
  ggplot(aes(x=genres, y=avg)) + 
  geom_col() +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x="Genre combinations", y="Avg rating", title="Avg rating of genres with combinations")


# Average rating distribution 
edx %>% 
  group_by(movieId) %>%
  summarise(avg_rating = sum(rating)/n()) %>%
  ggplot(aes(avg_rating)) +
  geom_histogram(bins=50) + 
  theme_light() +
  labs(x="Avg Rating", y="Movies", title = "Number of ratings per movie")
# Only a scant number of movies achieve an average rating surpassing approximately 4.2. The majority of films fall within the range of 2.5 to 4 in terms of average ratings.

# Average rating distribution of users
edx %>% 
  group_by(userId) %>%
  summarise(avg_rating = sum(rating)/n()) %>%
  ggplot(aes(avg_rating)) +
  geom_histogram(bins=50) + 
  theme_light() +
  labs(x="Avg Rating", y="Users", title = "Avg ratings of users")
# The majority of users tend to rate movies within the range of 3.5 to 4.2, surpassing the average ratings observed in the overall movie rating distribution.

# Number of ratings per user
edx %>% 
  count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram( bins=50) +
  scale_x_log10() +
  #scale_y_log10() +  # log10 on number of ratings axis provides not a better readable graph
  theme_light() +
  labs(x = "Users", y = "Number of ratings")
# Most users rate between a few and about 100 movies. 

# Average rating per release year with trendline
edx %>% 
  group_by(releaseyear) %>%
  summarise(rating = mean(rating)) %>%
  ggplot(aes(releaseyear, rating)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x,6)) +
  theme_light() +
  labs(x="Release year", y="Avg rating", title="Avg rating per release year")
# Movies released before 1970 are generally rated higher than movies released in the 
# last two decades. This is a know effect (only good stuff survive the test of time).

# Number of released movies per year
edx %>%
  group_by(releaseyear) %>%
  summarise(n=n()) %>%
  ggplot(aes(releaseyear, n)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x,6)) +
  theme_light() +
  labs(x="Release Year", y="NOF Movies", title="Nr of movies released per year")
# Movie releases were relative stable before the 1970, then picked up and almost
# exploded 1990 and the following years, probably due to advancements in technology,
# production and market (e.g. movie theaters, movie rentals, tv...). Since before 
# 2000 the number of movies released collapsed as quickly as its explosion a decade 
# earlier.

# Number of ratings per year. Only include the years from 1996 to 2008, data before
# and after is 0.
edx %>%
  group_by(yearrated) %>%
  summarise(n=n()) %>%
  ggplot(aes(yearrated, n)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x,8)) +
  theme_light() +
  xlim(1996, 2008) +
  labs(x="Year rated", y="NOF Movies", title="Nr of movies rated per year")
# Movie ratings per year are stable with some outliers.

# 5 year blocks of movie average rating 
edx_5yr <- edx %>%
  mutate(five_year = floor((releaseyear - 1) / 5) * 5 + 1) %>%
  group_by(main_genre, five_year) %>%
  summarize(mean_rating = mean(rating, na.rm = TRUE)) %>%
  as.data.frame()

# include only some selected genres
edx_5yr_genre <- filter(edx_5yr, main_genre %in% c("Fantasy", "Action", "Adventure", "Drama", "Horror", "Thriller"))

# plot the average rating of a genre with an overlayed fitting curve
ggplot(edx_5yr_genre, aes(x = five_year, y = mean_rating, color = main_genre)) +
  geom_line() +
  geom_smooth(method = "lm", formula = y ~ poly(x,8), se = FALSE, size = 1.5) +
  scale_x_continuous(limits = c(1915, 2008), breaks = seq(1915, 2008, 5)) +
  labs(x = "Year", y = "Average Rating", color = "Genre")
# some genres have pretty consistent average ratings over the years, others like e.g. 
# Horror or Fantasy fluctuate a lot more.

# Machine Learning 

# preparations
set.seed(35)

# split data into training and test set
test_index <- createDataPartition(y = edx$rating, times=1, p=0.2, list=FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]

# Table of model results (RMSE)
ml_results <- tibble()


test_set_final <- test_set %>%
  as_tibble() %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId") %>%
  semi_join(train_set, by = "main_genre")

# Add the removed rows from the test_set to the train_set
removed <- anti_join(
  as_tibble(test_set),
  test_set_final,
  by = names(test_set_final)
)
train_set <- rbind(train_set, removed)
test_set <- test_set_final

# cleanup
rm(test_set_final, test_index, removed)

# Blind Guess
guess_model <- sample(c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5), length(test_set$rating), replace=TRUE)

# calc the RMSE of the guessed ratings on the test set
rmse_guessing <- RMSE(test_set$rating, guess_model)

ml_results <- ml_results %>%
  bind_rows(tibble(Model="Guessing", RMSE=rmse_guessing))

# show the resulting RMSE table
ml_results

# Resulting RMSE is about 2.156. Pretty far of the 0.865 we are after.
# cleanup
rm(guess_model)

# mean
#traingin set average 
avg_model <- mean(train_set$rating)

# test set RMSE
rmse_avg_model <- RMSE(test_set$rating, avg_model)

# add result
ml_results <- ml_results %>% 
  bind_rows(tibble(Model="Avg Model", RMSE=rmse_avg_model))

# show RMSE table
ml_results

# The average of every movie used for predicting a rating results in 1.059841. 

rm(avg_model)

# Genres and Mean

movie_avg <- mean(train_set$rating)

# bias of genres. 
genre_bias <- train_set %>%
  group_by(main_genre) %>%
  summarise(deviation_genre = mean(rating - movie_avg))

# combine genre bias with the test_set
mean_genre_model <- test_set %>%
  inner_join(genre_bias, by="main_genre")

# predict the rating, genre bias of the movie in the test_set + average
mean_genre_model$predicted_rating <- movie_avg + mean_genre_model$deviation_genre

# calculate RMSE (on test_set)
rmse_mean_genre_model <- RMSE(test_set$rating, mean_genre_model$predicted_rating)

# add result to table
ml_results <- ml_results %>% 
  bind_rows(tibble(Model="Genre Model", RMSE=rmse_mean_genre_model))

# show the resulting RMSE table
ml_results

# The RMSE is a bit better (1.04824) than just take the average like before.
# cleanup
rm(mean_genre_model)

# Movie and Mean 
movie_bias <- train_set %>%
  group_by(movieId) %>%
  summarise(deviation_movie = mean(rating - movie_avg))

# on the test set add the movie avg (3.512) with the difference the movie had 
# to the avg in the training set and pull that column as a vector
mean_movie_model <- test_set %>%
  inner_join(movie_bias, by="movieId")

# predict the rating, based the movie (by movieId) on the deviation + average 
mean_movie_model$predicted_rating <- mean_movie_model$deviation_movie + movie_avg

# RMSE (on test_set)
rmse_mean_movie_model <- RMSE(test_set$rating, mean_movie_model$predicted_rating)

# add result to table
ml_results <- ml_results %>% 
  bind_rows(tibble(Model="Movie Model", RMSE=rmse_mean_movie_model))

# show the resulting RMSE table
ml_results
rm(mean_movie_model)

# User mean
# user_bias is the difference of the avg users rating to the mean rating
user_bias <- train_set %>%
  group_by(userId) %>%
  summarise(deviation_user = mean(rating - movie_avg))

# on the test set add the movie avg (3.512) with the difference the movie had 
# to the avg in the training set and pull that column as a vector
mean_user_model <- test_set %>%
  inner_join(user_bias, by="userId")

# predict the rating, based the movie (by movieId) on the deviation + average 
mean_user_model$predicted_rating <- mean_user_model$deviation_user + movie_avg

# RMSE (on test_set)
rmse_mean_user_model <- RMSE(test_set$rating, mean_user_model$predicted_rating)

# add result to table
ml_results <- ml_results %>% 
  bind_rows(tibble(Model="User Model", RMSE=rmse_mean_user_model))

# show the resulting RMSE table
ml_results

# With and RMSE of 0.978 
# cleanup
rm(mean_user_model)

# Release year and Mean 
releaseyear_bias <- train_set %>%
  group_by(releaseyear) %>%
  summarise(deviation_releaseyear = mean(rating - movie_avg))

#vector the difference of the movie average and TS set 
mean_releaseyear_model <- test_set %>%
  inner_join(releaseyear_bias, by="releaseyear")

# predict the rating
mean_releaseyear_model$predicted_rating <- mean_releaseyear_model$deviation_releaseyear + movie_avg

# New RMSE test set
rmse_mean_releaseyear_model <- RMSE(test_set$rating, mean_releaseyear_model$predicted_rating)

# add result to table
ml_results <- ml_results %>% 
  bind_rows(tibble(Model="Release Year Model", RMSE=rmse_mean_releaseyear_model))

# show RMSE table
ml_results
rm(mean_releaseyear_model)

# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Function to calculate RMSE
calculate_rmse <- function(predictions, actual) {
  return(RMSE(actual, predictions))
}

# Tune genre bias with a sequence of parameters
tuning_param <- seq(0, 1, 0.1)
rmse_seq <- sapply(tuning_param, function(t) {
  avg <- mean(train_set$rating)
  
  # Calculate genre bias
  genre_bias <- train_set %>%
    group_by(main_genre) %>%
    summarise(deviation_genre = t * sum(rating - movie_avg)/n())
  
  # Calculate movie bias
  movie_bias <- train_set %>%
    group_by(movieId) %>%
    summarise(deviation_movie = 0.8944 * sum(rating - movie_avg)/n())
  
  # Calculate user bias
  user_bias <- train_set %>%
    group_by(userId) %>%
    summarise(deviation_user = 0.798 * sum(rating - movie_avg)/n())
  
  # Join data frames and make predictions
  model <- test_set %>%
    inner_join(genre_bias, by="main_genre") %>%
    inner_join(movie_bias, by="movieId") %>%
    inner_join(user_bias, by="userId")
  
  model$predicted_rating <- model$deviation_genre + 
    model$deviation_user + 
    model$deviation_movie + 
    movie_avg
  
  # Return RMSE
  return(calculate_rmse(model$predicted_rating, test_set$rating))
})



########### MEAN + MOVIE + GENRE + USER ##############
# combine user, movie and genre together
mean_movie_user_genre_model <- test_set %>%
  inner_join(movie_bias, by="movieId") %>%
  inner_join(user_bias, by="userId") %>%
  inner_join(genre_bias, by="main_genre")

# make prediction on test set with the movie/user/genre model
mean_movie_user_genre_model$predicted_rating <- mean_movie_user_genre_model$deviation_user + 
  mean_movie_user_genre_model$deviation_movie + 
  mean_movie_user_genre_model$deviation_genre + 
  movie_avg

# RMSE (on test_set)
rmse_mean_movie_user_genre_model <- RMSE(test_set$rating, mean_movie_user_genre_model$predicted_rating)

# add result to table
ml_results <- ml_results %>% 
  bind_rows(tibble(Model="Movie + User + Genre Model", RMSE=rmse_mean_movie_user_genre_model))

# show the resulting RMSE table
ml_results
rm(mean_movie_user_genre_model)

# combine user, movie, genre and release year together
mean_movie_user_genre_releaseyear_model <- test_set %>%
  inner_join(movie_bias, by="movieId") %>%
  inner_join(user_bias, by="userId") %>%
  inner_join(genre_bias, by="main_genre") %>%
  inner_join(releaseyear_bias, by="releaseyear")

# make prediction on test set with the movie/user/genre model
mean_movie_user_genre_releaseyear_model$predicted_rating <- mean_movie_user_genre_releaseyear_model$deviation_user + 
  mean_movie_user_genre_releaseyear_model$deviation_movie + 
  mean_movie_user_genre_releaseyear_model$deviation_genre + 
  mean_movie_user_genre_releaseyear_model$deviation_releaseyear + 
  movie_avg

# new calculation on RMSE on TS
rmse_mean_movie_user_genre_releaseyear_model <- RMSE(test_set$rating, mean_movie_user_genre_releaseyear_model$predicted_rating)

# add result
ml_results <- ml_results %>% 
  bind_rows(tibble(Model="Movie + User + Genre + Release Year Model", RMSE=rmse_mean_movie_user_genre_releaseyear_model))
ml_results
rm(mean_movie_user_genre_releaseyear_model)

# Movie, mean, User and genres 
main_genre_bias <- train_set %>%
  group_by(main_genre) %>%
  summarise(deviation_main_genre = mean(rating - movie_avg))

side1_genre_bias <- train_set %>%
  group_by(side1_genre) %>%
  summarise(deviation_side1_genre = mean(rating - movie_avg))

side2_genre_bias <- train_set %>%
  group_by(side2_genre) %>%
  summarise(deviation_side2_genre = mean(rating - movie_avg))

# combine user, movie and Genres
mean_movie_user_all_genre_model <- test_set %>%
  inner_join(movie_bias, by="movieId") %>%
  inner_join(user_bias, by="userId") %>%
  inner_join(main_genre_bias, by="main_genre") %>%
  inner_join(side1_genre_bias, by="side1_genre") %>%
  inner_join(side2_genre_bias, by="side2_genre")

# calculation prediction on test set with the movie, User, Genres calculations
mean_movie_user_all_genre_model$predicted_rating <- 
  mean_movie_user_all_genre_model$deviation_user + 
  mean_movie_user_all_genre_model$deviation_movie + 
  mean_movie_user_all_genre_model$deviation_main_genre + 
  mean_movie_user_all_genre_model$deviation_side1_genre + 
  mean_movie_user_all_genre_model$deviation_side2_genre + 
  movie_avg

# new calculation of RMSE TS
rmse_mean_movie_user_all_genre_model <- RMSE(test_set$rating, mean_movie_user_all_genre_model$predicted_rating)

# add result
ml_results <- ml_results %>% 
  bind_rows(tibble(Model="Movie + User + All Genre Model", RMSE=rmse_mean_movie_user_all_genre_model))

# calculate and show
ml_results
rm(mean_movie_user_all_genre_model)

# movie, Genres, User and Mean

comb_genre_bias <- train_set %>%
  group_by(genres) %>%
  summarise(deviation_comb_genre = mean(rating - movie_avg))

# combine user, movie and genres together
mean_movie_user_comb_genre_model <- test_set %>%
  inner_join(movie_bias, by="movieId") %>%
  inner_join(user_bias, by="userId") %>%
  inner_join(comb_genre_bias, by="genres")

# make prediction on test set with the mean, movie,user
mean_movie_user_comb_genre_model$predicted_rating <- 
  mean_movie_user_comb_genre_model$deviation_user + 
  mean_movie_user_comb_genre_model$deviation_movie + 
  mean_movie_user_comb_genre_model$deviation_comb_genre + 
  movie_avg

# New RMSE Calculations on TS
rmse_mean_movie_user_comb_genre_model <- RMSE(test_set$rating, mean_movie_user_comb_genre_model$predicted_rating)

# add result
ml_results <- ml_results %>% 
  bind_rows(tibble(Model="Movie + User + Combined Genre Model", RMSE=rmse_mean_movie_user_comb_genre_model))
ml_results
rm(mean_movie_user_comb_genre_model)

# Regularizations
# Define a sequence of lambda values
lambdas <- seq(0.00, 0.001, 0.0001)

# Function to calculate RMSE for a given lambda
calculate_rmse <- function(lambda, train_set, test_set, movie_avg) {
  
  # Calculate genre bias using the training set
  genre_bias <- train_set %>%
    group_by(main_genre) %>%
    summarise(deviation_genre = sum(rating - movie_avg) / (n() + lambda))
  
  # Build the model on the test set
  model <- test_set %>%
    inner_join(genre_bias, by = "main_genre")
  
  # Predict ratings
  model$predicted_rating <- model$deviation_genre + movie_avg
  
  # Calculate and return RMSE
  return(RMSE(test_set$rating, model$predicted_rating))
}

# Iterate over lambda values and calculate RMSE
rmses <- sapply(lambdas, function(lambda) {
  calculate_rmse(lambda, train_set, test_set, movie_avg)
})

# Visualize the results
qplot(lambdas, rmses, geom = "line", xlab = "Lambda", ylab = "RMSE", main = "Optimizing Lambda for RMSE")

# Identify the optimal lambda
optimal_lambda <- lambdas[which.min(rmses)]
print(paste("Optimal Lambda:", optimal_lambda))


# define sequences of values for genre, movie and user parameters
tuning_param_genre <- seq(0.005, .0055, 0.0005)
tuning_param_movie <- seq(0.8945, 0.8955, 0.0005)
tuning_param_user <- seq(0.7995, 0.8005, 0.0005)

# initialize variables for the best RMSE and corresponding tuning parameters
best_rmse <- Inf
best_params <- c()

# loop over all combinations of genre, movie and user parameters
for (genre_param in tuning_param_genre) {
  for (movie_param in tuning_param_movie) {
    for (user_param in tuning_param_user) {
      # compute the bias terms and predicted ratings using the current tuning parameters
      avg <- mean(train_set$rating)
      
      genre_bias <- train_set %>%
        group_by(main_genre) %>%
        summarise(deviation_genre = genre_param * sum(rating - movie_avg)/n())
      
      movie_bias <- train_set %>%
        group_by(movieId) %>%
        summarise(deviation_movie = movie_param * sum(rating - movie_avg)/n())
      
      user_bias <- train_set %>%
        group_by(userId) %>%
        summarise(deviation_user = user_param * sum(rating - movie_avg)/n())
      
      model <- test_set %>%
        inner_join(genre_bias, by="main_genre") %>%
        inner_join(movie_bias, by="movieId") %>%
        inner_join(user_bias, by="userId")
      
      model$predicted_rating <- model$deviation_genre + 
        model$deviation_user + 
        model$deviation_movie + 
        movie_avg
      
      # compute the RMSE and update the best RMSE and tuning parameters if applicable
      rmse <- RMSE(test_set$rating, model$predicted_rating)
      if (rmse < best_rmse) {
        best_rmse <- rmse
        best_params <- c(genre_param,movie_param,user_param)
      }
    }
  }
}

# print the best RMSE and tuning parameters
cat("Best RMSE:", best_rmse, "\n")
cat("Best tuning parameters:", paste0("genre_param=", best_params[1], ", movie_param=", best_params[2], ", user_param=", best_params[3]), "\n")


########### TUNING ##############
# sequence from 0 to 1
# Sequence from 0 to 1
tuning_param <- seq(0, 1, 0.1)

# Tune the genre bias
rmse_seq <- sapply(tuning_param, function(t) {
  avg <- mean(train_set$rating)
  
  genre_bias <- train_set %>%
    group_by(main_genre) %>%
    summarise(deviation_genre = t * sum(rating - movie_avg) / n())
  
  movie_bias <- train_set %>%
    group_by(movieId) %>%
    summarise(deviation_movie = 0.8944 * sum(rating - movie_avg) / n())
  
  user_bias <- train_set %>%
    group_by(userId) %>%
    summarise(deviation_user = 0.798 * sum(rating - movie_avg) / n())
  
  model <- test_set %>%
    inner_join(genre_bias, by = "main_genre") %>%
    inner_join(movie_bias, by = "movieId") %>%
    inner_join(user_bias, by = "userId")
  
  model$predicted_rating <- with(model, deviation_genre + deviation_user + deviation_movie + movie_avg)
  
  return(RMSE(model$rating, model$predicted_rating))
})


# plot the tuning parameters
qplot(tuning_param, rmse_seq, geom="line")

# find min tuning
param <- tuning_param[which.min(rmse_seq)]
param
# with a bit of tuning the sequence we find 0.005 for the genre bias

# tune the movie bias
rmse_seq <- sapply(tuning_param, function(t) {
  avg <- mean(train_set$rating)
  
  genre_bias <- train_set %>%
    group_by(main_genre) %>%
    summarise(deviation_genre = 0.005 * sum(rating - movie_avg)/n())
  
  movie_bias <- train_set %>%
    group_by(movieId) %>%
    summarise(deviation_movie = t * sum(rating - movie_avg)/n())
  
  user_bias <- train_set %>%
    group_by(userId) %>%
    summarise(deviation_user = 0.798 * sum(rating - movie_avg)/n())
  
  model <- test_set %>%
    inner_join(genre_bias, by="main_genre") %>%
    inner_join(movie_bias, by="movieId") %>%
    inner_join(user_bias, by="userId")
  
  model$predicted_rating <- model$deviation_genre + 
    model$deviation_user + 
    model$deviation_movie + 
    movie_avg
  
  return(RMSE(test_set$rating, model$predicted_rating))
})

# plot the tuning parameters
qplot(tuning_param, rmse_seq, geom="line")

# find min tuning
param <- tuning_param[which.min(rmse_seq)]
param
# with a bit of tuning we find 0.8944

# tune the user bias
rmse_seq <- sapply(tuning_param, function(t) {
  avg <- mean(train_set$rating)
  
  genre_bias <- train_set %>%
    group_by(main_genre) %>%
    summarise(deviation_genre = 0.005 * sum(rating - movie_avg)/n())
  
  movie_bias <- train_set %>%
    group_by(movieId) %>%
    summarise(deviation_movie = 0.8944 * sum(rating - movie_avg)/n())
  
  user_bias <- train_set %>%
    group_by(userId) %>%
    summarise(deviation_user = t * sum(rating - movie_avg)/n())
  
  model <- test_set %>%
    inner_join(genre_bias, by="main_genre") %>%
    inner_join(movie_bias, by="movieId") %>%
    inner_join(user_bias, by="userId")
  
  model$predicted_rating <- model$deviation_genre + 
    model$deviation_user + 
    model$deviation_movie + 
    movie_avg
  
  return(RMSE(test_set$rating, model$predicted_rating))
})

# plot the tuning parameters
qplot(tuning_param, rmse_seq, geom="line")

# find min tuning
param <- tuning_param[which.min(rmse_seq)]
param
# with a bit of tuning we find 0.798

# resulting tuned version
tuned_movie_user_genre_model <- function(t) {
  avg <- mean(train_set$rating)
  
  genre_bias <- train_set %>%
    group_by(main_genre) %>%
    summarise(deviation_genre = 0.0055 * sum(rating - movie_avg)/n())
  
  movie_bias <- train_set %>%
    group_by(movieId) %>%
    summarise(deviation_movie = 0.8944 * sum(rating - movie_avg)/n())
  
  user_bias <- train_set %>%
    group_by(userId) %>%
    summarise(deviation_user = 0.798 * sum(rating - movie_avg)/n())
  
  model <- test_set %>%
    inner_join(genre_bias, by="main_genre") %>%
    inner_join(movie_bias, by="movieId") %>%
    inner_join(user_bias, by="userId")
  
  model$predicted_rating <- model$deviation_genre + 
    model$deviation_user + 
    model$deviation_movie + 
    movie_avg
  
  return(RMSE(test_set$rating, model$predicted_rating))
}

tuned_movie_user_genre_model()
# 0.878559

# RMSE

# global movie average
movie_avg <- mean(train_set$rating)

genre_bias <- train_set %>%
  group_by(main_genre) %>%
  summarise(deviation_genre = 0.0055 * sum(rating - movie_avg)/n())

movie_bias <- train_set %>%
  group_by(movieId) %>%
  summarise(deviation_movie = 0.8944 * sum(rating - movie_avg)/n())

user_bias <- train_set %>%
  group_by(userId) %>%
  summarise(deviation_user = 0.798 * sum(rating - movie_avg)/n())

# make the final model by combining avg,movie and user bias
final_model <- final_holdout_test %>%
  #inner_join(genre_bias, by="main_genre") %>%
  inner_join(movie_bias, by="movieId") %>%
  inner_join(user_bias, by="userId")

# make predictions on final_holdout_test set
final_model$predicted_rating <-
  movie_avg +
  #final_model$deviation_genre + 
  final_model$deviation_user + 
  final_model$deviation_movie

# RMSE (on verification)
rmse_final_model <- RMSE(final_holdout_test$rating, final_model$predicted_rating)
rmse_final_model
# 0.8798819

# add result to table
ml_results <- ml_results %>% 
  bind_rows(tibble(Model="Final Model Verification", RMSE=rmse_final_model))

# show all achieved RMSE from table
ml_results


# sequence from 0 to 1
tuning_param <- seq(0.79, 0.85, 0.0025)

# tune the genre bias
rmse_seq <- sapply(tuning_param, function(t) {
  avg <- mean(train_set$rating)
  
  genre_bias <- train_set %>%
   group_by(main_genre) %>%
   summarise(deviation_genre = 0.0055 * sum(rating - movie_avg)/n())
  
  movie_bias <- train_set %>%
    group_by(movieId) %>%
    summarise(deviation_movie = 0.8925 * sum(rating - movie_avg)/n())
  
  user_bias <- train_set %>%
    group_by(userId) %>%
    summarise(deviation_user = t * sum(rating - movie_avg)/n())
  
  releaseyear_bias <- train_set %>%
    group_by(releaseyear) %>%
    summarise(deviation_releaseyear = 0.008 * sum(rating - movie_avg)/n())
  
  model <- test_set %>%
    #inner_join(genre_bias, by="main_genre") %>%
    inner_join(movie_bias, by="movieId") %>%
    inner_join(user_bias, by="userId") %>%
    inner_join(releaseyear_bias, by="releaseyear")
  
  model$predicted_rating <- #model$deviation_genre + 
    model$deviation_user + 
    model$deviation_movie + 
    model$deviation_releaseyear + 
    movie_avg
  
  return(RMSE(test_set$rating, model$predicted_rating))
})

# plot the tuning parameters
qplot(tuning_param, rmse_seq, geom="line")

# find min tuning
param <- tuning_param[which.min(rmse_seq)]
param

# genre_bias        0.0
# movie_bias        0.885
# user_bias         0.801
# releaseyear_bias  0.008



avg <- mean(train_set$rating)

genre_bias <- train_set %>%
  group_by(main_genre) %>%
  summarise(deviation_genre = 0.0055 * sum(rating - movie_avg)/n())

movie_bias <- train_set %>%
  group_by(movieId) %>%
  summarise(deviation_movie = 0.8925 * sum(rating - movie_avg)/n())

user_bias <- train_set %>%
  group_by(userId) %>%
  summarise(deviation_user = 0.801 * sum(rating - movie_avg)/n())

releaseyear_bias <- train_set %>%
  group_by(releaseyear) %>%
  summarise(deviation_releaseyear = 0.008 * sum(rating - movie_avg)/n())

model <- test_set %>%
  #inner_join(genre_bias, by="main_genre") %>%
  inner_join(movie_bias, by="movieId") %>%
  inner_join(user_bias, by="userId")

model$predicted_rating <- model$deviation_user + 
  model$deviation_movie + movie_avg 

final_rmse <- RMSE(test_set$rating, model$predicted_rating)
final_rmse

