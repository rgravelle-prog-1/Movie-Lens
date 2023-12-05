---
title: "\\centering MovieLens Capstone Project"
author: "Robert Gravelle"
date: "2023-11-13"
output:
  pdf_document:
    df_print: kable
    toc: yes
    fig_width: 7
    fig_height: 6
    latex_engine: lualatex
    keep_tex: yes
  word_document:
    toc: yes
subtitle: Professional Certificate PH125.9x
header-includes:
- \usepackage{listings}
- \lstset{breaklines=true}
---
![Background Image](C:/RProj/MovieLens/MovieLens/graphic.png){width=50%, height=50%, opacity=.02}


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo=TRUE, progress=TRUE, verbose=TRUE, dev='pdf')

if(!require(pacman)) install.packages("pacman")
library(pacman)
p_load(ggplot2, devtools, benchmarkme, rmarkdown, lubridate, scales, parallel, stringr, kableExtra, tidyverse, caret)

options(tinytex.verbose = TRUE)
```

\newpage

# MovieLens


## Introduction
This report delves into the analysis of MovieLens data, encompassing the training of a recommendation model and assessing its performance. Initially, we embark on exploring and potentially refining the dataset, subjecting it to inspection for a comprehensive evaluation of potential training strategies. Subsequently, the focus shifts to constructing a machine learning model dedicated to suggesting movies to users.

## Dataset Information
The 10M MovieLens dataset is a rich repository of cinematic insights, containing 10 million user-generated ratings across a spectrum of 10,000 movies. This expansive collection captures the diverse perspectives of 72,000 randomly selected users, offering a nuanced glimpse into the tapestry of audience preferences. This dataset serves as a valuable foundation for in-depth analyses and the development of robust recommendation models within the realm of movie suggestions.

## Initial Project Configuration
In this project I initiated the loading process of the MovieLens 10M dataset, meticulously partitioning it into two subsets: *edx* and *final_holdout_test*. These subsets, constituting 10% of the entire MovieLens data, serve a specific purpose—solely reserved for conclusive validation towards the project's conclusion. In preparing this dataset, key elements used are: userId, movieId, rating, timestamp, title, and genre. Together, they form the foundational components of this dynamic dataset, prepared for detailed exploration and validation.. 

## Goal
This dataset serves as a portal for exploration, providing valuable insights into the intricate process of crafting an effective recommendation algorithm. The quest for a machine learning algorithm led me to a robust and meticulously developed algorithm to rigorously test against the dataset of the final_holdout_test set. The meticulous process ensures the algorithm's robustness and efficacy in pursuing an efficient program of movie recommendations.

## Summary
The analysis revealed interesting properties and correlations of the various features. Among other things, older films tended to be rated higher than newer ones, and some genres were generally rated slightly higher or lower. Above all, however, the films and the users play a decisive role in developing a model. Unfortunately, only an RMSE of about 0.879 was possible with this method. For further interesting model trainings neither the time nor the available computing power was sufficient, for example other training approaches like KNN or Decision Tree could be tried.

\newpage

## Code provided by EdX in the Capstone directons for the project


> ```{r}
> # Create edx and final_holdout_test sets
> 
> # Note: this process could take a couple of minutes
> 
> if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
> if(!require(caret)) install.packages("caret", repos = "http.cran.us.r-project.org")
> 
> library(tidyverse)
> library(caret)
> 
> # MovieLens 10M dataset:
> # https://grouplens.org/datasets/movielens/10m/
> # http://files.grouplens.org/datasets/movielens/ml-10m.zip
> 
> options(timeout = 250)
> 
> dl <- "ml-10M100K.zip"
> if(!file.exists(dl))
>   download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
> 
> ratings_file <- "ml-10M100K/ratings.dat"
> if(!file.exists(ratings_file))
>   unzip(dl, ratings_file)
> 
> movies_file <- "ml-10M100K/movies.dat"
> if(!file.exists(movies_file))
>   unzip(dl, movies_file)
> 
> ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
>                          stringsAsFactors = FALSE)
> colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
> ratings <- ratings %>%
>   mutate(userId = as.integer(userId),
>          movieId = as.integer(movieId),
>          rating = as.numeric(rating),
>          timestamp = as.integer(timestamp))
> 
> movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
>                         stringsAsFactors = FALSE)
> colnames(movies) <- c("movieId", "title", "genres")
> movies <- movies %>%
>   mutate(movieId = as.integer(movieId))
> 
> movielens <- left_join(ratings, movies, by = "movieId")
> 
> # Final hold-out test set will be 10% of MovieLens data
> set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
> # set.seed(1) # if using R 3.5 or earlier
> test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
> edx <- movielens[-test_index,]
> temp <- movielens[test_index,]
> 
> # Make sure userId and movieId in final hold-out test set are also in edx set
> final_holdout_test <- temp %>% 
>   semi_join(edx, by = "movieId") %>%
>   semi_join(edx, by = "userId")
> 
> # Add rows removed from final hold-out test set back into edx set
> removed <- anti_join(temp, final_holdout_test)
> ```



\newpage
# Analysis

## Data Inspection and preprocessing

Let's begin by examining the structure and contents of the Edx dataset in closer detail.

`r knitr::kable(head(edx,5)) %>% kable_styling(font_size=8, latex_options="hold_position")`

This dataset comprises various columns, each providing crucial information:

* **userId**: Uniquely identifies the user who rated the movie.
* **movieId**: Serves as a distinctive identifier for the rated movie.
* **rating**: Indicates the user-assigned rating for the movie, with the rating scale yet to be determined.
* **timestamp**: Presents the timestamp in UNIX format.
* **title**: Encompasses the movie's title, inclusive of its release year.
* **genres**: Enumerates a set of genres linked to the movie, with multiple genres separated by '|'.

Within the realms of the edx dataset, a multitude of captivating explorations await:

1. **Exceptional Cinematic Appeal:**
   - Identify movies that soar above the average rating, potentially fueled by compelling narratives or impeccable production.

2. **Genre Elevation:**
   - Scrutinize variations in ratings across different genres or their combinations, unveiling those that consistently receive higher acclaim.

3. **User Rating Dynamics:**
   - Delve into the potential bias within user ratings, discerning whether there's a prevalent inclination towards loftier or lower ratings overall.

4. **Genre Affinity Impact:**
   - Investigate how individual user genre preferences influence their ratings. For instance, a user inclined towards Horror and Action may tend to rate movies outside these genres lower.

5. **Temporal Rating Odyssey:**
   - Unearth the correlation between user ratings and movie release years, potentially revealing evolving patterns or trends over time.

6. **Popularity Quotient:**
   - Navigate the landscape of user ratings concerning the popularity spectrum, distinguishing between indie and blockbuster films. This exploration might unveil distinct rating patterns associated with movies of varying popularity levels.

\newpage

Before delving into our analysis, it's imperative to meticulously inspect the dataset for any missing values or anomalies in unique data names that might hint at outlier information.

The dataset *edx* contains `r sum(is.na(edx))` missing values.


```{r echo=FALSE, warning=FALSE}
# List genres in dataset 
unique_genres <- unique(unlist(strsplit(edx$genres, "\\|")))

```

Lets discover the genres in the dataset

\begin{center}

`r knitr::kable(data.frame(Genres = unique_genres))` 
\end{center}

We notice a unusual genre named in the list of  *(no genres listed)*.

`r knitr::kable(subset(edx, genres=="(no genres listed)")) %>% kable_styling(font_size=8, latex_options="hold_position")`

Upon delving deeper, it came to light that a solitary movie, "Pull My Daisy," lacks a designated genre. As per IMDB, this 1958 film falls under the category of "Short." To refine our dataset, let's extract and categorize the listed genres, allocating the initial three genres of each movie into distinct columns.

```{r echo=FALSE, warning=FALSE, results='asis'}
# "Pull My Daisy" from 1958 is the only movie without a genre, 

edx$genres <- ifelse(edx$genres == "(no genres listed)", "Short", edx$genres)

# Extract first few listed genres per movie in separate columns
edx$main_genre  <- sapply(strsplit(edx$genres, "\\|"), function(x) x[1])
edx$side1_genre <- sapply(strsplit(edx$genres, "\\|"), function(x) x[2])
edx$side2_genre <- sapply(strsplit(edx$genres, "\\|"), function(x) x[3])
```

In discovery we see there are `r { length(unique(unlist(strsplit(edx$genres, "\\|")))) }` unique genres in the EdX dataset.

Next, we convert the UNIX timestamps into a more readable date/time format and, for convenience, extract the year when the movie was rated. Additionally, we isolate the release year embedded in the title, laying the groundwork for potential further exploration.

```{r echo=FALSE}
# Extract data for ratings and release years
edx <- edx %>%
  mutate(date=as_datetime(timestamp), yearrated=year(date))  

edx <- edx %>%
  mutate(releaseyear=as.numeric(str_extract(str_extract(title, "[/(]\\d{4}[/)]$"), regex("\\d{4}"))))
```

Now some columns are added for further inspection, lets see the table again.

`r knitr::kable(head(edx)) %>% kable_styling(font_size=4, latex_options="hold_position")`

\newpage
## Distributions and Plots
\begin}center} We need to see the distribution of Ratings \end{center}

```{r echo=FALSE,  message=FALSE, warning=FALSE, results='asis', out.width="70%"}
# Distribution of ratings 
library(viridis)
library(dplyr)
library(ggplot2)

edx %>%
  group_by(rating) %>%
  summarise(perc = n() / nrow(edx)) %>%
  ggplot(aes(rating, perc, fill = as.factor(rating))) +
  geom_col() +
  scale_fill_viridis(discrete = TRUE) +  # Using Viridis color palette
  theme_light() +
  labs(x = "Rating", y = "%", title = "Rating distribution overall")
```

The majority of ratings are whole numbers; Additionally, the distribution of half-point ratings mirrors the pattern and distribution of the observed ratings in whole-number ratings.

\newpage

\begin{center} Now lets view the the distribution of the genres.\end{center}


```{r echo=FALSE, warning=FALSE, results='asis', out.width="70%"}
# Distribution of genres
library(viridis)
library(dplyr)
library(ggplot2)

edx %>% 
  group_by(main_genre) %>%
  summarise(perc = n() / nrow(edx)) %>% 
  ggplot(aes(reorder(main_genre, -perc), perc, fill = main_genre)) +
    geom_col() +
    scale_fill_viridis(discrete = TRUE) +  # Using Viridis color palette
    theme_light() +
    theme(axis.text.x = element_text(angle=90)) +
    labs(x = "Genre", y = "Percentage of movies", title = "Distribution of genres")

```
The prevalent genres predominantly include Action, Comedy, and Drama. Yet, delving into the realm of sub-genres unveils a more nuanced distribution. While Drama, Comedy, and Action still claim the top spots, their positions undergo subtle shifts. Consequently, Drama emerges as a frequently paired side genre, harmonizing seamlessly with genres such as Romance, Thriller, and others.
```{r echo=FALSE, warning=FALSE, results='asis', out.width="70%"}
library(viridis)
library(dplyr)
library(ggplot2)

edx %>%
  select(main_genre, side1_genre, side2_genre) %>%
  pivot_longer(cols=c(main_genre, side1_genre, side2_genre), values_to = "genre", values_drop_na = TRUE) %>%
  group_by(genre) %>%
  summarise(perc = n() / nrow(edx)) %>% 
  ggplot(aes(reorder(genre, -perc), perc, fill = genre)) +
    geom_col() +
    scale_fill_viridis(discrete = TRUE) +  # Using Viridis color palette
    theme_light() +
    theme(axis.text.x = element_text(angle=90)) +
    labs(x = "Genre", y = "Percentage of movies", title = "Distribution of genres and subgenres together")

```

\newpage

## Genres
\begin{center} Lets now see the Genres graphed against the average rating. \end{center}
```{r echo=FALSE, warning=FALSE, results='asis', out.width="70%"}
library(viridis)
library(dplyr)
library(ggplot2)

edx %>% 
  group_by(main_genre) %>%
  summarise(avg_genre_rating = mean(rating)) %>%
  arrange(desc(avg_genre_rating)) %>%
  ggplot(aes(reorder(main_genre, -avg_genre_rating), avg_genre_rating, fill = avg_genre_rating)) +
    geom_col() +
    scale_fill_viridis() +  # Using Viridis color palette
    theme_light() +
    theme(axis.text.x = element_text(angle=90)) +
    labs(x = "Genre", y = "Average Rating", title = "Rating distribution overall")
```
Genre ratings reveal a preference for "intellectual" movie genres, such as Film-Noir, Crime, and Drama, which consistently garner higher ratings compared to genres associated with entertainment, like Action, Fantasy, and Horror. This trend holds true when evaluating the average ratings of genre combinations, specifically those with more than 50,000 ratings.
```{r echo=FALSE, warning=FALSE, results='asis', out.width="70%"}
library(viridis)
library(dplyr)
library(ggplot2)

edx %>% 
  group_by(main_genre) %>%
  summarise(avg_genre_rating = mean(rating)) %>%
  arrange(desc(avg_genre_rating)) %>%
  ggplot(aes(reorder(main_genre, -avg_genre_rating), avg_genre_rating, fill = avg_genre_rating)) +
    geom_col() +
    scale_fill_viridis() +  # Using Viridis color palette
    theme_light() +
    theme(axis.text.x = element_text(angle=45)) +
    labs(x = "Genre", y = "Average Rating", title = "Rating distribution overall")
```

When taken genre combinations into account, a similar picture is painted, but some entertainment genre combinations, notably a Action combination, make it higher up the list (e.g. Action|Drama|War, Action|Adventure)

\newpage
\begin{center} Rating distribution of movies \end{center}

```{r echo=FALSE, warning=FALSE, results='asis', out.width="70%"}
library(viridis)
library(dplyr)
library(ggplot2)

edx %>% 
  group_by(movieId) %>%
  summarise(avg_rating = sum(rating)/n()) %>%
  ggplot(aes(x = avg_rating)) +
    geom_density(alpha = 0.7, fill = viridis(1)) +  # Smooth lines with Viridis color
    theme_light() +
    labs(x = "Avg Rating", y = "Movies", title = "Number of ratings per movie")

```
Only a scant number of movies achieve an average rating surpassing approximately 4.2. The majority of films fall within the range of 2 and 4.5 in terms of average ratings.
\begin{center} Average rating distribution of users \end{center}
```{r echo=FALSE, warning=FALSE, results='asis', out.width="70%"}
edx %>% 
  group_by(userId) %>%
  summarise(avg_rating = sum(rating)/n()) %>%
  ggplot(aes(avg_rating)) +
    geom_density(alpha = 0.7, fill = viridis(1)) + 
    theme_light() +
    labs(x="Avg Rating", y="Users", title = "Average ratings of users")
```
\newpage




## User Ratings

The majority of users tend to rate movies within the range of 3.5 to 4.2, surpassing the average ratings observed in the overall movie rating distribution.

\begin{center} Number of ratings per user \end{center}

```{r echo=FALSE, warning=FALSE, results='asis', out.width="70%"}
 edx %>% 
  count(userId) %>% 
  ggplot(aes(n)) + 
   geom_density(alpha = 0.7, fill = viridis(1)) +
    scale_x_log10() +
    #scale_y_log10() +  # log10 on number of ratings axis provides not a better readable graph
    theme_light() +
    labs(x = "Users", y = "Number of ratings")

```
 
\newpage

## Trendline Graphs for Ratingsand Release Dates
\begin{center} Trend lines average rating per release year \end{center}

```{r echo=FALSE, warning=FALSE, results='asis'}
library(viridis)
library(dplyr)
library(ggplot2)

edx %>% 
  group_by(releaseyear) %>%
  summarise(rating = mean(rating)) %>%
  ggplot(aes(releaseyear, rating, fill = rating)) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ poly(x,6)) +
    scale_fill_viridis() +  # Using Viridis color palette
    theme_light() +
    labs(x = "Release year", y = "Avg rating", title = "Avg rating per release year")

```
Films from the pre-1970 era tend to receive higher ratings compared to those released in the last two decades. This phenomenon is a well-known effect, where only the exceptional works withstand the test of time.

\newpage

\begin{center} Number of released movies per year \end{center}

```{r echo=FALSE, warning=FALSE, results='asis'}
library(viridis)
library(dplyr)
library(ggplot2)

edx %>%
  group_by(releaseyear) %>%
  summarise(n = n()) %>%
  ggplot(aes(releaseyear, n, fill = n)) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ poly(x,6)) +
    scale_fill_viridis() +  # Using Viridis color palette
    theme_light() +
    labs(x = "Release Year", y = "Number of Movies", title = "Number of Movies Released per Year")

```

Movie releases maintained a relatively stable pattern before 1970, experienced a significant uptick around 1990, and almost reached an explosive surge in the subsequent years. This surge can likely be attributed to advancements in technology, production, and market dynamics, including factors such as the proliferation of movie theaters, the rise of movie rentals, and television. However, since the late 1990s, the number of movie releases witnessed a sharp decline, reversing the rapid expansion observed a decade earlier.

\newpage

Ratings distribution over the years, focusing on the period from 1996 to 2008, with data outside this range registered as zero.

```{r echo=FALSE, warning=FALSE, results='asis'}
library(ggplot2)

edx %>%
  group_by(yearrated) %>%
  summarise(n = n()) %>%
  ggplot(aes(yearrated, n)) +
    geom_point(color = "blue") +
    geom_smooth(method = "lm", formula = y ~ poly(x, 6), color = "orange") +
    theme_light() +
    coord_cartesian(xlim = c(1996, 2008)) +
    labs(x = "Year rated", y = "Number of Movies", title = "Number of movies rated per year")

```
\newpage

\begin{center} Movie ratings per year are stable with some outlier \end{center}

Get the average rating of movie per genre but in 5 year bins.

```{r echo=FALSE, warning=FALSE, error=FALSE, results='asis'}
# average rating of movies of genres in 5-year bins
edx_5yr <- edx %>%
  mutate(five_year = floor((releaseyear - 1) / 5) * 5 + 1) %>%
  group_by(main_genre, five_year) %>%
  summarize(mean_rating = mean(rating, na.rm = TRUE)) %>%
  as.data.frame()

# include only some selected genres
edx_5yr_genre <- filter(edx_5yr, main_genre %in% c("Fantasy", "Action", "Adventure", "Drama", "Horror", "Thriller"))

# plot the average rating of a genres with trending curves
ggplot(edx_5yr_genre, aes(x = five_year, y = mean_rating, color = main_genre)) +
  geom_line() +
  geom_smooth(method = "lm", formula = y ~ poly(x,8), se = FALSE, size = 1.5) +
  scale_x_continuous(limits = c(1915, 2008), breaks = seq(1915, 2008, 5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  labs(x = "Year", y = "Average Rating", color = "Genre")

```

Certain genres maintain a steady average rating throughout the years, while others, such as Horror or Fantasy, exhibit more significant fluctuations.

\newpage
# Modeling 
Prepare the training and test datasets, and set up a table to track RMSE results as we refine the model. Based on insights from the mean+movie approach, it's essential to ensure that the training dataset encompasses all movieIds. This precaution guards against encountering movieIds in the test set without corresponding training data. I anticipate a similar consideration might apply to userId and the primary genre.

```{r echo=FALSE}
set.seed(35) 

# split data into training and test set
test_index <- createDataPartition(y = edx$rating, times=1, p=0.2, list=FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]

# Table of model results (RMSE)
ml_results <- tibble()

# Make sure all movieId and userId in the test_set are also in the train_set.
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
```

## Blind Guessing
Try blind guessing a rating from 0 to 5.

```{r echo=TRUE, warning=FALSE}
# Blind guessing
guess_model <- sample(c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5), length(test_set$rating), replace=TRUE)
```

```{r include=FALSE, echo=TRUE, warning=FALSE}
# calculate RMSE of the guessing
rmse_guessing <- RMSE(test_set$rating, guess_model)

# add result to table 
ml_results <- ml_results %>%
  bind_rows(tibble(Model="Guessing", RMSE=rmse_guessing))

# Resulting RMSE is about 2.156. Pretty far of the 0.8649 we are after.
# cleanup
rm(guess_model)
```

The resulting `r rmse_guessing` is still far of the < 0.8649, no suprise.

`r kable(ml_results)`

## Mean
Get the average of all ratings on the training set and use this to predict a movie.

```{r echo=TRUE}
# The mean rating for each movie in the training set.
avg_model <- mean(train_set$rating)
```

```{r include=FALSE, echo=TRUE, warning=FALSE}
# RMSE on TS
rmse_avg_model <- RMSE(test_set$rating, avg_model)
ml_results <- ml_results %>% 
  bind_rows(tibble(Model="Avg Model", RMSE=rmse_avg_model))
rm(avg_model)
```

The average of every movie used for predicting a rating results in `r { rmse_avg_model }`. Closer but still far off.

`r kable(ml_results)`

## Genre bias Deviation
Evaluate the genre bias, the deviation from the movie average for each genre. 

```{r echo=TRUE}
# movie average
movie_avg <- mean(train_set$rating)
genre_bias <- train_set %>%
  group_by(main_genre) %>%
  summarise(deviation_genre = mean(rating - movie_avg))

# combine genre bias with the test_set
mean_genre_model <- test_set %>%
  inner_join(genre_bias, by="main_genre")
```

```{r include=FALSE, echo=TRUE, warning=FALSE}
# Caculate the rating, genre bias of the movie in the test_set + average
mean_genre_model$predicted_rating <- mean_genre_model$deviation_genre + movie_avg
rmse_mean_genre_model <- RMSE(test_set$rating, mean_genre_model$predicted_rating)

# include results in running table
ml_results <- ml_results %>% 
  bind_rows(tibble(Model="Genre Model", RMSE=rmse_mean_genre_model))
rm(mean_genre_model)
```

The RMSE is slightly improved at r { rmse_mean_genre_model } compared to simply taking the average as done previously.

`r kable(ml_results)`

\newpage
## Movie bias rating

The movie bias represents the variance between the average movie rating and the overall mean rating.

```{r echo=TRUE}
movie_bias <- train_set %>%
  group_by(movieId) %>%
  summarise(deviation_movie = mean(rating - movie_avg))

mean_movie_model <- test_set %>%
  inner_join(movie_bias, by="movieId")
```

```{r include=FALSE, echo=TRUE, warning=FALSE}
# Calculate the rating based on the movie (using movieId) by incorporating the deviation and average.
mean_movie_model$predicted_rating <- mean_movie_model$deviation_movie + movie_avg

# RMSE TS
rmse_mean_movie_model <- RMSE(test_set$rating, mean_movie_model$predicted_rating)

# include results in running table
ml_results <- ml_results %>% 
  bind_rows(tibble(Model="Movie Model", RMSE=rmse_mean_movie_model))
rm(mean_movie_model)
```

With and RMSE of `r { rmse_mean_movie_model }` we are now in the sub 1 category.

`r kable(ml_results)`

## User bias
Lets inspect the user bias. 

```{r echo=TRUE}
user_bias <- train_set %>%
  group_by(userId) %>%
  summarise(deviation_user = mean(rating - movie_avg))

mean_user_model <- test_set %>%
  inner_join(user_bias, by="userId")
```

```{r include=FALSE, echo=TRUE, warning=FALSE}
# predict the rating, based the movie (by movieId) on the deviation + average 
mean_user_model$predicted_rating <- mean_user_model$deviation_user + movie_avg

# RMSE TS
rmse_mean_user_model <- RMSE(test_set$rating, mean_user_model$predicted_rating)

# include results in running table
ml_results <- ml_results %>% 
  bind_rows(tibble(Model="User Model", RMSE=rmse_mean_user_model))

# show the resulting RMSE table
ml_results
rm(mean_user_model)
```

The user bias results in an RMSE of `r { rmse_mean_user_model }`.

`r kable(ml_results)`


## Release year bias
Lets see if the release year will bring the RMSE down.

```{r echo=TRUE}
releaseyear_bias <- train_set %>%
  group_by(releaseyear) %>%
  summarise(deviation_releaseyear = mean(rating - movie_avg))

mean_releaseyear_model <- test_set %>%
  inner_join(releaseyear_bias, by="releaseyear")
```

```{r include=FALSE, echo=TRUE, warning=FALSE}
# predict the rating, based the release year on the deviation + average 
mean_releaseyear_model$predicted_rating <- mean_releaseyear_model$deviation_releaseyear + movie_avg

# RMSE TS
rmse_mean_releaseyear_model <- RMSE(test_set$rating, mean_releaseyear_model$predicted_rating)

# include results in running table
ml_results <- ml_results %>% 
  bind_rows(tibble(Model="Release Year Model", RMSE=rmse_mean_releaseyear_model))

# show the resulting RMSE table
ml_results
rm(mean_releaseyear_model)
```

The release year of a movie bias results in an RMSE of `r { rmse_mean_releaseyear_model }`.

`r kable(ml_results)`

## User and Movie bias
Let's add the average rating of a user into the mix. 

```{r echo=TRUE}
# user_bias is the differnece of the avg user rating to the mean rating
user_bias <- train_set %>%
  group_by(userId) %>%
  summarise(deviation_user = mean(rating - movie_avg))

mean_movie_user_model <- test_set %>%
  inner_join(movie_bias, by="movieId") %>%
  inner_join(user_bias, by="userId")
```

```{r include=FALSE, echo=TRUE, warning=FALSE}
# predict the rating, based the movie (by movieId) and the user (by userId) on the deviation + average 
mean_movie_user_model$predicted_rating <- mean_movie_user_model$deviation_user + 
                                          mean_movie_user_model$deviation_movie + 
                                          movie_avg

# RMSE TS
rmse_mean_movie_user_model <- RMSE(test_set$rating, mean_movie_user_model$predicted_rating)

# include results in running table
ml_results <- ml_results %>% 
  bind_rows(tibble(Model="Movie + User Model", RMSE=rmse_mean_movie_user_model))

rm(mean_movie_user_model)
```

The user and movie gets us below 0.9. But `r { rmse_mean_movie_user_model }` is still not near the desired < 0.865.

`r kable(ml_results)`


## User and Movie and Release Year bias
To the previous model we add the release year.

```{r echo=TRUE}
mean_movie_user_releaseyear_model <- test_set %>%
  left_join(movie_bias, by='movieId') %>%
  left_join(user_bias, by='userId') %>%
  left_join(releaseyear_bias, by='releaseyear')
```

```{r include=FALSE, echo=TRUE, warning=FALSE}
# make prediction on test set with the movie/user/releaseyear model
mean_movie_user_releaseyear_model$predicted_rating <- mean_movie_user_releaseyear_model$deviation_user + 
                                                      mean_movie_user_releaseyear_model$deviation_movie + 
                                                      mean_movie_user_releaseyear_model$deviation_releaseyear + 
                                                      movie_avg

# RMSE TS
rmse_mean_movie_user_releaseyear_model <- RMSE(test_set$rating, mean_movie_user_releaseyear_model$predicted_rating)

# add result to table
ml_results <- ml_results %>% 
  bind_rows(tibble(Model="Movie + User + Release Year Model", RMSE=rmse_mean_movie_user_releaseyear_model))

# show the resulting RMSE table
ml_results

rm(mean_movie_user_releaseyear_model)
```

With the release year added to the user and movie bias we get `r { rmse_mean_movie_user_releaseyear_model }`.

`r kable(ml_results)`

## User, Movie and Genre bias
Now combine user, movie and genre together in a single model.

```{r echo=TRUE}
mean_movie_user_genre_model <- test_set %>%
  inner_join(movie_bias, by="movieId") %>%
  inner_join(user_bias, by="userId") %>%
  inner_join(genre_bias, by="main_genre")
```

```{r include=FALSE, echo=TRUE, warning=FALSE}
mean_movie_user_genre_model$predicted_rating <- mean_movie_user_genre_model$deviation_user + 
                                                mean_movie_user_genre_model$deviation_movie + 
                                                mean_movie_user_genre_model$deviation_genre + 
                                                movie_avg

# RMSE TS
rmse_mean_movie_user_genre_model <- RMSE(test_set$rating, mean_movie_user_genre_model$predicted_rating)

# add result to table
ml_results <- ml_results %>% 
  bind_rows(tibble(Model="Movie + User + Genre Model", RMSE=rmse_mean_movie_user_genre_model))
```

This resulted in `r { rmse_mean_movie_user_genre_model } `, which is worse than only using the movie and user. Maybe some tuning will fix it.

`r kable(ml_results)`


## User, Movie, Release Year and Genre bias
Now combine user, movie, release year and genre together in a single model.

```{r echo=TRUE}
mean_movie_user_genre_releaseyear_model <- test_set %>%
  inner_join(movie_bias, by="movieId") %>%
  inner_join(user_bias, by="userId") %>%
  inner_join(genre_bias, by="main_genre") %>%
  inner_join(releaseyear_bias, by="releaseyear")
```

```{r include=FALSE, echo=TRUE, warning=FALSE}
# make prediction on test set with the movie/user/genre model
mean_movie_user_genre_releaseyear_model$predicted_rating <- mean_movie_user_genre_releaseyear_model$deviation_user + 
                                                mean_movie_user_genre_releaseyear_model$deviation_movie + 
                                                mean_movie_user_genre_releaseyear_model$deviation_genre + 
                                                mean_movie_user_genre_releaseyear_model$deviation_releaseyear + 
                                                movie_avg

# RMSE TS
rmse_mean_movie_user_genre_releaseyear_model <- RMSE(test_set$rating, mean_movie_user_genre_releaseyear_model$predicted_rating)

# add result to table
ml_results <- ml_results %>% 
  bind_rows(tibble(Model="Movie + User + Genre + Release Year Model", RMSE=rmse_mean_movie_user_genre_releaseyear_model))

# show the resulting RMSE table
ml_results

# cleanup
rm(mean_movie_user_genre_releaseyear_model)
```

This resulted in `r { rmse_mean_movie_user_genre_releaseyear_model } `.

`r kable(ml_results)`

## User, Movie, Release Year and first three listed genres bias
Same as before but instead of the whole genres list as a whole the first three listed genres are used. 

```{r echo=TRUE}
main_genre_bias <- train_set %>%
  group_by(main_genre) %>%
  summarise(deviation_main_genre = mean(rating - movie_avg))

side1_genre_bias <- train_set %>%
  group_by(side1_genre) %>%
  summarise(deviation_side1_genre = mean(rating - movie_avg))

side2_genre_bias <- train_set %>%
  group_by(side2_genre) %>%
  summarise(deviation_side2_genre = mean(rating - movie_avg))

mean_movie_user_all_genre_model <- test_set %>%
  inner_join(movie_bias, by="movieId") %>%
  inner_join(user_bias, by="userId") %>%
  inner_join(main_genre_bias, by="main_genre") %>%
  inner_join(side1_genre_bias, by="side1_genre") %>%
  inner_join(side2_genre_bias, by="side2_genre")
```

```{r include=FALSE, echo=TRUE, warning=FALSE}
# make prediction on test set with the movie/user/genre model
mean_movie_user_all_genre_model$predicted_rating <- 
  mean_movie_user_all_genre_model$deviation_user + 
  mean_movie_user_all_genre_model$deviation_movie + 
  mean_movie_user_all_genre_model$deviation_main_genre + 
  mean_movie_user_all_genre_model$deviation_side1_genre + 
  mean_movie_user_all_genre_model$deviation_side2_genre + 
  movie_avg

# RMSE TS
rmse_mean_movie_user_all_genre_model <- RMSE(test_set$rating, mean_movie_user_all_genre_model$predicted_rating)

# add result to table
ml_results <- ml_results %>% 
  bind_rows(tibble(Model="Movie + User + All Genre Model", RMSE=rmse_mean_movie_user_all_genre_model))

# show the resulting RMSE table
ml_results
rm(mean_movie_user_all_genre_model)
```

This resulted in `r { rmse_mean_movie_user_all_genre_model } `.

`r kable(ml_results)`


```{r echo=FALSE}
########### REGULARIZATION ##############
lambdas <- seq(0.00, 0.001, 0.0001)

rmses <- sapply(lambdas, function(lambda) {
  
  genre_bias <- train_set %>%
    group_by(main_genre) %>%
    summarise(deviation_genre = sum(rating - movie_avg)/(n() + lambda))

  model <- test_set %>%
    inner_join(genre_bias, by="main_genre")
  
  model$predicted_rating <- model$deviation_genre + movie_avg

  return(RMSE(test_set$rating, model$predicted_rating))
})

#qplot(lambdas, rmses, geom = "line")
#lambda <- lambdas[which.min(rmses)]
#print(lambda)
```
\newpage 
## Tuning

Lets try to tune the three biases, each individually and plot the tuning parameter and the resulting RMSE.

```{r echo=FALSE, warning=FALSE, results='asis', out.width="50%"}
tuning_param <- seq(0, 1, 0.1)

### tuning genre bias
rmse_seq <- sapply(tuning_param, function(t) {
  avg <- mean(train_set$rating)
  
  genre_bias <- train_set %>%
    group_by(main_genre) %>%
    summarise(deviation_genre = t * sum(rating - movie_avg)/n())
  
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
})

# plot the tuning parameters
qplot(tuning_param, rmse_seq, geom="line", color = I("blue")) +
  labs(title = "Tuning Parameters vs RMSE",
       x = "Tuning Parameter",
       y = "RMSE")

# tuning movie bias
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
qplot(tuning_param, rmse_seq, geom="line", color = I("green")) +
  labs(title = "Tuning Parameters vs RMSE",
       x = "Tuning Parameter",
       y = "RMSE")

#### tuning user bias
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
qplot(tuning_param, rmse_seq, geom="line", color = I("red")) +
  labs(title = "Tuning Parameters vs RMSE",
       x = "Tuning Parameter",
       y = "RMSE")
```

After searching for each tuning parameter individually these are the final tuning parameters:

* genre_bias: 0.0055
* movie_bias: 0.8944
* user_bias: 0.798

The resulting tuned function with the individual bias tuning factors:

```{r echo=TRUE}
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

# add result to table
ml_results <- ml_results %>% 
  bind_rows(tibble(Model="Tuned model", RMSE=tuned_movie_user_genre_model()))

```

kable(ml_results, format = "latex", booktabs = TRUE, col.names = c("Model", "RMSE")) %>%
  kable_styling(full_width = FALSE, latex_options = "hold_position", position = "center") %>%
  row_spec(0, bold = TRUE, color = "white", background = "#2E86C1") %>%
  row_spec(which(ml_results$RMSE == min(ml_results$RMSE)), background = "#82E0AA")

\newpage

## Results
### RMSE
Lets test the final model with its tuning in place against the verification set.

```{r echo=TRUE}
final_model_prediction <- function() {
  avg <- mean(train_set$rating)
  
  #genre_bias <- train_set %>%
  #  group_by(main_genre) %>%
  #  summarise(deviation_genre = 0.0055 * sum(rating - movie_avg)/n())
  
  movie_bias <- train_set %>%
    group_by(movieId) %>%
    summarise(deviation_movie = 0.8944 * sum(rating - movie_avg)/n())
  
  user_bias <- train_set %>%
    group_by(userId) %>%
    summarise(deviation_user = 0.798 * sum(rating - movie_avg)/n())
  
  model <- final_holdout_test %>%
    #inner_join(genre_bias, by="main_genre") %>%
    inner_join(movie_bias, by="movieId") %>%
    inner_join(user_bias, by="userId")
  
  model$predicted_rating <- #model$deviation_genre + 
    model$deviation_user + 
    model$deviation_movie + 
    movie_avg
  return(model$predicted_rating)
}
```

```{r include=FALSE, echo=TRUE, warning=FALSE}
# RMSE (on verification)
rmse_final_model <- RMSE(final_holdout_test$rating, final_model_prediction())

# add result to table
ml_results <- ml_results %>% 
  bind_rows(tibble(Model="Final Model Verification", RMSE=rmse_final_model))
```

This are all the achieved model RMSE:

`r knitr::kable(ml_results, "latex", booktabs = TRUE) %>%
  kable_styling(full_width = FALSE) %>%
  row_spec(0, bold = TRUE, background = "#2E86C1", color = "white") %>%
  row_spec(which.min(ml_results$RMSE), background = "#82E0AA")
`
\newpage
# Conclusion
Even with all this tuning, lower than `r rmse_final_model` is not possible with this approach. More training and maybe separating different features is needed.  

### Future Improvements
Further information about the users could improve accuracy, e.g. shopping preferences, music taste, background information like education level. But there privacy concern about the usage of user personal data has to be considered. Training with larger dataset would be beneficial but would require more capable systems (e.g. with GPU). 
Even with this dataset and no more features or data other model algorithms could be tried, for example KNN, SVM or Decision Tree could be tried. For further interesting model trainings neither the time nor the available computing power was sufficient.


\newpage
# Resources and References

1. Rafael Irizarry. 2018. Introduction to Data Science.
2. Jared Lander, 2017, R for Everyone Advanced Analytics and Graphics.
3. Norman Matloff, 2011 & 2019, The Art of R Programming





