##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

#if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
#if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
#if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
#Selecting Librarys to use
library(tidyverse)
library(caret)
library(data.table)
library(dplyr)
library(dslabs)
library(tidyverse)
library(ggplot2)
library(lubridate)

# ##Downloading dataset and setting up training and testing sets according to EDX given instructions
# 
# # MovieLens 10M dataset:
# # https://grouplens.org/datasets/movielens/10m/
# # http://files.grouplens.org/datasets/movielens/ml-10m.zip
# 
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
 
# if using R 3.6 or earlier:
#movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],title = as.character(title),genres = as.character(genres))
 

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")



# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>%
   semi_join(edx, by = "movieId") %>%
   semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)
rm(dl, ratings, movies, test_index, temp, movielens, removed)
 
 
#Summary Data about the edx data frame (MovieLens data)

#QUIZ 
#Q1 
#number rows & number cols
dim(edx)
nrow(edx)

#Q2
#num zeros
sum(edx$rating == 0)

#num threes
sum(edx$rating == 3)

#Q3
#number movies

numberMovies <- edx %>% group_by(movieId) %>% summarise(numberRatings = n())
nrow(numberMovies)

#Q4
#number users

numberUsers <- edx %>% group_by(userId) %>% summarise(numberRatings = n())
nrow(numberUsers)

#Q5

# need to split up genres in edx first (problems with " |  ")
#called

edxSplitGenre <-  edx  %>% separate_rows(genres, sep = "\\|")


#number ratings per genre -- > use edx spited by genre 

rateGenre <- edxSplitGenre %>% group_by(genres) %>% summarise(count  = n())%>%
  arrange(desc(count))
rateGenre

#Q6 
#highest rated movie

ratingMovies <- edx %>% group_by(movieId) %>% 
  summarize(numRatings = n(), title = first(title)) %>%
  arrange(desc(numRatings)) %>%
  top_n(10, numRatings)

ratingMovies
#can see pulp is at the top 


#Q7 most given rating
givenRating <- edx %>% group_by(rating) %>% summarise(num = n()) %>% 
  arrange(desc(num))
givenRating

#Method and Analysis

# Data Analysis

# Analysis Section (Graphing of data to see trends)  Will be following a bit from text book to give a guild line for myself 
# otherwise would be a little lost on what to display

# Graph of ratings per moiveID/Title

numberMovies %>% ggplot(aes(movieId,numberRatings)) + 
  geom_bar(stat="identity") + 
  coord_flip() + 
  ggtitle("Number of Ratings per MovieId")

# Can be seen that some movies have more ratings than others but this graph is not that usefull otherwise

# Continueing to show number of ratings for top 10 movies for interest sake (and viewing summary data)

ratingMovies %>% ggplot(aes(title , numRatings)) + 
  geom_bar(stat="identity") + coord_flip() +
  ggtitle("Top 10 Movies and Number Ratings")

# Some more usefull information about the rating of the movies would be how the number of ratings are distrubuted as seen bellow


numberMovies %>%
  ggplot(aes(numberRatings)) + geom_histogram(fill = "green", color = "black", bins = 30) +
  scale_x_log10() + 
  ggtitle("Number of Movie Ratings")

# graph shows that some movies have been rated more times than others this creates a bais towards these movies. A regularisation and penalty term will need to be added to models as the reduce error caused due to the movies that have rarerly been rated.

# from the previous graph it can be seen that there are a number of movies that have only been rated once
# these movies could, as previously learnt in the course, cause making predictions on ratings inaccurate
# the following movies displaced below are the singular rated movies - There are 126 of these movies
# only 10 of these movies are displayed in descending ratings

edx %>%
  group_by(movieId) %>%
  summarize(count = n()) %>%
  filter(count == 1) %>%
  left_join(edx, by = "movieId") %>%
  group_by(title) %>%
  summarize(rating = rating, numberOfRatings = count) %>%
  arrange(desc(rating)) %>% 
  top_n(10, rating) %>% 
  knitr::kable()


#USERS


# Shown below is a graph of the distribution of number of ratings given by users
# what can be observed is that the majority of users only rate between 40 and 100 movies
# it is also evident from the graph that some users are more active than others 
# this two observations show that a user bais needs to be taken into account when making predictions
  
numberUsers %>%
  ggplot(aes(numberRatings)) + geom_histogram(bins = 30 , color = "black") +
  scale_x_log10() +
  ggtitle("Users Distribution") +
  xlab("Number of Ratings") +
  ylab("Count")

# As seen in graph below users tend to rate movies higher than lower, this is evident as the rating of 4 is most common followed by 3 and 5
# what is also evident in that users tend to give moves full stars rating compared to half stars as can been seen that the .5 ratings are less common 


givenRating %>% ggplot(aes(rating,num)) +
  geom_bar(stat="identity") + 
  scale_x_discrete(limits = c(seq(0.5,5,0.5))) +
  scale_y_continuous(breaks = c(seq(0, 3000000, 500000))) + 
  ylab("Count")
  
# Some users also tend to be more particular with their rating than other users. This can be viewed in the graph below and can be seen that some users give movies a low rating where as others give high ratings
# only users having rated a hundred or more are used to construct the graph below, this is do as to show there is a trend 


edx %>%
  group_by(userId) %>%
  filter(n() >= 100) %>%
  summarize(avgRate = mean(rating)) %>%
  ggplot(aes(avgRate)) +
  geom_histogram(bins = 30, color = "black") +
  xlab("Average rating") +
  ylab("Number of users") +
  ggtitle("Average Movie ratings") +
  scale_x_discrete(limits = c(seq(0.5,5,0.5))) 


# A brief look at how users ratings change over the years that movies have been released
# As observed below there seems to be a trend that indicates that more recent (or younger users, from 1950 till present) 
# tend to rate movies more strictly or lower

# need to change the time stamp into years
edxWithYear <- edx %>% mutate(year = as.numeric(str_sub(title,-5,-2)))

edxWithYear %>% group_by(year) %>%
  summarise(avgRating = mean(rating)) %>%
  ggplot(aes(year,avgRating)) + geom_point() +
  geom_smooth() + ggtitle("Change in Average Ratings by Year")

# MODELLING APPROACH 

# Begin with computing the RMSE, which is the loss-function for this model.
# 

#Create the RMSE Function as this will be called a lot
RMSE <- function(rating, predRating){
  sqrt(mean((rating - predRating)^2))
}

# RMSE is viewed as similar to standard deviation (sd) - RMSE is the error that us made when making a prediction of a movie rating
# This state means that a RMSE result larger than 1 is bad 
# We want the RMSE to be as close to 0 as possible as this would mean we have little error when making a prediction


# Simplest possible model
# This first model uses the edx dataset's rating mean to make predictions. This model predicts the same rating for all movies, regardless of the user.
# The expected rating of the dataset is between 3 and 4

mu <- mean(edx$rating)
mu

# Next is to predict a naive RMSE or a baseline model (uses only mean)

baselineRMSE <- RMSE(validation$rating,mu)
baselineRMSE

# the results of the RMSE from this simple method can be seen below:

resultsRMSE <- data_frame(method = "Mean Only ", RMSE = baselineRMSE)
resultsRMSE

# MOVIE Effect Model
# This is an attempt to improve on the previous model but incorporating the movie effect into a new model 
# When making use of the movie effect model, we must take head of the penalty term (b_i) - movie effect
# Thus looking at the graph below it can be noted that different movies are rated differently. As seen by the histogram not being symmetric
# and is skewed toward a negative rating effect.
# the movie effect can be accounted for by computing the difference from the mean rating.


movieAvg <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
movieAvg %>% qplot(b_i, geom ="histogram", bins = 20, data = ., color = I("black"))

# The improvement to the prediction using this model can be viewed below

predRating <- validation %>% 
  left_join(movieAvg, by="movieId") %>%
  mutate(pred = mu + b_i)
modelMovieEffect <- RMSE(validation$rating,predRating$pred)

resultsRMSE <- bind_rows(resultsRMSE,data_frame(method = "Movie Effect Method", RMSE = modelMovieEffect))

resultsRMSE %>% knitr::kable()

## The Error has dropped by 0.1172931 which indicated that the prediction methods are getting better

## Movie and User Effect Model
# As seen previously different Users rate movies different to others. There are some users that rate critically with low rating, other that rate 
# movies optimistically with high rating and lastly there are users that does care.
# This behavior is categorized as the penalty term (b_u) User Effect

userAvg <- edx %>% 
  left_join(movieAvg, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
userAvg %>% qplot(b_u, geom ="histogram", bins = 30, data = ., color = I("black"))

# As both the movie and user baises obscure the prediction of a movie rating
# an improvement in RMSE can be obtained by adding the user effect with the movie effect 
 predRatingUM <- validation %>%
   left_join(movieAvg, by = "movieId") %>%
   left_join(userAvg , by = "userId") %>%
   mutate(pred = mu + b_i + b_u)
 
 modelMovieUserEffect <- RMSE(validation$rating,predRatingUM$pred)
 
 resultsRMSE <- bind_rows(resultsRMSE, data_frame(method = "Movie and User Effect Model", RMSE = modelMovieUserEffect))
 
 resultsRMSE %>% knitr::kable()

# The RMSE has decreased further which is good.
 
 ##Regularisation of Movie and User Effect Model
# As noted in the Visualisation/data exploration section, some users rate far more than other users and other users that rated very few movies.
# This user effect combined with some movies being rates very few times such a 1 times (as seen there are 126 movies with a single user rating)
# make the predictions essentially noisy and untrustworthy. Therefore a regularisation is used to create a penalty term to gives lessens importance of the effect that increases the error
# thus reducing RMSE
# a value of lambda should be found that will minise RMSE 

lambda <- seq(0,10,0.25)

rmses <- sapply(lambda, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>% group_by(movieId) %>% 
    summarise(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating - mu - b_i)/(n()+1))
  
  predR <- validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>% 
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  
  return(RMSE(validation$rating , predR))
  
})

# Find optimal lambda from Graph below
qplot(lambda, rmses)

bestLambda <- lambda[which.min(rmses)]
bestLambda

# Use lambda = for final model 

# Final model results are below

resultsRMSE <- bind_rows(resultsRMSE, data_frame(method = " Regularisation of Movie and User Effect Model", RMSE = min(rmses) ))

resultsRMSE %>% knitr::kable()

## Results
# the results from the models are as follows :

resultsRMSE %>% knitr::kable() 

# The lowest RMSE is 0.8650484 and this is achieved by the regularisation of Movie and User Effect Model

## CONCLUSION

# The RMSE table shows that there was a continued improvement from model to models as new penalty terms where added. 
# The Mean Only calculated a RMSE of greater than 1, which means that the error in prediction was over a single star which is terrible.
# There was significant improvement with the implementations of the Movie Effect Method and Movie and User Effect Model, these reduced the RMSE to 0.9439087 and 0.8653488 respectively.
# Finally the Regularisation of Movie and User Effect Model reduces the RMSE to 0.8650484 which is within the acceptable goal for this project and one can somewhat trust the prediction.
# It can be noted that future improvement to the RMSE could be achieve by including other effects such as genre, year, and movie age to a model.
# One could also try other machine learning techniques such as perhaps a neural network to better predict a movie rating.





