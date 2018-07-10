rm(list=ls())
Sys.setlocale("LC_ALL", "English")

#Load library
library(dplyr)

#Read files
ratings <- read.csv(file.choose(),header = TRUE,stringsAsFactors = FALSE)
movies <- read.csv(file.choose(),header = TRUE,stringsAsFactors = FALSE)
tags <- read.csv(file.choose(),header = TRUE,stringsAsFactors = FALSE)

#Calculating the mean rating and subtracting from each rating of a user to calculate the adjusted rating.
Mean <- ratings %>% group_by(movieId) %>% summarize(rating_mean = mean(rating)) 
Ratings <- ratings %>% left_join(Mean,by="movieId",sort=FALSE) %>% mutate(rating_adjusted=rating-rating_mean)

#Calculating the similarity value for each movie user has not rated 
#to movies user has rated and selecting 20 most similar movies .
distinct_user <- unique(Ratings$userId) 

Bi_Filter <- function(movie, ratings, targetmovie){
  movie_data_u <- ratings %>% filter(movieId==movie) %>% select(userId, movieId, rating_adjusted) %>% distinct()
  names(movie_data_u) <- c("userId", "usermovieId", "useradjrating")
  movie_val_u <- sqrt(sum(movie_data_u$useradjrating^2))
  movie_val_t <- sqrt(sum(targetmovie$targetadjrating^2))
  
  movie_data <- targetmovie %>% inner_join(movie_data_u, by="userId", sort=FALSE) %>%
    mutate(vector_product = targetadjrating*useradjrating) %>% group_by(targetmovieId,usermovieId) %>%
    summarise_all(funs(sum(as.numeric(.)))) %>% mutate(dot=vector_product/(movie_val_t*movie_val_u))
  
  return(movie_data)
}

Movie_Filter <- function(movie, ratings, user){
  movie_data_t <- ratings %>% filter(movieId==movie) %>% select(userId, movieId, rating_adjusted) %>% distinct()
  names(movie_data_t) <- c("userId", "targetmovieId", "targetadjrating")
  user_data <- ratings %>% filter(userId==user)
  movielist_u <- unique(user_data$movieId)
  bifilteredmovie <- do.call(rbind,lapply(movielist_u, function(x){Bi_Filter(x, ratings, movie_data_t)}))
  
  bifilteredmovie <- bifilteredmovie %>% filter(dot<1) %>% arrange(desc(dot)) %>% head(20)
  return(bifilteredmovie)
}

User_Rating <- function(user, ratings){
  target_data <- ratings %>% filter(userId != user)
  movielist_t <- unique(target_data$movieId)
  moviefiltereddata <- do.call(rbind,lapply(movielist_t, function(x){Movie_Filter(x, ratings, user)}))
  return(moviefiltereddata)
}

movie_data <- do.call(rbind,lapply(distinct_user, function(x){User_Rating(x, Ratings)}))

#Finally calculating the predicted rating for the movies

Movie_NBR <- function(movie, moviedata, ratings, userdata){
  movie_nbr <- moviedata %>% filter(targetmovieId==movie)
  movie_ratings <- ratings %>% filter(movieId==movie)
  movie_mean <- mean(movie_ratings$rating)
  movie_nbr_dot <- userdata %>% 
    inner_join(movie_nbr[,c("dot","usermovieId","targetmovieId")], by=c("movieId"="usermovieId"), sort=FALSE) %>%
    mutate(wt_rating = dot*rating_adjusted) %>% mutate(dot_abs = abs(dot)) %>% group_by(targetmovieId) %>%
    summarise_all(funs(sum(as.numeric(.)))) %>% select(targetmovieId, wt_rating, dot_abs) %>%
    mutate(Rating = (wt_rating/dot_abs)+movie_mean)
  return(movie_nbr_dot)
}

NBR_Rating <- function(user, ratings, moviedata){
  target_data <- ratings %>% filter(userId != user)
  user_data <- ratings %>% filter(userId == user)
  movielist_t <- unique(target_data$movieId)
  NBRdata <- do.call(rbind,lapply(movielist_t, function(x){Movie_NBR(x, moviedata, ratings, user_data)}))
  NBRdata <- NBRdata %>% arrange(desc(Rating))
  return(NBRdata)
}

movie_rating <- do.call(rbind,lapply(distinct_user, function(x){NBR_Rating(x, Ratings, movie_data)}))
