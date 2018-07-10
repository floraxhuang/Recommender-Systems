rm(list=ls())
Sys.setlocale("LC_ALL", "English")

#Load library
library(dplyr)

#Read files
ratings <- read.csv(file.choose(),header = TRUE,stringsAsFactors = FALSE)
movies <- read.csv(file.choose(),header = TRUE,stringsAsFactors = FALSE)
tags <- read.csv(file.choose(),header = TRUE,stringsAsFactors = FALSE)

#Calculating the mean rating and subtracting from each rating of a user to calculate the adjusted rating.
Mean <- ratings %>% group_by(userId) %>% summarize(rating_mean = mean(rating))
Ratings <- merge(ratings, Mean, by="userId",sort=FALSE) %>% mutate(rating_adjusted=rating-rating_mean)

#Finding the top 30 similar user profiles for each user.
distinct_users <- unique(Ratings$userId)
distinct_movies <- unique(Ratings$movieId)

Bi_Filter <- function(user, ratings,targetdata){
  user_data <- ratings %>% filter(userId == user)
  user_val <- sqrt(sum(user_data$rating_adjusted^2))
  target_val <- sqrt(sum(targetdata$target_adjrating^2))
  names(user_data) <- c("movieuserId","movieId","user_rating","timestamp","rating_mean","user_adjrating")
  
  merge_data <- targetdata %>% inner_join(user_data[,c("user_adjrating","movieId","movieuserId")], by="movieId", sort=FALSE) %>%
    mutate(vector_product = target_adjrating*user_adjrating) %>% group_by(targetuserId,movieuserId) %>%
    summarize_all(funs(sum(as.numeric(.)))) %>% mutate(dot=vector_product/(user_val*target_val))
  
  return(merge_data)
}

Movie_Filter <- function(movie, ratings, targetdata){
  movie_data <- ratings %>% filter(movieId == movie)
  movie_distinct_user <- unique(movie_data$userId)
  bifiltereddata <- do.call(rbind,lapply(movie_distinct_user, function(x){Bi_Filter(x, ratings, targetdata)}))
  
  bifiltereddata <- bifiltereddata %>% filter(dot<1) %>% arrange(desc(dot)) %>% head(30) %>% mutate(movieId = movie)
  return(bifiltereddata)
}

User_Rating <- function(user, ratings, movielist){
  target_data <- ratings %>% filter(userId == user)
  names(target_data) <- c("targetuserId","movieId","target_rating","timestamp","rating_mean","target_adjrating")
  moviefiltereddata <- do.call(rbind,lapply(movielist, function(x){Movie_Filter(x, ratings, target_data)}))
  return(moviefiltereddata)
}

User_Data <- do.call(rbind,lapply(distinct_users, function(x){User_Rating(x,Ratings, distinct_movies)}))

#Calculating the predicted rating for each item and ignoring the item if less than 2 similar neighbours.
Movie_Append <- function(movie, userdata, targetdata, ratings){
  targetdata_mean <- mean(targetdata$rating)
  
  usermoviedata <- userdata %>% filter(movieId==movie)
  user_dot_adj_rating <- ratings %>% inner_join(usermoviedata[,c("dot","movieuserId","targetuserId")], by=c("userId"="movieuserId"), sort=FALSE)
  user_dot_adj_rating_m <- user_dot_adj_rating %>% filter(movieId==movie)
  if(length(unique(user_dot_adj_rating_m$userId))>=2){
    user_dot_adj_rating_m <- user_dot_adj_rating_m %>% mutate(wt_rating=dot*rating_adjusted) %>% 
      mutate(dot_abs=abs(dot)) %>% group_by(targetuserId) %>% summarise_all(funs(sum(as.numeric(.)))) %>%
      select(targetuserId, wt_rating, dot_abs) %>% mutate(Rating=(wt_rating/dot_abs)+targetdata_mean) %>%
      mutate(movieId=movie) %>% select(-c(wt_rating, dot_abs))
  }
  return(user_dot_adj_rating_m)
}

User_Dot_Rating <- function(user, ratings, movielist){
  target_data <- ratings %>% filter(userId == user)
  names(target_data) <- c("targetuserId","movieId","target_rating","timestamp","rating_mean","target_adjrating")
  movieappenddata <- do.call(rbind,lapply(movielist, function(x){Movie_Append(x, User_Data, target_data, Ratings)}))
  movieappenddata <- movieappenddata %>% arrange(desc(Rating))
  return(movieappenddata)
}

User_dot_adj_rating <- do.call(rbind,lapply(distinct_users, function(x){User_Dot_Rating(x, Ratings, distinct_movies)}))
