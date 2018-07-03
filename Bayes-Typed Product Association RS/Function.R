rm(list=ls())

#Load library
library(plyr)

#Read files
ratings <- read.csv(file.choose(),header = TRUE)
movies <- read.csv(file.choose(),header = TRUE)
tags <- read.csv(file.choose(),header = TRUE)
  
distinct_movies <- sort(unique(ratings$movieId),decreasing = FALSE)
distinct_raters <- sort(unique(ratings$userId),decreasing = FALSE)

ifRated <- function(df){
  sapply(1:length(distinct_raters), function(x){ifelse(distinct_raters[x] %in% df$userId,1,0)})
}
RatedMatrix <- dlply(ratings[,1:2], .(movieId), ifRated)

findMatch <- function(data,movie,movielist, movienames){
  orig_data <- data[[movie]]
  other_data <- data[-movie]
  
  comp <- sapply(other_data, function(x){sum((orig_data+x)==2)/sum(orig_data)})
  comp <- append(comp,0,movie-1)
  comp_max <- which(comp==sort(comp,partial=length(comp))[length(comp)])
  movie_name <- sapply(comp_max, function(x){movienames$title[which(movienames$movieId==movielist[x])]})
  rec <- setNames(data.frame(matrix(ncol = 4, nrow = length(movie_name))), c("MovieID","MovieTitle","RecommendationID","RecommendationTitle"))
  rec$MovieID <- rep(movielist[movie], length(movie_name))
  rec$MovieTitle <- rep(movienames$title[which(movienames$movieId==movielist[movie])],length(movie_name))
  rec$RecommendationID <- comp_max
  rec$RecommendationTitle <- movie_name
  return(rec)
}

RecommendationList <- do.call(rbind,lapply(1:length(RatedMatrix), function(x){findMatch(RatedMatrix, x, distinct_movies, movies)}))
write.csv(RecommendationList,"RecommendationList.csv",row.names = FALSE)
