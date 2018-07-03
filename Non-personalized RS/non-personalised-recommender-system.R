rm(list=ls())
#Libraries
library(dplyr)
library(tibble)

#Read in files
ratings <- read.csv(file.choose(),header = TRUE)
movies <- read.csv(file.choose(),header = TRUE)
tags <- read.csv(file.choose(),header = TRUE)

#Mean rating of each movie
meanratings <- ratings %>% group_by(movieId) %>% dplyr::summarize(MeanRating = round(mean(rating, na.rm=TRUE),2)) 

alpha <- 5
#Calculating damped mean using alpha = 5
sumratings <- ratings %>% group_by(movieId) %>% dplyr::summarize(SumRating = round(sum(rating, na.rm=TRUE),2)) 
sumratings$sumratingfactors <- sumratings$SumRating + alpha*mean(ratings$rating, na.rm = TRUE)

countratings <- ratings %>% group_by(movieId) %>% dplyr::summarize(CountRating = n()) 
countratings$countratingfactors <- countratings$CountRating+alpha

dampedratings <- merge(x = sumratings, y = countratings, by="movieId")
dampedratings$dampedmean <- round(dampedratings$sumratingfactors/dampedratings$CountRating,2)

ratings_mean_dampmean <- meanratings
ratings_mean_dampmean$dampedmean <- dampedratings$dampedmean 

#Rank each movie
ratings_mean_dampmean <- ratings_mean_dampmean %>% mutate(MeanRank = dense_rank(desc(MeanRating)), DampedMeanRank = dense_rank(desc(dampedmean))) 
top50movies <- head(arrange(ratings_mean_dampmean[,c("movieId","dampedmean","DampedMeanRank")],DampedMeanRank),50)
top50movies <- add_column(top50movies, Title=sapply(top50movies$movieId, function(x){as.character(movies$title[which(movies$movieId==x)])}), .after = "movieId")
write.csv(top50movies,"Top50MoviesRank.csv", row.names = FALSE)
