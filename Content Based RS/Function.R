rm(list=ls())

#Load library
library(dplyr)

#Read files
ratings <- read.csv(file.choose(),header = TRUE,stringsAsFactors = FALSE)
movies <- read.csv(file.choose(),header = TRUE,stringsAsFactors = FALSE)
tags <- read.csv(file.choose(),header = TRUE,stringsAsFactors = FALSE)

#Calculating the TF value and IDF value and multiplying together to get TF-IDF value
TF <- tags %>% group_by(movieId,tag) %>% summarise(tag_count_TF=n())
TF_unique <- TF[!duplicated(TF[c("movieId","tag")]),]
DF <- TF_unique %>% group_by(tag) %>% summarise(tag_count_DF=n())
DF$IDF <- log10(length(unique(TF_unique$movieId))) - log10(DF$tag_count_DF)
TF_unique <- merge(TF_unique,DF,by="tag",all.x = TRUE,sort=FALSE)[,c(2,1,3,4,5)]
TF_unique$TF_IDF <- TF_unique$tag_count_TF * TF_unique$IDF

#Calculating the unit length vector by dividing TF-IDF value with the vector length of a particular movie.
VectLen <- TF[,c("movieId","TF_IDF")]
VectLen$TFIDF_sq <- VectLen$TF_IDF^2
VectLenSum <- VectLen %>% group_by(movieId) %>% summarise(TFIDF_sq_sum=sum(TFIDF_sq))
VectLenSum$vect_len <- sqrt(VectLenSum$TFIDF_sq_sum)
TF_unique <- merge(TF_unique,VectLenSum,by="movieId",all.x = TRUE,sort=FALSE)
TF_unique$TAG_WT <- TF_unique$TF_IDF/TF_unique$vect_len

#Calculate user profile for each user
Ratings_filter <- ratings %>% filter(rating>=3.5)
unique_users <- unique(ratings$userId)

UserProfile <- function(user, ratingdata, TFdata){
  temprating <- ratingdata %>% filter(userId==user) %>% inner_join(TFdata, by="movieId")%>%  
                group_by(tag) %>% summarize(tag_pref=sum(TAG_WT))
  temprating$User <- rep(user,nrow(temprating))
  return(temprating)
}

user_tag_pref <- do.call(rbind,lapply(unique_users, function(x){UserProfile(x, Ratings_filter, TF_unique)}))

#Calculate the cosine similarity between the two vectors
MovCosSim <- function(movie,TFdata,userpref){
  TF_Movie <- TFdata %>% filter(movieId==movie) %>% left_join(userpref, by="tag") %>%
    mutate_at(vars(tag_pref),funs(replace(., is.na(.), 0))) %>% mutate(tag_value = TAG_WT * tag_pref)
  TAG_WT_val <- sqrt(sum((TF_Movie$TAG_WT)^2))
  tag_pref_val <- sqrt(sum((userpref$tag_pref)^2))
  tag_merge <- TF_Movie %>% group_by(User, movieId) %>% summarize(Rating=sum(tag_value))
  tag_merge$Rating <- tag_merge$Rating/(TAG_WT_val*tag_pref_val)
  return(tag_merge)
}

UserCosSim <- function(user,userdata,TFdata){
  user_tag_pref_all <- userdata %>% filter(User==user)
  unique_movies <- unique(TF$movieId)
  tag_merge <- do.call(rbind, lapply(unique_movies, function(x){MovCosSim(x,TFdata,user_tag_pref_all)}))
  return(tag_merge)
}

TagMerge <- do.call(rbind,lapply(unique_users, function(x){UserCosSim(x,user_tag_pref,TF_unique)}))
TagMerge <- TagMerge %>% na.omit() %>% arrange(User, Rating)
