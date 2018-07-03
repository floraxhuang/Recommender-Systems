import pandas as pd
import numpy as np
import math

Ratings=pd.read_csv("C:/Users/Flora Huang/Documents/GitHub/Recommender System/Content Based Recommender/Data/ratings.csv")
Movies=pd.read_csv("C:/Users/Flora Huang/Documents/GitHub/Recommender System/Content Based Recommender/Data/movies.csv", encoding = "ISO-8859-1")
Tags=pd.read_csv("C:/Users/Flora Huang/Documents/GitHub/Recommender System/Content Based Recommender/Data/tags.csv", encoding = "ISO-8859-1")

#Calculating the TF value and IDF value and multiplying together to get TF-IDF value
TF= Tags.groupby(["movieId","tag"], as_index = False, sort = False).count().rename(columns = {"userId": "tag_count_TF"})[["movieId","tag","tag_count_TF"]]
Tag_distinct = Tags[["tag","movieId"]].drop_duplicates()
DF =Tag_distinct.groupby(["tag"], as_index = False, sort = False).count().rename(columns = {"movieId": "tag_count_DF"})[["tag","tag_count_DF"]]
a=math.log10(len(np.unique(Tags["movieId"])))
DF["IDF"] = a - np.log10(DF["tag_count_DF"])
TF = pd.merge(TF,DF,on = "tag", how = "left", sort = False)
TF["TF-IDF"]=TF["tag_count_TF"]*TF["IDF"]

#Calculating the unit length vector by dividing TF-IDF value with the vector length of a particular movie.
Vect_len=TF[["movieId","TF-IDF"]].copy()
Vect_len["TF-IDF-Sq"] = Vect_len["TF-IDF"]**2
Vect_len = Vect_len.groupby(["movieId"], as_index = False, sort = False).sum().rename(columns = {"TF-IDF-Sq": "TF-IDF-Sq-sum"})[["movieId","TF-IDF-Sq-sum"]]
Vect_len["vect_len"] = np.sqrt(Vect_len[["TF-IDF-Sq-sum"]].sum(axis=1))
TF = pd.merge(TF,Vect_len,on = "movieId", how = "left", sort = False)
TF["TAG_WT"]=TF["TF-IDF"]/TF["vect_len"]

# calculate user profile for each user
Ratings_filter=Ratings[Ratings["rating"]>=3.5]
distinct_users=np.unique(Ratings["userId"])

#only doing one cal
user_tag_pref=pd.DataFrame()
i=1
for user in distinct_users[0:1]:
 user_data = Ratings_filter[Ratings_filter["userId"]==user]
 user_data = pd.merge(TF,user_data,on = "movieId", how = "inner", sort = False)
 user_data1 = user_data.groupby(["tag"], as_index = False, sort = False).sum().rename(columns = {"TAG_WT": "tag_pref"})[["tag","tag_pref"]]
 user_data1["user"]=user
 user_tag_pref = user_tag_pref.append(user_data1, ignore_index=True)
 i=i+1
 
#calculate the cosine similarity between the two vectors.
tag_merge_all=pd.DataFrame()

i=1
for user in distinct_users[0:1]:
 user_tag_pref_all= user_tag_pref[user_tag_pref["user"]==user]
 distinct_movies = np.unique(TF["movieId"])
 j=1
 for movie in distinct_movies:
    TF_Movie= TF[TF["movieId"]==movie]
    tag_merge = pd.merge(TF_Movie,user_tag_pref_all,on = "tag", how = "left", sort = False)
    tag_merge["tag_pref"]=tag_merge["tag_pref"].fillna(0)
    tag_merge["tag_value"]=tag_merge["TAG_WT"]*tag_merge["tag_pref"]
    TAG_WT_val=np.sqrt(np.sum(np.square(tag_merge["TAG_WT"]), axis=0))
    tag_pref_val=np.sqrt(np.sum(np.square(user_tag_pref_all["tag_pref"]), axis=0))
    tag_merge_final = tag_merge.groupby(["user","movieId"])[["tag_value"]].sum().rename(columns = {"tag_value": "Rating"}).reset_index()
 
    tag_merge_final["Rating"]=tag_merge_final["Rating"]/(TAG_WT_val*tag_pref_val)
 
    tag_merge_all = tag_merge_all.append(tag_merge_final, ignore_index=True)
    j=j+1
 i=i+1
tag_merge_all=tag_merge_all.sort_values(by=["user","Rating"]).reset_index()


