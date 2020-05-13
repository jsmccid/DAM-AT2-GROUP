library(tidyverse)
library(lubridate)
library(forcats)
library(caret)
library(corrplot)
library(ggridges)

# get data from refined file given
train <- readRDS('AT2_train_STUDENT.rds')
test <- readRDS('AT2_test_STUDENT.rds')

raw <- readRDS('train_raw.rds')

scrape <- readRDS('scrape.rds')

genres <- colnames(train)[13:31]

graph_dir = "graphs/"


#merge with zipcodes to get state/place names
ZipCodeSourceFile = "http://download.geonames.org/export/zip/US.zip"
temp <- tempfile()
download.file(ZipCodeSourceFile , temp)
ZipCodes <- read.table(unz(temp, "US.txt"), sep="\t")
unlink(temp)
names(ZipCodes) = c("CountryCode", "zip_code", "PlaceName", 
                    "State", "AdminCode1", "AdminName2", "AdminCode2", 
                    "AdminName3", "AdminCode3", "latitude", "longitude", "accuracy")

ZipCodes <- ZipCodes %>%  
  mutate(State = as.character(State)) %>% 
  mutate(State = if_else(State =="","Military",State)) %>% 
  select(c(zip_code,State)) 
  

#missing zipcodes  and fixes
#https://en.wikipedia.org/wiki/List_of_ZIP_Code_prefixes
bad_zip <-str_split("0  2146  2154  2159  2320  5779  20707  20770 33205 41850 46005 51157 54248 60005 60007 60008 60035 60067  60089 60090 60115 60135 60152 60187 60201 60202 60302 60402 60466 60476 60515 60613 60614 60615 60626 60630  60641 60657 60659 60804 61401 61455 61801 61820 62522 62901 62903 66315 91919 93055"," +")[[1]]    
bad_zip <- as.integer(bad_zip)


bad_state<-case_when(
  bad_zip > 1000 & bad_zip < 2800 ~ 'Massachusetts',
  bad_zip == 5779 ~ 'Vermont',
  bad_zip > 20700 & bad_zip< 20800 ~ 'Maryland',
  bad_zip == 33205 ~ 'Florida',
  bad_zip == 41850 ~ 'Kentucky',
  bad_zip == 46005 ~ 'Indiana',
  bad_zip == 51157 ~ 'Iowa',
  bad_zip == 54248 ~ 'Wisconsin',
  bad_zip > 60000 & bad_zip < 63000 ~ 'Illinois',
  bad_zip > 90000 & bad_zip < 96200 ~ 'California',
  TRUE ~ 'Unknown'
)

bad_df <- data.frame(
  zip_code = bad_zip,
  State = bad_state
)
ZipCodes <- bind_rows(ZipCodes,bad_df)



train <- train %>% 
  mutate(zip_code = as.integer(as.character(zip_code))) %>% 
  left_join(ZipCodes) %>%
  mutate(State = if_else(is.na(State),"Unknown",State)) %>%
  select(-zip_code)


# Mean rating per User
user_means <- train %>% 
  group_by(user_id) %>% 
  summarise(user_mean_rating = mean(rating))

#Check if movie was released before reviwers (mild assumption that age is accurate in June 1998)
train <- train %>% 
  mutate(older_than_reviewer = (1998 -age - year(release_date)) >= 0) %>% 
  mutate(age_diff = abs(1998 -age - year(release_date)))

# - User Age band/Gener ratings per item
user_age_gender_means <- train %>% 
  group_by(age_band,gender, item_id) %>% 
  summarise(user_age_band_gender_item_mean_rating = mean(rating)) %>% 
  ungroup() 


#Get most watched genre of movie and comapre it to movie being revied

#first get genres of movie
genre_tab<-train %>% 
  gather(genre,is_off,unknown:western) %>% 
  filter(is_off == T) %>% 
  select(c(item_id,movie_title,user_id,genre))

#then get most reviwed genres of each user
most_favoured<-train %>% 
  group_by(user_id) %>% 
  summarise_if(is.logical,sum) %>% 
  gather(most_reviewed,count,unknown:western) %>%
  group_by(user_id) %>% 
  filter(count == max(count)) %>% 
  select(user_id,most_reviewed) 

#finally, join to train set
train <- most_favoured%>% 
  inner_join(genre_tab, by = c('user_id' = 'user_id')) %>% 
  mutate(favoured = (genre == most_reviewed)) %>% 
  group_by(user_id,item_id) %>% 
  summarise(favoured = any(favoured == T)) %>% 
  full_join(train,by = c('user_id' = 'user_id','item_id' = 'item_id'))

#See if staf prefer thsi movie over teh population and also join everything
train <-train %>% 
  mutate(staff_prefered = !is.na(item_imdb_staff_average) &(item_mean_rating < item_imdb_staff_average))%>% 
  inner_join(user_means, by = c('user_id' = 'user_id'))%>% 
  inner_join(user_age_gender_means, by = c('item_id','age_band','gender'))

#general clean up
train <- train %>% select(-c(movie_title,video_release_date,imdb_url))

#repeating process for test data
genre_tab_test<-test %>% 
  gather(genre,is_off,unknown:western) %>% 
  filter(is_off == T) %>% 
  select(c(item_id,movie_title,user_id,genre))

test <- most_favoured%>% 
  inner_join(genre_tab_test, by = c('user_id' = 'user_id')) %>% 
  mutate(favoured = (genre == most_reviewed)) %>% 
  group_by(user_id,item_id) %>% 
  summarise(favoured = any(favoured == T)) %>% 
  full_join(test,by = c('user_id' = 'user_id','item_id' = 'item_id'))%>% 
  mutate(staff_prefered = !is.na(item_imdb_staff_average) &(item_mean_rating < item_imdb_staff_average))%>% 
  mutate(older_than_reviewer = (1998 -age - year(release_date)) >= 0) %>% 
  mutate(age_diff = abs(1998 -age - year(release_date))) %>% 
  left_join(user_means, by = c('user_id' = 'user_id'))%>% 
  left_join(user_age_gender_means, by = c('item_id','age_band','gender'))

test <- test %>% 
  mutate(zip_code = as.integer(as.character(zip_code))) %>% 
  left_join(ZipCodes) %>% 
  mutate(State = if_else(is.na(State),"Unknown",State)) %>%
  select(-zip_code)

test <- test %>% select(-c(movie_title,video_release_date,imdb_url))

#Count Graphs for users
for (var in c("age","gender","occupation","age_band")){
  title <- paste("Count of", var)
  ggplot(distinct(train[c("age","gender","occupation","age_band")]),aes_string(x = var)) + geom_bar() +
    labs(x = var,y = "Count") + ggtitle(title)
  unlink(paste(graph_dir,title,".png",sep = ""))
  ggsave(paste(graph_dir,title,".png",sep = ""))
}

ggplot(distinct(train[c("gender","occupation","age_band","user_mean_rating")]),aes(x=user_mean_rating)) + geom_density() + facet_wrap(~occupation)+
  labs(x = "User mean rating",y = "Count") + ggtitle("Mean ratings density\nby Occupation")
unlink(paste(graph_dir,"Mean ratings density by Occupation.png",sep = ""))
ggsave(paste(graph_dir,"Mean ratings density by Occupation.png",sep = ""))

#Count Graphs for reviews
for (var in c("favoured","rating","item_imdb_mature_rating")){
  title <- paste("Count of", var)
  ggplot(train,aes_string(x = var)) + geom_bar() +
    labs(x = var,y = "Count") + ggtitle(title)
  unlink(paste(graph_dir,title,".png",sep = ""))
  ggsave(paste(graph_dir,title,".png",sep = ""))
}


ggplot(train,aes(y=user_mean_rating, x= gender)) + geom_boxplot() + facet_wrap(~age_band) + 
  labs(x = "Gender",y= "Mean rating") + ggtitle("Average ratings by\nAge band & Gender")
unlink(paste(graph_dir,"Mean ratings by age_band and gender.png",sep = ""))
ggsave(paste(graph_dir,"Mean ratings by age_band and gender.png",sep = ""))


rating_genre <- train %>% 
  gather(genre,is_off,unknown:western) %>% 
  filter(is_off == T) %>% 
  mutate(rating = as.factor(rating)) %>% 
  select(-is_off)


ggplot(rating_genre ,aes(x=item_imdb_rating_of_ten,y=(item_mean_rating*2),colour= rating)) + geom_point() +
  labs(x = "IMDB mean rating",y="Movie mean Rating") + ggtitle("Population mean rating")+ 
  scale_color_discrete(name= 'User ratings')
unlink(paste(graph_dir,"Mean ratings compared to IMDB.png",sep = ""))
ggsave(paste(graph_dir,"Mean ratings compared to IMDB.png",sep = ""))

i <- 2
while(i <= length(genres)){
  title <- paste(genres[i:(i+3)],collapse =",")
  g <- ggplot(rating_genre %>% filter(genre %in% genres[i:(i+3)]) ,aes(x=item_imdb_rating_of_ten,y=(item_mean_rating*2),colour= rating)) + geom_point() +
    labs(x = "IMDB mean rating",y="Movie mean Rating") + ggtitle(title)+  facet_wrap(~genre)+
    scale_color_discrete(name= 'User ratings')
  print(g)
  unlink(paste(graph_dir,title,".png",sep = ""))
  ggsave(paste(graph_dir,title,".png",sep = ""))
  i= i + 4
}

ggplot((train %>% select(c(State,user_mean_rating,user_id)) %>% distinct()),aes(x=user_mean_rating,y = State)) + geom_density_ridges2() +
  labs(x = var,y = "Count") 
unlink(paste(graph_dir,"state_mean.png",sep = ""))
ggsave(paste(graph_dir,"state_mean.png",sep = ""))


saveRDS(train, 'better_train.rds')
saveRDS(test, 'better_test.rds')



