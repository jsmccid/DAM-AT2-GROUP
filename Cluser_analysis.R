library(tidyverse)
#library(lubridate)
#library(forcats)
library(caret)

#check for best clusering options
#best in thsi case is one with lowest RMSE between cluster averages and actual ratings


train <- readRDS('AT2_train_STUDENT.rds')
raw <- readRDS('train_raw.rds')
graph_dir = "graphs/"

scrape <- readRDS('scrape.rds')

train_stripped <- train %>% 
  select(c(user_id,item_id,rating))


mean_rating<-mean(train$rating)

max <- 1000

##Movie clustering
movies <- raw %>% 
  select(-c(user_id,age,gender,zip_code,occupation,rating,timestamp,movie_title,video_release_date,imdb_url)) %>% 
  left_join(scrape,by = c("item_id" = "movie_id")) %>% 
  distinct() %>% 
  select(-c(url,movie_code)) %>% 
  mutate_if(is.logical,as.factor) %>% 
  mutate(item_id = as.factor(item_id))%>% 
  mutate(mature_rating = as.factor(mature_rating))%>% 
  mutate(mature_rating = as.factor(mature_rating))%>% 
  mutate(release_date = as.numeric(release_date)) %>% 
  mutate_if(is.integer,as.numeric)

#preporcess before cluster
movie_pre <- preProcess(movies, method = "medianImpute")
movies <- predict(movie_pre, newdata = movies)

movies_dum <- dummyVars(~.,data = movies %>% select(-item_id), fullRank = T)
movies_matrix <- predict(movies_dum, newdata = movies %>% select(-item_id))

m_errors <- NULL
#then cluster into i groups get averages of movies and see if they match up
for(i in 1:max){
  move_clus <- kmeans(movies_matrix,i,nrow(movies))
  movie_test <- movies %>%
    select(item_id)
  movie_test$m_cluster <- move_clus$cluster
  test_m <- train_stripped %>% 
    full_join(movie_test,by = "item_id")
  
  
  m_ratings <- test_m %>% 
    select(c(user_id,m_cluster,rating)) %>% 
    group_by(user_id,m_cluster) %>% 
    summarise(m_cluster_rating = mean(rating)) %>% 
    ungroup() 
  test_m <-test_m %>% 
    left_join(m_ratings,by = c("user_id", "m_cluster")) %>% 
    mutate(m_cluster_rating = if_else(is.na(m_cluster_rating),mean_rating,m_cluster_rating))
  rmse <- RMSE(test_m$rating,test_m$m_cluster_rating)
  m_errors <- bind_rows(m_errors,data.frame(
    clusters = i,
    RMSE = rmse
  ))
}


g <- ggplot(m_errors,aes(x = clusters, y = RMSE))+ geom_line() +
  ggtitle("Movies clustering RMSE") 

print(g)
unlink(paste(graph_dir,"Movies clustering RMSE.png",sep = ""))
ggsave(paste(graph_dir,"Movies clustering RMSE.png",sep = ""))


u_errors <- NULL
###USER cluster
users <- raw %>% 
  select(c(user_id:zip_code)) %>% 
  distinct() %>% 
  mutate(occupation = as.factor(occupation)) %>% 
  mutate(zip_code = as.factor(zip_code)) %>% 
  mutate(user_id = as.factor(user_id))

#preporcess fore cluster
users_pre <- preProcess(users, method = "medianImpute")
users <- predict(users_pre, newdata = users)

users_dum <- dummyVars(~.,data = users %>% select(-user_id), fullRank = T)
users_matrix <- predict(users_dum, newdata = users %>% select(-user_id))
for(i in 1:max){
  users_clus <- kmeans(users_matrix,i,nrow(users))
  user_test <- users %>%
    select(user_id)
  user_test$u_cluster <- users_clus$cluster
  test_u <- train_stripped %>% 
    full_join(user_test, by = "user_id")
  
  
  u_ratings <- test_u %>% 
    select(c(item_id,u_cluster,rating)) %>% 
    group_by(item_id,u_cluster) %>% 
    summarise(u_cluster_rating = mean(rating)) %>% 
    ungroup() 
  test_u <-test_u %>% 
    left_join(u_ratings,by = c("item_id", "u_cluster")) %>% 
    mutate(u_cluster_rating = if_else(is.na(u_cluster_rating),mean_rating,u_cluster_rating))
  rmse <- RMSE(test_u$rating,test_u$u_cluster_rating)
  u_errors <- bind_rows(u_errors,data.frame(
    clusters = i,
    RMSE = rmse
  ))
}


g <- ggplot(u_errors,aes(x = clusters, y = RMSE))+ geom_line() +
      ggtitle("Users clustering RMSE") 

print(g)

unlink(paste(graph_dir,"Users clustering RMSE.png",sep = ""))
ggsave(paste(graph_dir,"Users clustering RMSE.png",sep = ""))







