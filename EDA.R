library(tidyverse)
library(corrplot)
library(ggridges)


old <- readRDS('AT2_df_STUDENT.rds')


genres <- colnames(old)[13:31]

graph_dir = "graphs/"
rm(old)

df <- readRDS('better_train.rds')


#Count Graphs for reviews
for (var in c("favoured","rating","item_imdb_mature_rating")){
  title <- paste("Count of", var)
  ggplot(df,aes_string(x = var)) + geom_bar() +
    labs(x = var,y = "Count") + ggtitle(title)
  unlink(paste(graph_dir,title,".png",sep = ""))
  ggsave(paste(graph_dir,title,".png",sep = ""))
}


ggplot(df,aes(y=user_mean_rating, x= gender)) + geom_boxplot() + facet_wrap(~age_band) + 
  labs(x = "Gender",y= "Mean rating") + ggtitle("Average ratings by\nAge band & Gender")
unlink(paste(graph_dir,"Mean ratings by age_band and gender.png",sep = ""))
ggsave(paste(graph_dir,"Mean ratings by age_band and gender.png",sep = ""))


rating_genre <- df %>% 
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

ggplot((df %>% select(c(State,user_mean_rating,user_id)) %>% distinct()),aes(x=user_mean_rating,y = State)) + geom_density_ridges2() +
  labs(x = var,y = "Count") 
unlink(paste(graph_dir,"state_mean.png",sep = ""))
ggsave(paste(graph_dir,"state_mean.png",sep = ""))
