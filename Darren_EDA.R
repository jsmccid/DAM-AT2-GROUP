library(tidyverse)
library(ggridges)


old <- readRDS('AT2_train_STUDENT.rds')


genres <- colnames(old)[13:31]

graph_dir = "graphs/"
rm(old)

df <- readRDS('better_train.rds')

df <- df %>% 
  mutate(age_band = as.character(age_band))


df$age_band[df$age_band == "under_18"]= "< 18 yrs"
df$age_band[df$age_band == "18_to_29"]= "18-29 yrs"
df$age_band[df$age_band == "30_to_44"]= "30-44 yrs"
df$age_band[df$age_band == "45_and_over"]= "> 45 yrs"



#Count Graphs for reviews
for (var in c("favoured","rating","item_imdb_mature_rating")){
  title <- paste("Count of", var)
  ggplot(df,aes_string(x = var)) + geom_bar() +
    labs(x = var,y = "Count") + ggtitle(title)
  unlink(paste(graph_dir,title,".svg",sep = ""))
  ggsave(paste(graph_dir,title,".svg",sep = ""), device = "svg")
}

#check user base in more detal
user_base <- df %>% 
  select(user_id,age_band,occupation,gender,State) %>% 
  distinct()

ggplot(user_base,aes(x = gender)) + geom_bar() +
  facet_wrap(age_band~.)+
  labs(title = "Counts of users by age band and gender",x = 'Gender',y = "Count") 
unlink(paste(graph_dir,"Counts of users by age_band and gender.svg",sep = ""))
ggsave(paste(graph_dir,"Counts of users by age_band and gender.svg",sep = ""), device = "svg")

#check user ratings in more detail
ggplot(df %>% 
         select(c(user_id,age_band,user_mean_rating,gender)) %>%
       distinct(),aes(y=user_mean_rating, x= gender)) + geom_boxplot() + facet_wrap(~age_band) + 
  labs(x = "Gender",y= "Mean rating") + ggtitle("Average ratings by\nAge band & Gender")
unlink(paste(graph_dir,"Mean ratings by age_band and gender.svg",sep = ""))
ggsave(paste(graph_dir,"Mean ratings by age_band and gender.svg",sep = ""), device = "svg")


rating_genre <- df %>% 
  gather(genre,is_off,unknown:western) %>% 
  filter(is_off == T) %>% 
  mutate(rating = as.factor(rating)) %>% 
  select(-is_off)

#check if reatungs corelated with means
ggplot(rating_genre  ,aes(x=item_imdb_rating_of_ten,y=(item_mean_rating*2),colour= rating)) + geom_point() +
  labs(x = "IMDB mean rating",y="Movie mean Rating") + ggtitle("Population mean rating")+ 
  scale_color_discrete(name= 'User ratings')
unlink(paste(graph_dir,"Mean ratings compared to IMDB.svg",sep = ""))
ggsave(paste(graph_dir,"Mean ratings compared to IMDB.svg",sep = ""), device = "svg")


#check ths=is over all genres by gender
i <- 2
while(i <= length(genres)){
  title <- paste(genres[i:(i+3)],collapse =",")
  g <- ggplot(rating_genre %>% filter(genre %in% genres[i:(i+3)]) ,aes(x=item_imdb_rating_of_ten,y=item_mean_rating,colour= gender)) + geom_point() +
    labs(x = "IMDB mean rating",y="Movie mean Rating") + ggtitle(title)+  facet_wrap(~genre)+
    scale_color_discrete(name= 'User ratings') + theme(legend.position = "bottom")
  print(g)
  unlink(paste(graph_dir,title,"_gender.svg",sep = ""))
  ggsave(paste(graph_dir,title,"_gender.svg",sep = ""), device = "svg")
  i= i + 4
}

#check ths=is over all genres by rating
i <- 2
while(i <= length(genres)){
  title <- paste(genres[i:(i+3)],collapse =",")
  g <- ggplot(rating_genre %>% filter(genre %in% genres[i:(i+3)]) ,aes(x=item_imdb_rating_of_ten,y=item_mean_rating,colour= rating)) + geom_point() +
    labs(x = "IMDB mean rating",y="Movie mean Rating") + ggtitle(title)+  facet_wrap(~genre)+
    scale_color_discrete(name= 'User ratings') + theme(legend.position = "bottom")
  print(g)
  unlink(paste(graph_dir,title,"_rating.png",sep = ""))
  ggsave(paste(graph_dir,title,"_rating.png",sep = ""), device = "svg")
  i= i + 4
}


#check mean ratings by genre in core and non core group

ggplot(rating_genre %>%
         filter(age >= 18 & age < 45 & gender== "M") %>% 
         select(c(item_imdb_rating_of_ten,user_age_band_item_imdb_mean_rating,genre)) %>% 
         distinct(),aes(x=item_imdb_rating_of_ten,y=user_age_band_item_imdb_mean_rating)) + 
    geom_point() + geom_jitter()+ facet_wrap(.~genre)+
  labs(x = "IMDB mean rating",y="Targeted group mean Rating") + ggtitle("Mean ratings in Males 18-44")+ 
  scale_color_discrete(name= 'Genres')
unlink(paste(graph_dir,"Mean ratings in Males 18-44.png",sep = ""))
ggsave(paste(graph_dir,"Mean ratings in Males 18-44.png",sep = ""), device = "svg")


ggplot(rating_genre %>%
         filter(age < 18 | age > 45 | gender== "F") %>% 
         select(c(item_imdb_rating_of_ten,user_gender_age_band_item_imdb_mean_rating,genre)) %>% 
         distinct(),aes(x=item_imdb_rating_of_ten, y=user_gender_age_band_item_imdb_mean_rating)) + 
  geom_point() + geom_jitter()+ facet_wrap(.~genre)+
  labs(x = "IMDB mean rating",y="Targeted group mean Rating") + ggtitle("Mean ratings not in Males 18-44")+ 
  scale_color_discrete(name= 'Genres')
unlink(paste(graph_dir,"Mean ratings not in Males 18-44.png",sep = ""))
ggsave(paste(graph_dir,"Mean ratings not in Males 18-44.png",sep = ""), device = "svg")

#check ratoign means by state
ggplot((df %>% select(c(State,user_mean_rating,user_id)) %>% distinct()),aes(x=user_mean_rating,y = State)) + geom_density_ridges2() +
  labs(title = "State average ratings",x = "Rating",y = "") 
unlink(paste(graph_dir,"state_mean.png",sep = ""))
ggsave(paste(graph_dir,"state_mean.png",sep = ""), device = "svg")


ggplot(df %>% select(item_imdb_length,item_mean_rating) %>% distinct(),aes(x=item_imdb_length,y = item_mean_rating)) + geom_point() +
  labs(x = "Runtime",y = "Rating") 
unlink(paste(graph_dir,"run_rate.png",sep = ""))
ggsave(paste(graph_dir,"run_rate.png",sep = ""), device = "svg")


ggplot(rating_genre %>%
         select(item_id,item_mean_rating,item_imdb_top_1000_voters_average,genre) %>% 
         filter(genre != 'unknown') %>% 
         distinct()
       ,aes(y = item_mean_rating,x = item_imdb_top_1000_voters_average,colour = genre)) + 
  geom_point() + geom_jitter()+
  labs(y = "Item _mean_rating",x = "IMDB_votes") + scale_color_discrete(name= 'Genres')
unlink(paste(graph_dir,"rating_votes.png",sep = ""))
ggsave(paste(graph_dir,"rating_votes.png",sep = ""))

#check rating measn by mature rating
ggplot((df %>% select(c(item_imdb_mature_rating,user_mean_rating,user_id)) %>% distinct()),aes(x=user_mean_rating,y = item_imdb_mature_rating)) +
  geom_density_ridges2() +
  labs(title = "Mature average ratings",x = "Rating",y = "") 
unlink(paste(graph_dir,"mature_mean.png",sep = ""))
ggsave(paste(graph_dir,"mature_mean.png",sep = ""))


