library(dplyr)
library(lubridate)
library(forcats)
library(caret)
library(randomForest)
library(doParallel)
library(parallel)
library(ranger)
library(pdp)

# get data from cleaning
train_df <- readRDS('better_train.rds')


train_df <- train_df %>% 
  ungroup() %>% 
  mutate(user_id = as.numeric(user_id)) %>% 
  mutate(item_id = as.numeric(item_id)) %>% 
  mutate(release_date = as.numeric(release_date)) %>% 
  mutate(timestamp = as.numeric(timestamp)) 

#state   and unknown moovies genres are not helping much
train_df$State <- NULL #as.factor(train_df$State)
train_df$unknown <- NULL
train_df$item_imdb_mature_rating <- NULL 


train_df <- train_df %>% mutate_if(is.factor,
                               fct_explicit_na,
                               na_level = "missing") %>% 
                          mutate_if(is.integer,as.numeric)

train_df$older_than_reviewer <- as.factor(train_df$older_than_reviewer)
train_df$older_than_reviewer <- fct_explicit_na(train_df$older_than_reviewer, na_level = "missing")

train_df_pre <- preProcess(train_df, method = "medianImpute")
train_df <- predict(train_df_pre, newdata = train_df)






saveRDS(train_df, 'prep_train.rds')

train_df <- readRDS('prep_train.rds')


# #split into training and validation sets
# set.seed(613)
# train_index <- sample(seq_len(nrow(train_df)), size = floor(0.95 * nrow(train_df)))
# train_set <- train_df[train_index, ]
# val_set <-train_df[-train_index, ]

#trun into a one_hot matrix
train_x <- train_df %>%  select(-c (rating))
train_y <- train_df$rating

train_x_1h <- dummyVars(~., data = train_x, fullRank = TRUE)
train_x_1h <- predict(train_x_1h, newdata = train_x)

cluster <- makePSOCKcluster(1, cores = 10)
registerDoParallel(cluster)

set.seed(923)
#random forest model
rf_control <- trainControl(method = "cv",
                          number = 10,
                          #repeats = 4,
                          search = 'random',
                          sampling = NULL,
                          verboseIter = F,
                          allowParallel =T ,
                          predictionBounds = c(1,5)
                         
                          )              
print("Training model")
start <- Sys.time()

# train the model on training set
rf_model <- train(x = train_x_1h,y = train_y, 
                   # data = train_df,
                   # tree_method = "exact",
                    method = "ranger",
                    trControl = rf_control,
                    tuneLength = 5,
                    metric = 'RMSE',
                    num.trees = 100,
                    importance = 'impurity',
                    verbose = F
)

end <- Sys.time() - start
print(end)
stopCluster(cluster)

# val_x <- val_set %>%  select(-c (rating))
# 
# val_x_1h <- dummyVars(~., data = val_x, fullRank = TRUE)
# val_x_1h <- predict(val_x_1h, newdata = val_x)
# 
# 
# val_set$pred_rating <- predict(rf_model, val_x_1h)
# 
# print("Val RMSE is:")
# 
# print(RMSE(val_set$rating,val_set$pred_rating))


graph_dir = "graphs/"

#Get 10 most important variables
imp <-rf_model$finalModel$variable.importance
imp<-data.frame(
  var = names(imp),
  Imp = imp,
  row.names = NULL
)

most_imp <-  imp %>%  top_n(10,Imp)

ggplot(most_imp,aes(y = reorder(var,Imp),x= Imp)) + geom_col() + 
  labs(title = "Importance of 10\nmost important",
       x = "Importance",
       y= "")
unlink(paste(graph_dir,"Important 10.png",sep = ""))
ggsave(paste(graph_dir,"Important 10.png",sep = ""))


max_pdp <- NULL
#PDP graphs for siad important variables
print("PDP graphing")
start <- Sys.time()
for(var in most_imp$var){
  cluster <- makePSOCKcluster(1, cores = 10)
  registerDoParallel(cluster)
  
  dat <- pdp::partial(rf_model,pred.var = var,rug = T,train = train_x_1h,
                 type = 'regression',parallel = T)
  
  stopCluster(cluster)
  dat <- dat %>%
    rename(actual_rating = yhat) %>% 
    rename(var_value = var) %>% 
    mutate(var = var) 
  max_pdp <- bind_rows(max_pdp,dat)
}
end <- Sys.time() - start
print(end)
#get max values of variables
max_pdp<-max_pdp %>% 
  group_by(var) %>% 
  summarise(max_val = max(var_value)) %>%
  full_join(max_pdp, by = "var") 
  
#plot PDPs with virables with range 1-5

ggplot(max_pdp %>%  filter(max_val <= 5),
       aes(x = var_value,y=actual_rating,color = var),group =1) + geom_line()+
  scale_colour_discrete("Variables")+
  labs(title = "PDP of variables\nof range 1-5",
         x = "Value of Variable",
         y = "Predicted Rating") +
  theme(legend.position="bottom",legend.direction = "vertical")
unlink(paste(graph_dir,"PDP_5.png",sep = ""))
ggsave(paste(graph_dir,"PDP_5.png",sep = ""))

#plot PDPs with virables with range 1-10

ggplot(max_pdp %>%  filter(max_val > 5),
       aes(x = var_value,y=actual_rating,color = var),group =1) + geom_line()+
  scale_colour_discrete("Variables")+
  labs(title = "PDP of variables\nof range 1-10",
       x = "Value of Variable",
       y = "Predicted Rating") +
  theme(legend.position="bottom",legend.direction = "vertical")
unlink(paste(graph_dir,"PDP_10.png",sep = ""))
ggsave(paste(graph_dir,"PDP_10.png",sep = ""))



save(rf_model, file = "models/rf_8_781_384.Rdata")
#load("models/rf_6_790_380.Rdata")

#prep testing model for predictions

test <- readRDS('better_test.rds')
test <- test %>% 
  ungroup() %>% 
  mutate(user_id = as.numeric(user_id)) %>% 
  mutate(item_id = as.numeric(item_id)) %>% 
  mutate(release_date = as.numeric(release_date)) %>% 
  mutate(timestamp = as.numeric(timestamp)) %>% 
  mutate(item_imdb_length = as.numeric(item_imdb_length))


test$State <- NULL #as.factor(train_df$State)
test$unknown <- NULL
test$item_imdb_mature_rating <- NULL 


test$older_than_reviewer <- as.factor(test$older_than_reviewer)
levels(test$older_than_reviewer) <- c("FALSE", "TRUE", "missing")

test <- test %>% mutate_if(is.factor,
                                   fct_explicit_na,
                                   na_level = "missing") %>% 
                                   mutate_if(is.integer,as.numeric)




test_pre <- preProcess(test, method = "medianImpute")
test <- predict(test_pre, newdata = test)

test_1h <- dummyVars(~., data = test, fullRank = TRUE)
test_1h <- predict(test_1h, newdata = test)

col.order <- colnames(train_x_1h)
test_1h <- test_1h[,col.order]

#dplyr::setdiff(colnames(train_x_1h),colnames(test_1h))
# 
#dplyr::setdiff(colnames(test_1h),colnames(train_x_1h))


test$rating <- predict(rf_model, test_1h)

preds <- test %>% 
  dplyr::select(c(user_id,item_id,rating)) %>% 
  mutate(user_item = paste(user_id,item_id,sep='_')) %>% 
  dplyr::select(c(rating,user_item))
write.csv(preds,"rf9__predictions.csv",row.names = F)


#make a note about clustering over fitting if it turns out baddly
#utterstupid


# preds <- test %>% 
#   dplyr::select(c(user_id,item_id,m_cluster_rating,u_cluster_rating)) %>% 
#   mutate(rating = (m_cluster_rating+u_cluster_rating)/2) %>% 
#   mutate(user_item = paste(user_id,item_id,sep='_')) %>% 
#   dplyr::select(c(rating,user_item))
# 
# write.csv(preds,"stupid_3_predictions.csv",row.names = F)