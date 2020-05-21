library(dplyr)
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

#take ratings out of train set for now
org_rating <- train_df$rating

train_df$rating = NULL

rating_pre <- preProcess(as.data.frame(org_rating), method = c("center","scale"))
train_y <- predict(rating_pre,newdata = as.data.frame(org_rating))

#fill in missiing values and normalzie and
train_df_pre <- preProcess(train_df, method = c("center","scale","bagImpute"))
train_df_procs <- predict(train_df_pre, newdata = train_df)

# chuck back in correct numbers for ids
train_df_procs$user_id <- train_df$user_id
train_df_procs$item_id <- train_df$item_id

de_norm <- function(norm,org){
  m<- mean(org)
  sd <- sd(org)
  return(sd*norm + m)
}


#save incase we need it later

# saveRDS(train_df, 'prep_train.rds')
# 
# train_df <- readRDS('prep_train.rds')


# #split into training and validation sets
# set.seed(613)
# train_index <- sample(seq_len(nrow(train_df)), size = floor(0.95 * nrow(train_df)))
# train_set <- train_df[train_index, ]
# val_set <-train_df[-train_index, ]



#trun into a one_hot matrix
train_x <- train_df_procs 
rm(train_df_procs)


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
                          allowParallel =T 
                         
                          )              
print("Training model")
start <- Sys.time()

# train the model on training set
rf_model <- train(x = train_x_1h,y = train_y$org_rating, 
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
for(var in as.character(most_imp$var)){
  cluster <- makePSOCKcluster(1, cores = 10)
  registerDoParallel(cluster)

  dat <- pdp::partial(rf_model,pred.var = var,rug = T,train = train_x_1h,
                 type = 'regression',parallel = T)

  stopCluster(cluster)
  dat <- dat %>%
    rename(actual_rating = yhat) %>%
    rename(var_value = var) %>%
    mutate(var = var)
  #un-nro,laize variable so its me interpretable
  dat$var_value  = de_norm(dat$var_value,unlist(as.list(train_df[var])))
  max_pdp <- bind_rows(max_pdp,dat)
}

max_pdp$actual_rating <- de_norm(max_pdp$actual_rating,org_rating)
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

#plot PDP with reveiws

ggplot(max_pdp %>%  filter(var == "reviews"),
       aes(x = var_value,y=actual_rating,color = var),group =1) + geom_line()+
  scale_colour_discrete("Variables")+
  labs(title = "PDP of Reviews",
       x = "Reviews",
       y = "Predicted Rating") +
  theme(legend.position="bottom",legend.direction = "vertical")
unlink(paste(graph_dir,"PDP_review.png",sep = ""))
ggsave(paste(graph_dir,"PDP_review.png",sep = ""))

#remianing variables

ggplot(max_pdp %>%  filter(var  == "timestamp"),
       aes(x = var_value,y=actual_rating,color = var),group =1) + geom_line()+
  scale_colour_discrete("Variables")+
  labs(title = "PDP of Timestamp",
       x = "Date of review",
       y = "Predicted Rating") +
  theme(legend.position="bottom",legend.direction = "vertical")
unlink(paste(graph_dir,"PDP_other.png",sep = ""))
ggsave(paste(graph_dir,"PDP_other.png",sep = ""))


#save(rf_model, file = "models/rf_8_781_384.Rdata")
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





#test_pre <- preProcess(test, method = "medianImpute")

test_procs <- predict(train_df_pre, newdata = test)
# chuck back in correct numbers for ids
test_procs$user_id <- test$user_id
test_procs$item_id <- test$item_id

test_1h <- dummyVars(~., data = test_procs, fullRank = TRUE)
test_1h <- predict(test_1h, newdata = test_procs)

col.order <- colnames(train_x_1h)
test_1h <- test_1h[,col.order]

#dplyr::setdiff(colnames(train_x_1h),colnames(test_1h))
# 
#dplyr::setdiff(colnames(test_1h),colnames(train_x_1h))


test$rating <- de_norm(predict(rf_model, test_1h),org_rating)

preds <- test %>% 
  dplyr::select(c(user_id,item_id,rating)) %>% 
  mutate(user_item = paste(user_id,item_id,sep='_')) %>% 
  dplyr::select(c(rating,user_item))
write.csv(preds,"rf10__predictions.csv",row.names = F)


#make a note about clustering over fitting if it turns out baddly
#utterstupid


# preds <- test %>% 
#   dplyr::select(c(user_id,item_id,m_cluster_rating,u_cluster_rating)) %>% 
#   mutate(rating = (m_cluster_rating+u_cluster_rating)/2) %>% 
#   mutate(user_item = paste(user_id,item_id,sep='_')) %>% 
#   dplyr::select(c(rating,user_item))
# 
# write.csv(preds,"stupid_3_predictions.csv",row.names = F)