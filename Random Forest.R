library(dplyr)
library(lubridate)
library(forcats)
library(caret)
library(randomForest)
library(doParallel)
library(parallel)
library(ranger)

# get data from cleaning
train_df <- readRDS('better_train.rds')


train_df <- train_df %>% 
  ungroup() %>% 
  mutate(user_id = as.numeric(user_id)) %>% 
  mutate(item_id = as.numeric(item_id)) %>% 
  mutate(release_date = as.numeric(release_date)) %>% 
  mutate(timestamp = as.numeric(timestamp)) 

#state is not helping much
train_df$State <-NULL #as.factor(train_df$State)

train_df <- train_df %>% mutate_if(is.factor,
                               fct_explicit_na,
                               na_level = "missing") %>% 
                          mutate_if(is.integer,as.numeric)

train_df$older_than_reviewer <- as.factor(train_df$older_than_reviewer)
train_df$older_than_reviewer <- fct_explicit_na(train_df$older_than_reviewer, na_level = "missing")

train_df_pre <- preProcess(train_df, method = "bagImpute")
train_df <- predict(train_df_pre, newdata = train_df)

saveRDS(train_df, 'prep_train.rds')

train_df <- readRDS('prep_train.rds')

train_x <- train_df %>%  select(-c (rating))
train_y <- train_df$rating

train_x_1h <- dummyVars(~., data = train_x, fullRank = TRUE)
train_x_1h <- predict(train_x_1h, newdata = train_x)

cluster <- makePSOCKcluster(1, cores = 10)
registerDoParallel(cluster)

set.seed(923)
#random forest model
rf_control <- trainControl(method = "adaptive_cv",
                          number = 10,
                          repeats = 10,
                          search = 'random',
                          sampling = NULL,
                          verboseIter = F,
                          allowParallel =T ,
                          predictionBounds = c(1,5)
                          )              
start <- Sys.time()

# train the model on training set
rf_model <- train(x = train_x_1h,y = train_y, 
                   # data = train_df,
                   # tree_method = "exact",
                    method = "ranger",
                    trControl = rf_control,
                    tuneLength = 20,
                    metric = 'RMSE',
                    importance = 'impurity',
                    verbose = F
)
end <- Sys.time() - start
print(end)
summary(rf_model)

stopCluster(cluster)

save(rf_model, file = "models/rf_1_783_38.Rdata")

#prep testing model for predictions

test <- readRDS('better_test.rds')
test <- test %>% 
  ungroup() %>% 
  mutate(user_id = as.numeric(user_id)) %>% 
  mutate(item_id = as.numeric(item_id)) %>% 
  mutate(release_date = as.numeric(release_date)) %>% 
  mutate(timestamp = as.numeric(timestamp)) %>% 
  mutate(item_imdb_length = as.numeric(item_imdb_length))


test$State <- NULL #as.factor(test$State)

test <- test %>% mutate_if(is.factor,
                                   fct_explicit_na,
                                   na_level = "missing") %>% 
                                   mutate_if(is.integer,as.numeric)

test$older_than_reviewer <- as.factor(test$older_than_reviewer)
levels(test$older_than_reviewer) <- c("FALSE", "TRUE", "missing")
test_pre <- preProcess(test, method = "bagImpute")
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
write.csv(preds,"rf2_predictions.csv",row.names = F)

