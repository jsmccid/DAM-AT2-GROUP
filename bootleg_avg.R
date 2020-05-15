library(dplyr)
library(caret)



load("models/lasso_0_903_38.Rdata")
load("models/lm_0_879_94.Rdata")
load("models/rf_0_782_38.Rdata")

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

train_x <- train_df %>%  select(-c (rating))
train_y <- train_df$rating

train_x_1h <- dummyVars(~., data = train_x, fullRank = TRUE)
train_x_1h <- predict(train_x_1h, newdata = train_x)

ls_preds <- predict(toy_model, train_x_1h)
lm_preds <- predict(lm_model, train_x_1h)
rf_preds <- predict(rf_model, train_x_1h)



agreate <- data.frame(
  ls_preds = ls_preds,
  lm_preds = lm_preds,
  rf_preds = rf_preds,
  rating = train_df$rating
)

agreate <- agreate %>% 
  mutate(avg = (ls_preds+lm_preds+rf_preds)/3)

#split into training and validation sets
set.seed(212)
train_index <- sample(seq_len(nrow(agreate)), size = floor(0.95 * nrow(agreate)))
agre_train <- agreate[train_index, ]
agre_val <-agreate[-train_index, ]

cluster <- makePSOCKcluster(1, cores = 10)
registerDoParallel(cluster)
set.seed(923)
#Linear Model
avg_control <- trainControl(method = "cv",
                           number = 10,
                           verboseIter = F,
                           allowParallel =T ,
                           predictionBounds = c(1,5),
                           search = 'random',
                           adaptive = list(min =2, alpha = 0.05, 
                                           method = "gls", complete = TRUE) 
)              
start <- Sys.time()

# train the model on training set
avg_model <- train(log(rating) ~.,data = agre_train,
                  method = "lm",
                  trControl = avg_control,
                  tuneGrid  = expand.grid(intercept = F),
                  metric = 'RMSE',
                  verbose = T
)
end <- Sys.time() - start
stopCluster(cluster)
print(end)
summary(avg_model)

qqnorm(avg_model$finalModel$residuals, pch = 1, frame = FALSE)
qqline(avg_model$finalModel$residuals, col = "steelblue", lwd = 2)

agre_val

print(rmse(agre_val$rating,agre_val$avg))







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


test$rating <- predict(avg_model, test_1h)

preds <- test %>% 
  dplyr::select(c(user_id,item_id,rating)) %>% 
  mutate(user_item = paste(user_id,item_id,sep='_')) %>% 
  dplyr::select(c(rating,user_item))
write.csv(preds,"avg2_predictions.csv",row.names = F)
