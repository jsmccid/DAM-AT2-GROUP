library(caret)
library(dplyr)
library(forcats)

better_train <-readRDS("better_train.rds")
# str(movie_data)
# summary(movie_data)
# names(movie_data)

better_train$user_id <- as.numeric(better_train$user_id)
better_train$item_id <- as.numeric(better_train$item_id)
better_train$release_date <- as.numeric(better_train$release_date) 
better_train$timestamp <- as.numeric(better_train$timestamp)
better_train$State <- as.factor(better_train$State)
better_train <- better_train %>% mutate_if(is.factor,
                                           fct_explicit_na,
                                           na_level = "missing")
better_train$older_than_reviewer <- as.factor(better_train$older_than_reviewer)
better_train$older_than_reviewer <- fct_explicit_na(better_train$older_than_reviewer, na_level = "missing")
better_train_pre <- preProcess(better_train, method = "medianImpute")

nas <- better_train %>% 
  filter_all(any_vars(is.na(.)))

better_train <- na.omit(better_train)

better_train$user_item <- paste(better_train$user_id,better_train$item_id,sep = "_")
movie_categories <- factor(c("unknown","action","adventure","animation","childrens","comedy","crime","documentary","drama","fantasy","film_noir","horror","musical","mystery","romance","sci_fi","thriller","war","western"))
movie_cat_level <- levels(movie_categories)

for (i in 1:nrow(better_train)){
  for (j in 1:nlevels(movie_categories)){
    if (better_train[i,movie_cat_level[j]]==TRUE){
      better_train[i,"movie_category"] <- movie_cat_level[j]
    }
  }
}

better_train <- better_train[ -c(1:2, 9:27) ]


linearModel <- glm(rating ~ favoured + age + gender + occupation + timestamp 
                   + age_band + item_mean_rating 
                   + user_age_band_item_mean_rating 
                   + user_gender_item_mean_rating + item_imdb_rating_of_ten 
                   + item_imdb_count_ratings + item_imdb_mature_rating 
                   + item_imdb_length + item_imdb_staff_votes 
                   + item_imdb_staff_average + item_imdb_top_1000_voters_votes 
                   + item_imdb_top_1000_voters_average 
                   + user_gender_item_imdb_mean_rating 
                   + user_gender_item_imdb_votes 
                   + user_age_band_item_imdb_votes 
                   + user_age_band_item_imdb_mean_rating 
                   + user_gender_age_band_item_imdb_votes 
                   + user_gender_age_band_item_imdb_mean_rating + State 
                   + older_than_reviewer + age_diff + staff_prefered 
                   + user_mean_rating + user_age_band_gender_item_mean_rating 
                   + user_item + movie_category, data=better_train, 
                   family=gaussian(link="identity"))

better_train$predictions <- predict(linearModel)
better_train$error <- ((better_train$rating - better_train$predictions)^2)
RMSE <- sqrt(sum(better_train$error))
RMSE

movie_data_test <-readRDS("better_test.rds")