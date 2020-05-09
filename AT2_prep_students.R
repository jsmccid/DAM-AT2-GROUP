# =========================================================================== #
#                                                                             #
# File Name: AT2_prep_ADMIN.R                                                 #
# Description: Imports all the preprocessed data, combines, creates new       #
#              features and then outputs a train.rds and test.rds file for    #
#              ML.                                                            #
#                                                                             #
# =========================================================================== #
# == Clean ================================================================
rm(list=ls())

# == Libraries ================================================================
# Load packages
library(tidyverse)
library(lubridate)
library(forcats)
library(caret)

# == Import preprocessed data =================================================

# Webscraping dataa
scrape <- readRDS('scrape.rds')

# Raw testing and training data
test <-  readRDS('test_raw.rds')
train <- readRDS('train_raw.rds')

# = Cleaning ==========================================================

# IMDB webscraping general item data
scrape_item_general <- scrape %>% select(movie_id, rating_of_ten:length,
                                         imdb_staff_votes:top_1000_voters_average)

# Add item_imdb_ to column names to disambiguate
names(scrape_item_general)[c(-1,-6,-7)] <- paste0('imdb_', names(scrape_item_general)[c(-1,-6,-7)])
names(scrape_item_general)[-1] <- paste0('item_', names(scrape_item_general)[-1])

# User age bands (Train)
train$age_band <- factor(case_when(
  train$age < 18 ~ 'under_18',
  train$age >= 18 & train$age <= 29 ~ '18_to_29',
  train$age >= 30 & train$age <= 44 ~ '30_to_44',
  train$age >= 45 ~ '45_and_over'),
  levels = c('under_18', '18_to_29', '30_to_44', '45_and_over'),
  ordered = TRUE # Don't order as makes modelling annoying
)

# User age bands (Test)
test$age_band <- factor(case_when(
  test$age < 18 ~ 'under_18',
  test$age >= 18 & test$age <= 29 ~ '18_to_29',
  test$age >= 30 & test$age <= 44 ~ '30_to_44',
  test$age >= 45 ~ '45_and_over'),
  levels = c('under_18', '18_to_29', '30_to_44', '45_and_over'),
  ordered = TRUE # Don't order as makes modelling annoying
)

# == Joining data and aggregate feature engineering ===========================

# = General features ==========================================================

# Mean rating per item
item_mean_ratings_train <- train %>% 
  group_by(item_id) %>% 
  summarise(item_mean_rating = mean(rating))

# = Demographic-wide features =================================================

# - Gender ratings per item
user_gender_item_mean_ratings_train <- train %>% 
  group_by(gender, item_id) %>% 
  summarise(user_gender_item_mean_rating = mean(rating)) %>% 
  ungroup()

# - Age band ratings per item
user_age_band_item_mean_ratings_train <- train %>% 
  group_by(age_band, item_id) %>% 
  summarise(user_age_band_item_mean_rating = mean(rating)) %>% 
  ungroup()

# = User specific IMDB demographic-wide features ==============================

# - IMDB Gender ratings per item
scrape_gender_specific <- scrape %>% 
  select(movie_id, males_votes:females_average) %>% 
  gather(key = gender_score, value = value, -movie_id, na.rm = TRUE) %>% 
  separate(col = gender_score, into = c('gender', 'score_type')) %>% 
  spread(key = score_type, value = value) %>% 
  mutate(gender = fct_recode(gender, 'M' = 'males', 'F' = 'females'),
         gender = fct_rev(gender)) %>% # To get the factor levels right
  rename(item_id = movie_id,
         user_gender_item_imdb_mean_rating = average,
         user_gender_item_imdb_votes = votes)

# Check levels are correct
identical(levels(train$gender), levels(scrape_gender_specific$gender))

# - Age band ratings per item
scrape_age_specific <- scrape %>% 
  select(movie_id, starts_with('aged')) %>% 
  gather(key = age_score, value = value, -movie_id, na.rm = TRUE) %>% 
  separate(col = age_score, into = c('age_band', 'score_type'), sep = '_a|_v') %>% 
  spread(key = score_type, value = value) %>% 
  mutate(age_band = fct_recode(age_band,
                               'under_18' = 'aged_under_18',
                               '18_to_29' = 'aged_18-29',
                               '30_to_44' = 'aged_30_44',
                               '45_and_over' = 'aged_45'),
         age_band = factor(age_band,
                           levels = c('under_18', '18_to_29', '30_to_44', '45_and_over'),
                           ordered = TRUE)) %>% # To get the factor levels the same as ml_data 
  rename(item_id = movie_id,
         user_age_band_item_imdb_mean_rating = verage,
         user_age_band_item_imdb_votes = otes)

# Check levels are correct
identical(levels(train$age_band), levels(scrape_age_specific$age_band))

# - Gender-Age band ratings per item
scrape_gender_age_specific <- scrape %>% 
  select(movie_id, males_under_18_votes:females_under_18_average,
         males_18_29_votes:females_18_29_average,
         males_30_44_votes:females_30_44_average,
         males_45_votes:females_45_average) %>% 
  gather(key = gender_age_score, value = value, -movie_id, na.rm = TRUE) %>% 
  separate(col = gender_age_score, into = c('gender', 'age_score_type'),
           extra = 'merge') %>% 
  separate(col = age_score_type, into = c('age_band', 'score_type'), sep = '_a|_v') %>% 
  spread(key = score_type, value = value) %>% 
  mutate(gender = fct_recode(gender, 'M' = 'males', 'F' = 'females'),
         gender = fct_rev(gender),
         age_band = fct_recode(age_band,
                               'under_18' = 'under_18',
                               '18_to_29' = '18_29',
                               '30_to_44' = '30_44',
                               '45_and_over' = '45'),
         age_band = factor(age_band,
                           levels = c('under_18', '18_to_29', '30_to_44', '45_and_over'),
                           ordered = TRUE)) %>% # To get the factor levels the same as ml_data 
  rename(item_id = movie_id,
         user_gender_age_band_item_imdb_mean_rating = verage,
         user_gender_age_band_item_imdb_votes = otes)

# Check levels are correct
identical(levels(train$gender), levels(scrape_gender_age_specific$gender))
identical(levels(train$age_band), levels(scrape_gender_age_specific$age_band))

# Now remove the scrape dataset
rm(scrape)

# = Join everything together =================================================

# Joing Variables onto Train Data
train <- train %>% 
  
  # Train set specific joins
  left_join(item_mean_ratings_train, by = 'item_id') %>% 
  left_join(user_age_band_item_mean_ratings_train, by = c('age_band', 'item_id')) %>% 
  left_join(user_gender_item_mean_ratings_train, by = c('gender', 'item_id')) %>% 
  
  # Scrape General joins (External dataset)
  left_join(scrape_item_general, by = c('item_id' = 'movie_id')) %>%
  left_join(scrape_gender_specific, by = c('gender', 'item_id')) %>% 
  left_join(scrape_age_specific, by = c('age_band', 'item_id')) %>%
  left_join(scrape_gender_age_specific, by = c('gender', 'age_band', 'item_id')) %>% 
  
  # Scrape User_Gender_Age joins (External dataset)
  
  mutate(user_id = factor(user_id),      # Make user and item factors
         item_id = factor(item_id))

# Joing Variables onto Test Data
test <- test %>% 
  
  # Train set specific joins
  left_join(item_mean_ratings_train, by = 'item_id') %>% 
  left_join(user_age_band_item_mean_ratings_train, by = c('age_band', 'item_id')) %>% 
  left_join(user_gender_item_mean_ratings_train, by = c('gender', 'item_id')) %>% 
  
  # Scrape General joins (External dataset)
  left_join(scrape_item_general, by = c('item_id' = 'movie_id')) %>%
  left_join(scrape_gender_specific, by = c('gender', 'item_id')) %>% 
  left_join(scrape_age_specific, by = c('age_band', 'item_id')) %>%
  left_join(scrape_gender_age_specific, by = c('gender', 'age_band', 'item_id')) %>% 

  mutate(user_id = factor(user_id),      # Make user and item factors
         item_id = factor(item_id))

# Remove the variable sets (Train)
rm(item_mean_ratings_train, 
   user_gender_item_mean_ratings_train, user_age_band_item_mean_ratings_train)

# Remove the varibale sets (Scrape)
rm(scrape_age_specific, scrape_gender_age_specific, scrape_gender_specific, scrape_item_general)

# Have a look
glimpse(train)
glimpse(test)

# Check missing
# missing_train <- train %>%
#   gather(col, value) %>%
#   group_by(col) %>%
#   summarize(missing_share = mean(is.na(value)))
# 
# missing_test <- train %>%
#   gather(col, value) %>%
#   group_by(col) %>%
#   summarize(missing_share = mean(is.na(value)))

# Remove missing checks
# rm(missing_train, missing_test)

# = Save out =================================================

# Save train out
saveRDS(train, 'AT2_train_STUDENT.rds')

# Save test out
saveRDS(test, 'AT2_test_STUDENT.rds')
