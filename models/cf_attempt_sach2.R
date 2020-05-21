# Load relevant libraries
library(recommenderlab)
library(reshape2)
library(ggplot2)

# Read training data from RDS file
movie_data <-readRDS("better_train.rds")

# Convert training data to user-item matrix
user_item_matrix<-acast(movie_data, user_id ~ item_id, value.var = "rating")

# Convert user-item matrix to realRatingMatrix
rating_matrix <- as(as.matrix(user_item_matrix), "realRatingMatrix")

# Test-train split
e <- evaluationScheme(rating_matrix, method="split", train=0.8, given=10, goodRating=3)

# Create Recommender (model) using user-based ollaborative filtering, centered normalisation 
# and cosine similarity
rec=Recommender(getData(e, "train"), "UBCF", 
                param=list(normalize = "center",method="Cosine"))

# Make predictions using training data
recom <- predict(rec, rating_matrix, type="ratingMatrix")

# Calculate RMSE/MSE/MAE
calcPredictionAccuracy(recom,rating_matrix)

# Read test data from RDS file
test_data <-readRDS("better_test.rds")

# Add dummy rating column of zeros
test_data$rating = 0

# Convert test data to user-item matrix
test_matrix<-acast(test_data, user_id ~ item_id, value.var = "rating")

# Convert user-item matrix to realRatingMatrix
test_rat_matrix <- as(as.matrix(test_matrix), "realRatingMatrix")

# Prediction attempt 1
test_pred <- predict(rec, test_rat_matrix, type="ratings")

# Prediction attempt 2
test_users <- unique(test_data$user_id)
movie_users <- unique(movie_data$user_id)
test_int <- intersect(test_users,movie_users)
test_pred <- predict(object = rec, as.numeric(test_int), type="ratings")

nrow(rating_matrix)
nrow(test_rat_matrix)
ncol(rating_matrix)
ncol(test_rat_matrix)

test_pred@data@x[test_pred@data@x[] < 0] <- 0
test_pred@data@x[test_pred@data@x[] > 5] <- 5