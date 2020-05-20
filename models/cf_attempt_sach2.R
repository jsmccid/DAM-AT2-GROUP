library(recommenderlab)
library(reshape2)
library(ggplot2)

movie_data <-readRDS("better_train.rds")

user_item_matrix<-acast(movie_data, user_id ~ item_id, value.var = "rating")

rating_matrix <- as(as.matrix(user_item_matrix), "realRatingMatrix")

e <- evaluationScheme(rating_matrix, method="split", train=0.8, given=10, goodRating=0)

rec=Recommender(getData(e, "train"), "UBCF", 
                param=list(normalize = "center",method="Cosine"))

recom <- predict(rec, rating_matrix, type="ratingMatrix")

calcPredictionAccuracy(recom,rating_matrix)

test_data <-readRDS("better_test.rds")

test_data$rating = 0
test_matrix<-acast(test_data, user_id ~ item_id, value.var = "rating")
test_rat_matrix <- as(as.matrix(test_matrix), "realRatingMatrix")

test_pred <- predict(rec, as.numeric(test_data$user_id), type="ratings")

test_pred@data@x[test_pred@data@x[] < 0] <- 0
test_pred@data@x[test_pred@data@x[] > 5] <- 5