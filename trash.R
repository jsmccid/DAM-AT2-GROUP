## trash 


# cs <- cosineSim(movies_matrix)
# cd <- 1-cs
# 
# cosine_movie_clus <- kmeans(cd, 10, nstart = 100)
# 
# wss <- 2:(length(movies_matrix)-1)
# for (i in 2:(length(movies_matrix)-1)) wss[i] <- sum(kmeans(cd,centers=i,nstart=25)$withinss)
# plot(2:(length(movies_matrix)-1), wss[2:(length(movies_matrix)-1)], type="b", xlab="Number of Clusters",
#      ylab="Within groups sum of squares") 

### from collabfill

# test$rating <- NA
# test_ratings <- test %>% 
#   select(user_id, item_id, rating) %>% 
#   pivot_wider(names_from = item_id, values_from = rating) %>% 
#   column_to_rownames(var = "user_id")
# 
# test_matrix <- as.matrix(test_ratings)
# 
# test_matrix_big <- left_join(test_matrix, train_matrix) 
# 
# test_rrm <- as(test_matrix, "realRatingMatrix")
# 
# preds_ibcf_mat <- as(preds_ibcf, "matrix")
# pred_ib_df <- as.data.frame(recom_ibcf_mat)
# 
# icbf_sub <- rec_ib_df %>% 
#   pivot_longer(names_to = c("item_id", "rating"))
# 
# jest <- as(Jester5k, "matrix")