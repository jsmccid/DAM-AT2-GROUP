# Load relevant libraries
library(ggplot2)

# Read training data from RDS file
movie_data <-readRDS("AT2_train_STUDENT.rds")
scrape <- readRDS("scrape.rds")

movie_data$length = 0
for (i in 1:nrow(movie_data)){
  movie_data[i,"length"]=scrape[scrape$movie_id==as.numeric(movie_data[i,"item_id"]),"length"]
}

## SUBSCRIPTIONS
high_rating_data <- movie_data[movie_data$rating == "5",]
high_rating_content <- levels(as.factor(high_rating_data$movie_title))

core_high_rating <- high_rating_data[high_rating_data$age >= 18 & high_rating_data$age <= 44 & high_rating_data$gender == "M",]
core_high_content <- levels(as.factor(core_high_rating$movie_title))

ggplot(data = movie_data,aes(x=item_id, y=length)) + geom_point()

short_core_data <- core_high_rating[!is.na(core_high_rating$length <= 150),]
short_core_content <- levels(as.factor(core_high_rating$movie_title))