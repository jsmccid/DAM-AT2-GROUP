# Load relevant libraries
library(ggplot2)
library(stringr)

# Read training data from RDS file
movie_data <-readRDS("AT2_train_STUDENT.rds")

# Read 
scrape <- readRDS("scrape.rds")

movie_data$length = 0
for (i in 1:nrow(movie_data)){
  movie_data[i,"length"]=scrape[scrape$movie_id==as.numeric(movie_data[i,"item_id"]),"length"]
}

## SUBSCRIPTIONS
# Find movies with a rating of 5
high_rating_data <- movie_data[movie_data$item_mean_rating >= 4.5,]
high_rating_content <- unique(high_rating_data$movie_title)

# Extracting the ratings made by male users between the ages of 18 and 24
core_high_rating <- high_rating_data[high_rating_data$age >= 18 & high_rating_data$age <= 44 & high_rating_data$gender == "M",]
core_high_content <- unique(core_high_rating$movie_title)

# Plotting Item ID against runtime
ggplot(data = movie_data,aes(x=item_id, y=length)) + geom_point()

# Extracting movies with a short runtime
short_core_data <- core_high_rating[core_high_rating$length <= 135,]
short_core_content <- unique(short_core_data$movie_title) # This gives only NA values

#Inspecting the runtime of movies highly rated by the core userbase
core_high_rating$length

movie_categories <- factor(c("unknown","action","adventure","animation","childrens","comedy","crime","documentary","drama","fantasy","film_noir","horror","musical","mystery","romance","sci_fi","thriller","war","western"))
movie_cat_level <- levels(movie_categories)

for (i in 1:nrow(core_high_rating)){
  for (j in 1:nlevels(movie_categories)){
    if (core_high_rating[i,movie_cat_level[j]]==TRUE){
      core_high_rating[i,"movie_category"] <- movie_cat_level[j]
    }
  }
}

core_high_rating$movie_category <- str_replace(core_high_rating$movie_category, "sci_fi", "sci-fi")

# Plotting number of ratings against movie genre for the core userbase
ggplot(data = core_high_rating,aes(x=movie_category)) + geom_bar() + labs(x = "Movie Genre",
       y = "Number of ratings",
       title = "Number of ratings by core userbase per genre")

## ADS
# Extracting movies with a long runtime
long_runtime_data <- movie_data[!is.na(movie_data$length >= 135),]
long_runtime_movies <- unique(long_runtime_data$movie_title)

# Finding the long running movies with high rating by core user base
long_core_data <- core_high_rating[core_high_rating$length >= 135,]
long_core_content <- unique(long_core_data$movie_title)