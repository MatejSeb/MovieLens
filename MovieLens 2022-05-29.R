##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(stringi)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
#movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#                                           title = as.character(title),
#                                           genres = as.character(genres))
# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)
#################################################################################################
# Force the values displayed in the graphs NOT to be in the scientific format (e.g. 1.3e08)
options(scipen=999)

# backup the dataset provided by the course instructors.
#---------------------------------------------------------------------------------------------------
edx_original = edx
validation_original = validation

# Transform the data provided, by including columns regarding genres, time dimensions etc.
#---------------------------------------------------------------------------------------------------
# The lines of code below handle rows that only have one genre assigned to it.
# Create a column named single_genre, which contains 1 if the genres column contains only
# one genre, and 0 if it contains more than one genre.
edx = edx %>% mutate("single_genre" = ifelse(edx$genres %in% c("Comedy","Drama","Thriller","Western","Horror","Documentary","Action","Romance",
                  "Sci-Fi","Children","Adventure","Animation","Musical","Film-Noir","Crime","War",
                  "Mystery","Fantasy","IMAX","(no genres listed)") ==TRUE,1,0))

validation = validation %>% mutate("single_genre" = ifelse(validation$genres %in% c("Comedy","Drama","Thriller","Western","Horror","Documentary","Action","Romance",
                                                               "Sci-Fi","Children","Adventure","Animation","Musical","Film-Noir","Crime","War",
                                                               "Mystery","Fantasy","IMAX","(no genres listed)") ==TRUE,1,0))

# If the movie has only one genre, retain the genre, otherwise return NA.
edx = edx %>% mutate("single_genre_title" = ifelse(single_genre == 1, genres, NA))

validation = validation %>% mutate("single_genre_title" = ifelse(single_genre == 1, genres, NA))

#---------------------------------------------------------------------------------------------------
# The following lines recode the genres column to numbers, since calculating anything with original
# genres column causes immense problems due to the size of the column - it can contain a lot of
# letters.
# Select the column genres, then obtain its distinct values.
edx_genres = edx %>% select(genres)
distinct_edx_genres = distinct(edx_genres)

validation_genres = validation %>% select(genres)
distinct_validation_genres = distinct(validation_genres)

# Assign each of the distinct values a random number.
set.seed(1, sample.kind = "Rounding")
distinct_edx_genres_numbers = data.frame(distinct_edx_genres, genre_number = sample(seq(1, nrow(distinct_edx_genres), 1), nrow(distinct_edx_genres), replace = FALSE))
distinct_edx_genres_numbers %>% arrange(genre_number)

set.seed(1, sample.kind = "Rounding")
distinct_validation_genres_numbers = data.frame(distinct_validation_genres, genre_number = sample(seq(1, nrow(distinct_validation_genres), 1), nrow(distinct_validation_genres), replace = FALSE))
distinct_validation_genres_numbers %>% arrange(genre_number)

# Merge the number of each genre back into the edx dataset. Remove the name of genre.
edx_gennum = merge(edx, distinct_edx_genres_numbers)
edx_gennum_nogenre = edx_gennum %>% select(-genres)

validation_gennum = merge(validation, distinct_validation_genres_numbers)
validation_gennum_nogenre = validation_gennum %>% select(-genres)

#---------------------------------------------------------------------------------------------------
# The following line extracts the year of movie creation from the movie name.
edx_new = edx_gennum_nogenre %>% 
  mutate(movie_year = as.numeric(substr(stri_sub(edx_gennum_nogenre$title,-6),2,5)))

validation_new = validation_gennum_nogenre %>% 
  mutate(movie_year = as.numeric(substr(stri_sub(validation_gennum_nogenre$title,-6),2,5)))

#---------------------------------------------------------------------------------------------------
# The following lines calculates the number of entries, mean and standard deviation of rating for each genre.
edx_new_genre = edx_new %>% group_by(genre_number) %>%
  summarise(n_genre = n(), mean_genre = mean(rating), sd_genre = sd(rating))
edx_new_genre_rejoined = merge(edx_new, edx_new_genre,by = "genre_number")
edx_new = edx_new_genre_rejoined

validation_new_genre = validation_new %>% group_by(genre_number) %>%
  summarise(n_genre = n(), mean_genre = mean(rating), sd_genre = sd(rating))
validation_new_genre_rejoined = merge(validation_new, validation_new_genre,by = "genre_number")
validation_new = validation_new_genre_rejoined

#---------------------------------------------------------------------------------------------------
# The following lines calculate the number of entries, mean and standard deviation of rating for each movie.
edx_new_movie = edx_new %>% group_by(movieId) %>%
  summarise(n_movie = n(), mean_movie = mean(rating), sd_movie = sd(rating))
edx_new_movie_rejoined = merge(edx_new, edx_new_movie,by = "movieId")

validation_new_movie = validation_new %>% group_by(movieId) %>%
  summarise(n_movie = n(), mean_movie = mean(rating), sd_movie = sd(rating))
validation_new_movie_rejoined = merge(validation_new, validation_new_movie,by = "movieId")

#---------------------------------------------------------------------------------------------------
# The following line calculates the year of rating of the movie. The timestamp column contains 
# the seconds after 01.01.1970 the rating took place (https://files.grouplens.org/datasets/movielens/ml-10m-README.html).
# Thus, the time stamp value is transformed to minutes, hours, days, and (taking account of leap years)
# to years. Finally, the title of the movie is removed.
edx_new = edx_new_movie_rejoined %>% mutate(year_timestamp = round(timestamp/(60*60*24*((365*3+366)/4))+1970)) %>% 
  select(-title)

validation_new = validation_new_movie_rejoined %>% mutate(year_timestamp = round(timestamp/(60*60*24*((365*3+366)/4))+1970)) %>% 
  select(-title)

#---------------------------------------------------------------------------------------------------
# Calculate the number of ratings and mean rating per user
edx_new_users = edx_new %>% group_by(userId) %>% summarise(n_user = n(), mean_user = mean(rating))
edx_new = merge(edx_new, edx_new_users, by = "userId")

validation_new_users = validation_new %>% group_by(userId) %>% summarise(n_user = n(), mean_user = mean(rating))
validation_new = merge(validation_new, validation_new_users, by = "userId")

#---------------------------------------------------------------------------------------------------
# Calculate the number of ratings and mean rating per user and genre
edx_new_user_genre = edx_new %>% group_by(userId, genre_number) %>% summarise(n_user_genre = n(),
                                                                         mean_user_genre = mean(rating))
edx_new = merge(x=edx_new, y=edx_new_user_genre, by.x=c("userId", "genre_number"), by.y=c("userId", "genre_number"))

validation_new_user_genre = validation_new %>% group_by(userId, genre_number) %>% summarise(n_user_genre = n(),
                                                                              mean_user_genre = mean(rating))
validation_new = merge(x=validation_new, y=validation_new_user_genre, by.x=c("userId", "genre_number"), by.y=c("userId", "genre_number"))

#---------------------------------------------------------------------------------------------------
# Delete the redundant stored objects, and clear the computer memory.
rm(edx_gennum, edx_gennum_nogenre, edx_new_genre, edx_new_genre_rejoined, edx_new_movie, 
   edx_new_movie_rejoined, distinct_edx_genres, edx_genres, edx_new_user_genre, edx_new_users)
rm(validation_gennum, validation_gennum_nogenre, validation_new_genre, validation_new_genre_rejoined, validation_new_movie, 
   validation_new_movie_rejoined, distinct_validation_genres, validation_genres, validation_new_users, validation_new_user_genre)
gc()

#---------------------------------------------------------------------------------------------------
# Preview the original data set 'edx_original' and the transformed data set 'edx_new'
head(edx_original)
head(edx_new)

# Calculate the RMSE. 
#---------------------------------------------------------------------------------------------------
# The method returning the best (lowest) RMSE was loess. Cross validation was used both to improve the RMSE and to speed up the calculation times, with the value of 7 chosen.
# Standard deviations did not present a meaningful improvement to RMSE and were dropped. 
# Year of timestamp was not used, as the original timestamp yielded better RMSE. 
# The mean of user and genre combination was also dropped, as it contributed to overtraining.

# Fair warning - the calculation of the algorithm took over 30 minutes on 64 GB of ddr5 ram and i9-12900k
# Intel processor. The average memory usage was above 25 GB. Which shows that good computers are
# no substitute for statistics and coding knowledge. Run the code below at your own discretion.
gc()
edx_control_loess <- trainControl(method = "cv", number = 7)

edx_new = edx_new %>% filter(n_movie > 10)

#grid <- expand.grid(span = 0.35, degree = 1)
set.seed(1, sample.kind = "Rounding")
edx_loess <- train(rating ~ 
                  genre_number
                  +userId
                  +movieId
                  +timestamp 
                  #+year_timestamp
                  +n_genre
                  +mean_genre
                  #+sd_genre
                  +single_genre
                  +movie_year
                  +n_movie
                  +mean_movie                  
                  #+sd_movie,
                  +n_user
                  +mean_user
                  +n_user_genre
                  #+mean_user_genre
                  , method = "gamLoess",
                  #tuneGrid=grid,
                  data = edx_new
                  ,trControl = edx_control_loess
)
edx_loess
RMSE_initial = round(edx_loess$results["RMSE"], 5)

# Run the algorithm on the validation data set to obtain the final RMSE.
validation_prediction = predict(edx_loess, validation_new)

RMSE_final = round(RMSE(validation_prediction, validation_new$rating), 5)
RMSE_final


# Graphs
#---------------------------------------------------------------------------------------------------
# Histogram of ratings.
edx %>% ggplot(aes(rating)) + geom_histogram() + ggtitle("Frequency of ratings") +
  xlab("Rating") + ylab("No. of ratings")


# Mean rating vs. no. of ratings according to the year of rating (from timestamp).
# Calculate the ratio of the mean rating vs. number of ratings. This will enable the proper
# combo graph to be displayed, by correcting the scale of the second y axis.
year_timestamp_data = edx_new %>% group_by(year_timestamp) %>%
  summarise(mean = mean(rating), n = n())
year_timestamp_data_coeff = max(year_timestamp_data$mean)/max(year_timestamp_data$n)

# The code below draws the combo graph, which has different values on each y axis. The axis and
# lines are colored the same color, so that we can recognize which is which.
edx_new %>% group_by(year_timestamp) %>%
  summarise(mean = mean(rating), n = n()) %>%
  ggplot(aes(x = year_timestamp, group = 1)) +
  geom_line(aes(y = mean), col="blue") +
  geom_line(aes(y = n*year_timestamp_data_coeff), col="red") +
  scale_y_continuous(name = "Mean rating", sec.axis = sec_axis(~./year_timestamp_data_coeff, name="No. of ratings")) +
  ggtitle("Mean rating vs. no. of ratings per year of rating") +
  xlab("Year of rating") +
  theme(axis.title.y.left=element_text(color="blue"),
        axis.text.y.left=element_text(color="blue"),
        axis.title.y.right=element_text(color="red"),
        axis.text.y.right=element_text(color="red"))


# No. of ratings per year of rating, divided by individual rating.
edx_new %>% mutate(rating = as.character(rating)) %>%
  ggplot(aes(x = year_timestamp, group = rating, color = rating), label = rating) +
  geom_line(stat = 'count') + ggtitle("Frequency of ratings per rating and year of rating") +
  xlab("Year of rating") + ylab("No. of ratings")


# Mean rating vs. no. of ratings according to the year of movie creation.
# Calculate the ratio of the mean rating vs. number of ratings. This will enable the proper
# combo graph to be displayed, by correcting the scale of the second y axis.
year_created_data = edx_new %>% group_by(movie_year) %>%
  summarise(mean = mean(rating), n = n())
year_created_data_coeff = max(year_created_data$mean)/max(year_created_data$n)

# The code below draws the combo graph, which has different values on each y axis. The axis and
# lines are colored the same color, so that we can recognize which is which.
edx_new %>% group_by(movie_year) %>%
  summarise(mean = mean(rating), n = n()) %>%
  ggplot(aes(x = movie_year, group = 1)) +
  geom_line(aes(y = mean), col="blue") +
  geom_line(aes(y = n*year_created_data_coeff), col="red") +
  scale_y_continuous(name = "Mean rating", sec.axis = sec_axis(~./year_created_data_coeff, name="No. of ratings")) +
  ggtitle("Mean rating vs. no. of ratings per year of movie creation") +
  xlab("Year of movie creation") +
  theme(axis.title.y.left=element_text(color="blue"),
        axis.text.y.left=element_text(color="blue"),
        axis.title.y.right=element_text(color="red"),
        axis.text.y.right=element_text(color="red"))


# Mean rating vs. no. of ratings according to the genre, where only one genre per movie is specified.
# Calculate the ratio of the mean rating vs. number of ratings. This will enable the proper
# combo graph to be displayed, by correcting the scale of the second y axis.
genre_rating_data = edx_new %>% filter(single_genre == 1) %>% group_by(single_genre_title) %>%
  summarise(mean = mean(rating), n = n())
genre_rating_data_coeff = max(genre_rating_data$mean)/max(genre_rating_data$n)

# The code below draws the combo graph, which has different values on each y axis. The axis and
# lines are colored the same color, so that we can recognize which is which.
edx_new %>% filter(single_genre == 1) %>% group_by(single_genre_title) %>%
  summarise(mean = mean(rating), n = n()) %>%
  ggplot(aes(x = reorder(single_genre_title, mean), group = 1)) +
  theme(axis.text.x = element_text(angle = 65, hjust = 1)) +
  geom_line(aes(y = mean), col="blue") +
  geom_line(aes(y = n*genre_rating_data_coeff), col="red") +
  scale_y_continuous(name = "Mean rating", sec.axis = sec_axis(~./genre_rating_data_coeff, name="No. of ratings")) +
  ggtitle("Mean rating vs. no. of ratings per genre (single genres only)") +
  xlab("Genre name (single genres only)") +
  theme(axis.title.y.left=element_text(color="blue"),
        axis.text.y.left=element_text(color="blue"),
        axis.title.y.right=element_text(color="red"),
        axis.text.y.right=element_text(color="red"))