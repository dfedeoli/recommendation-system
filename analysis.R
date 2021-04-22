# Installing all required packages
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(skimr)) install.packages("skimr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(tibble)) install.packages("tibble", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(cowplot)) install.packages("cowplot", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")

#Loading required libraries
library(caret)
library(tidyverse)
library(ggplot2)
library(skimr)
library(lubridate)
library(stringr)
library(recosystem)
library(data.table)
library(tibble)
library(kableExtra)


## MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip


dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)


ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))


movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")


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


# Cleaning variables to recover RAM space
rm(dl, ratings, movies, test_index, temp, movielens, removed)


# Saving datasets as .rda files to make them easier to load, if necessary
save(edx, file = "rda/edx.rda")
save(validation, file = "rda/validation.rda")
#-------------------------------------------------------------------------------


## Exploratory Analysis and Visualization


# Preview of the dataframe
head(edx)


# More detailed alternative to Summary function
skim(edx)


# Number of distinct users and movies
edx %>% summarize(n_userIds = n_distinct(userId),
          n_movieIds = n_distinct(movieId),
          n_titles = n_distinct(title),
          n_genres = n_distinct(genres))


# More movies than titles?
edx %>% group_by(title) %>% 
  summarize(n_movieIds=n_distinct(movieId)) %>% 
  filter(n_movieIds>1)                              # Number of movies with more than 1 movieId


# War of the Worlds (2005) has 2 distinct movieIds
edx %>% filter(title=='War of the Worlds (2005)') %>% group_by(movieId) %>% 
  summarize(title=title[1], genres=genres[1],n=n()) # Count of reviews for each movieId


# Distribution of ratings
edx %>% mutate(stars=ifelse(rating %in% c(1:5),'full','half')) %>% 
  ggplot(aes(factor(rating),fill=stars)) + geom_bar() + scale_fill_hue(c=80) + 
  labs(x='rating')
ggsave("figs/ratings_dist.png")


# User and Movies Histograms (count)
p1 <- edx %>% count(userId) %>% ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + scale_x_log10() + 
  labs(x='Reviews given by user') # Number of reviews from each user
p2 <- edx %>% count(movieId) %>% ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + scale_x_log10() +
  labs(x='Reviews received by movie',y='') # Number of reviews of each movie
gridExtra::grid.arrange(p1, p2, ncol = 2)
rm(p1,p2)
ggsave('figs/count_histograms.png')


# User and Movies Histograms (average rating)
p1 <- edx %>% group_by(userId) %>% summarize(avg=mean(rating)) %>% 
  ggplot(aes(avg)) + geom_histogram(bins=35, color='black') +
  labs(x='Average rating given by user') #average rating a user gives
edx %>% group_by(userId) %>% summarize(avg=mean(rating)) %>% 
  filter(avg< 1.5) %>% dim() %>% .[1] # Few users average giving ratings under 1.5
p2 <- edx %>% group_by(movieId) %>% summarize(avg=mean(rating)) %>% 
  ggplot(aes(avg)) + geom_histogram(bins=35, color='black') +
  labs(x='Average rating received by movie') # Average rating received by a movie
edx %>% group_by(movieId) %>% summarize(avg=mean(rating)) %>% 
  filter(avg< 1.5) %>% dim() %>% .[1] # Few movies average less than 1.5
edx %>% group_by(movieId) %>% summarize(avg=mean(rating)) %>% 
  filter(avg> 4.25) %>% dim() %>% .[1] # Few movies average higher than 4.25
cowplot::plot_grid(p1,p2,ncol=1, align='v')
rm(p1,p2)
ggsave('figs/avg_rating_histograms.png')


# Unique Movies in the Dataset
unique_movies <- edx %>% group_by(title) %>% 
  summarize(title=title[1], genres=genres[1], count=n()) %>% arrange(desc(count))


# Plotting the most rated movies 
unique_movies[1:10,] %>% 
  mutate(title = str_remove(title, ", The"),                 
         title = str_remove(title, " - A New Hope \\(a.k.a. Star Wars\\)")) %>%  # Making titles smaller
  ggplot(aes(x=reorder(title, count), y=count)) + 
  geom_bar(stat = "identity") + labs(x='title') + coord_flip()
ggsave('figs/unique_movies_count.png')


# Plotting most rated movies average rating
edx %>% filter(title %in% head(unique_movies$title,n=10)) %>% group_by(title) %>% 
  summarize(avg=mean(rating), count=n()) %>% 
  mutate(title = str_remove(title, ", The"),
         title = str_remove(title, " - A New Hope \\(a.k.a. Star Wars\\)")) %>% 
  ggplot(aes(x=reorder(title, count), y=avg)) + geom_bar(stat = "identity") + 
  labs(x='',y='average rating') + coord_flip()
ggsave('figs/unique_movies_avg.png')


# Release date
release_date <- edx$title %>% str_extract('\\([0-9]{4}\\)') %>%
  str_extract('[0-9]{4}') %>% as.integer()
summary(release_date)
edx <- data.frame(edx,released=release_date) # Adding released date column
edx[which(edx$released==1915),] %>% summarize(title=title[1], n=n())


# Reviews count by release date
edx %>% group_by(movieId) %>%
  summarize(n = n(), released = as.character(first(released))) %>%
  qplot(released, n, data = ., geom = "boxplot") +
  scale_y_log10(breaks=c(1,10,100,200,500,1000,10000)) + scale_x_discrete(breaks=seq(1915,2005,5)) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + labs(x='year released',y='count')
ggsave('figs/release_date_review_count.png')


# Average ratings by release date
edx %>% group_by(movieId) %>%
  summarize(n = n(), released = as.character(first(released)), avg_rating=mean(rating)) %>%
  qplot(released, avg_rating, data = ., geom = "boxplot") + scale_x_discrete(breaks=seq(1915,2005,5)) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + labs(x='year released', y='average rating')
ggsave('figs/release_date_avg_rating.png')
edx <- edx[,-ncol(edx),drop=FALSE]
rm(release_date)


# Top genre combinations reviewed
edx %>% group_by(genres) %>% summarize(count = n()) %>% arrange(desc(count)) %>% head(n=7) %>% 
  data.frame(pos=c(1:7), .)


# Plotting the genre effect on ratings
gcomb_15 <- c(1,seq(31,415,32),444) # Equally spaced indexes to select genre combinations
edx %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating),
            se = sd(rating)/sqrt(n())) %>%     # Average ratings and respective errors
  filter(n > 1000) %>% arrange(desc(avg)) %>%  # Only genres that were reviewed over 1000 times
  .[gcomb_15,] %>%
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg,
             ymin = avg - 2*se, ymax = avg + 2*se)) +  # Plotting the values and errors
  geom_point() + geom_errorbar() + theme(axis.text.x=element_text(angle = 30, hjust = 1)) + 
  labs(x='', y= 'average rating')
ggsave('figs/genreeffect.png')
rm(gcomb_20)


# All genres possible
genre_combinations <- unique(edx$genres)     # All unique genre combinations in the dataset
all_genres <- character()                    # Empty character vector
for (i in genre_combinations) {
  movie_genres <- str_split(i,'\\|')[[1]]    # Splitting the genre combinations to recover each genre
  for (j in movie_genres) {
    all_genres <- rlist::list.append(all_genres, j) # Appending each genre recovered to 'all_genres'
  }
}
all_genres <- unique(all_genres)             # Unique genres in the dataset
rm(genre_combinations,movie_genres,i,j)      # Cleaning variables to recover RAM space

# All genres reviews count
all_genres_count <- sapply(all_genres, function(g) {
  sum(str_detect(edx$genres, g)) # Number of reviews per genre      
}) %>% data.frame(review_count=.) %>% arrange(desc(.))
all_genres_count


# One genre with standard deviation too high (high variability, low number of ratings)
edx[which(edx$genres=='(no genres listed)'),]
edx[which(edx$genres=='(no genres listed)'),] %>% 
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n()))


# Unique genres average rating
all_genres_avg <- matrix(nrow=20,ncol=2) # Genre average and standard error matrix
i <- 1
for (var in all_genres) {
  vals <- edx[which(str_detect(edx$genres,as.character(var))),] %>% 
    summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) # n, average and se for each genre 
  all_genres_avg[i,1] <- vals$avg                                     # Genre average
  all_genres_avg[i,2] <- vals$se                                      # Genre standard error
  i <- i+1
}
data.frame(genres=all_genres, avg=all_genres_avg[,1], se=all_genres_avg[,2]) %>% # Genre, avg and se
  .[1:19,] %>%                                             # Dropping the '(no genres listed)' genre
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg,
             ymin = avg - 2*se, ymax = avg + 2*se)) +               # Plotting the values and errors
  geom_point() + geom_errorbar() + theme(axis.text.x=element_text(angle = 45, hjust = 1)) + 
  labs(x='genres', y= 'average rating')
ggsave('figs/unique_genres_avg_rating.png')
rm(var,i,vals,all_genres_avg)


# Top genres reviewed
top_genres <- all_genres_count %>% filter(.>=1e6) # Genres with over 1 million reviews


# Plotting number of reviews per top genre
top_genres %>% ggplot(aes(x= row.names(.), y = review_count, fill=row.names(.))) +
  geom_bar(stat='identity') + scale_fill_hue(c=40) + theme(legend.position="none") + 
  labs(x='', y='reviews count')
ggsave('figs/reviews_top_genres.png')


# Number of movies reviewed per genre
movies_per_top_genre <- sapply(row.names(top_genres), function(g) {
  sum(str_detect(unique_movies$genres, g))
})


# Plotting number of movies per top genre
data.frame(movies_per_top_genre) %>% 
  ggplot(aes(x= row.names(.), y = movies_per_top_genre, fill=row.names(.))) +
  geom_bar(stat='identity') + scale_fill_hue(c=40) + theme(legend.position="none") +
  labs(x='', y='movies count')
ggsave('figs/movies_top_genres.png')


# Transforming timestamp into date and time
edx <- mutate(edx, date = as_datetime(timestamp))


# Plotting the time effect (week) on ratings
edx %>% mutate(date = round_date(date, unit = "week")) %>% # Turning dates into week
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%                     # Average ratings per week
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()                                            # Smooth loess function   
ggsave('figs/timeeffect.png')


# Cleaning variables to recover RAM space
rm(movies_per_top_genre,top_genres,unique_movies,all_genres_count)
#-------------------------------------------------------------------------------


## Feature Engineering


# Transforming the 'genres' column into binary columns for each genre
for (var in all_genres) {
  edx <- edx %>% mutate(genre=ifelse(str_detect(genres,as.character(var)),1,0))
  colnames(edx)[ncol(edx)] <- as.character(var)
}
head(edx[,-c(4,5,7)]) # Checking the columns
edx <- edx[,-c(4:7),drop=FALSE] # Dropping timestamp, title and date
rm(var)
#-------------------------------------------------------------------------------


## Modeling


# Creating the training and test sets
set.seed(123, sample.kind = "Rounding")
index <- createDataPartition(edx$rating, times=1, p=0.2, list= FALSE) # Indexes for the sets
test <- edx[index,]
train <- edx[-index,]

# Cleaning variable and reducing dataframe to recover RAM space
rm(index)
edx <- edx %>% select(userId, movieId, rating) # only utilized again in the end ('recosystem' package)


# Make sure userId and movieId in test set are also in train set
test <- test %>% 
  semi_join(train, by = "movieId") %>%
  semi_join(train, by = "userId")


# RMSE function to check model's quality
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}
#-------------------------------------------------------------------------------


## Naive RMSE/ Overall Average Rating 


# Average rating of the training set (mu)
mu <- mean(train$rating)


# Naive RMSE calculation
RMSE(test$rating, mu)
#-------------------------------------------------------------------------------


## Movie Effects


# Movie-specific coefficient (b_i)
movie_avgs <- train %>% group_by(movieId) %>% summarize(b_i = mean(rating-mu))


# Predicting ratings with 'mu' and 'b_i'
predicted_ratings <- mu + test %>%
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)


# Summary and calculated RMSE
summary(predicted_ratings)
RMSE(test$rating, predicted_ratings)
#-------------------------------------------------------------------------------


## Movie + User effects


# User-specific coefficient (b_u)
user_avgs <- train %>%  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%  summarize(b_u = mean(rating - mu - b_i))


# Predicting ratings with 'mu', 'b_i' and 'b_u'
predicted_ratings <- test %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)


# Summary, number of predicted values under 0.5 and over 5 and calculated RMSE
summary(predicted_ratings)
sum(predicted_ratings<0.5)
sum(predicted_ratings>5)
RMSE(test$rating, predicted_ratings)


rm(predicted_ratings) # Cleaning variables to recover RAM space
#-------------------------------------------------------------------------------


## Genres Effect


# Adding 'b_i' and 'b_u' permanently to the training and test sets
train <- train %>% left_join(movie_avgs, by='movieId') %>% 
  left_join(user_avgs, by='userId') 
test <- test %>% left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId')
rm(user_avgs,movie_avgs)

# Calculating the beta value for each genre in the dataset (from beta = sum of beta_k*x_i)
beta_k <- vector() # empty vector
for (i in seq_along(all_genres)) {
  b_value<- train %>% 
    group_by(!!sym(all_genres[[i]])) %>%          # Unquoting with !! and sym
    summarize(beta_k=mean(rating-mu-b_i-b_u)) %>% # beta value for all_genre[i]
    filter((.[1])==1) %>% .[[2]]                  # filter for all_genre[i]==1 and pulling the beta value
  beta_k <- append(beta_k, b_value)               # appending value to beta vector
}
rm(i,b_value)


# 'beta' as a dataframe
df_beta<-data.frame(beta_k) 
rownames(df_beta) <- all_genres
df_beta # size (20,1)

# Matrix multiplication to determine sum of beta*x_i (genre effect)
head(test[,4:23])                                   # genre columns, size (1799965,20)
sum_x_beta <- as.matrix(test[,4:23])%*%as.matrix(df_beta) # matrix multiplication (%*%)
sum_x_beta <- data.frame(sum_x_beta=sum_x_beta[,1])       # size (1799965,1)


# Adding 'beta' column to test set
test <- data.frame(test, sum_x_beta)
rm(df_beta,beta_k,sum_x_beta,all_genres) # Cleaning variables to recover RAM space


# Predicting ratings with 'mu', 'b_i', 'b_u' and 'beta'
predicted_ratings <- test %>%
  mutate(pred = mu + b_i + b_u + sum_x_beta) %>%
  pull(pred)


# Summary, number of predicted values under 0.5 and over 5 and calculated RMSE
summary(predicted_ratings)
sum(predicted_ratings<0.5)
sum(predicted_ratings>5)
RMSE(test$rating, predicted_ratings)
#-------------------------------------------------------------------------------


# Regularized Movie + User effects


# Cleaning variables and reducing dataframes to recover RAM space
rm(predicted_ratings)
train <- train %>% select(userId, movieId, rating)
test <- test %>% select(userId, movieId, rating)


# Applying different lambda values, obtaining their respective RMSE and plotting results
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  b_i <- train %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))       # Regularized Movie effects
  b_u <- train %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l)) # Regularized User effects
  predicted_ratings <-
    test %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)                                      # Predictions with 'mu', 'b_i' and 'b_u'
  return(RMSE(test$rating, predicted_ratings))
})
qplot(lambdas, rmses, xlab='lambda', ylab='RMSEs')  # Plotting lambda v RMSEs 
ggsave('figs/lambdas_rmses.png')


lambdas[which.min(rmses)] # Optimal lambda value
min(rmses)                # Minimum RMSE


rm(lambdas,rmses,mu) # Cleaning variables to recover RAM space
#-------------------------------------------------------------------------------


# Matrix Factorization with recosystem


# Training set
train_dm <- data_memory(user_index = train$userId, item_index = train$movieId,
                          rating = train$rating,
                        index1 = TRUE) # Storing userId, movieId and rating for the training set
rm(train) # Cleaning variable to recover RAM space


# Test set
test_dm <- data_memory(user_index = test$userId, item_index = test$movieId, 
                       index1 = TRUE)             # Storing userId and movieId for the test set
test_rating <- test$rating                        # Storing the ratings from test set to compare later 
rm(test) # Cleaning variable to recover RAM space


# Model tuning for optimal results
set.seed(123, sample.kind = "Rounding") # Setting seed
r <- Reco()                             # Loading the recommendation system 
params = r$tune(train_dm, opts = list(dim = c(15, 20),
                                        costp_l1 = 0, #c(0, 0.1),
                                        costp_l2 = c(0.01, 0.1),
                                        costq_l1 = 0, #c(0, 0.1),
                                        costq_l2 = c(0.01, 0.1),
                                        lrate = c(0.075, 0.1), nthread = 2))


# Optimal parameters for minimum RMSE
optimal_params = params$min


# Training recommendation system with optimal parameters
r$train(train_dm, opts = c(optimal_params, nthread = 1, niter = 20))


# Predictions
predicted_ratings = r$predict(test_dm, out_memory()) 


# Summary, number of predicted values under 0.5 and over 5 and calculated RMSE
summary(predicted_ratings)
sum(predicted_ratings<0.5)
sum(predicted_ratings>5)
RMSE(test_rating, predicted_ratings)


rm(test_rating,predicted_ratings,params,test_dm)
#-------------------------------------------------------------------------------


# Training with full edx dataset


# Training set
train_dm <- data_memory(user_index = edx$userId, item_index = edx$movieId, 
                          rating = edx$rating, index1 = TRUE)
rm(edx) # Cleaning variables to recover RAM space


# Training recommendation system with optimal parameters
r$train(train_dm, opts = c(optimal_params, nthread = 1, niter = 20))

# Validation set
validation_dm <- data_memory(user_index = validation$userId, item_index = validation$movieId,
                             index1 = TRUE)
validation_rating <- validation$rating # Storing the ratings from validation set to compare later


#Predictions
predicted_ratings = r$predict(validation_dm, out_memory()) 
rm(train_dm,validation) # Cleaning variables to recover RAM space


# Summary, number of predicted values under 0.5 and over 5 and calculated RMSE
summary(predicted_ratings)
sum(predicted_ratings<0.5)
sum(predicted_ratings>5)
RMSE(validation_rating, predicted_ratings)

