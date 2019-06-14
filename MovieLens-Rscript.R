
#MovieLens Project
#R-script submitted by Myint Moe Chit

# Installing (if required) and loading the necessary packages

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")


# Downloading the dataset from GroupLens 
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)


# Extracting the downloaded data and constructing a dataframe
ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)

colnames(movies) <- c("movieId", "title", "genres")


movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres)) 

movielens <- left_join(ratings, movies, by = "movieId")

movielens %>% as_tibble()

# Plotting the number of ratings received by different movies

movielens %>% 
    dplyr::count(movieId) %>% 
    ggplot(aes(n)) + 
    geom_histogram(bins = 30, color = "black") + 
    scale_x_log10() + 
    ggtitle("Movies")

# Plotting the number of ratings given by different users
movielens %>% 
    dplyr::count(userId) %>% 
    ggplot(aes(n)) + 
    geom_histogram(bins = 30, color = "black") +
    scale_x_log10() +
    ggtitle("Users")

# Seperating genres in individual rows
movielens_g <- separate_rows(data = movielens, genres, sep = "\\|")

# Plotting the number of ratings received by different movies
movielens_g %>% group_by(genres) %>%
    summarize(n = n(), avg = mean(rating)) %>%
    ggplot(aes(x = genres, y = avg)) + 
    geom_col() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Dividind data into training set (90% of observations) and test (edx) set (10% of observations)
set.seed(1)

test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)

edx <- movielens[-test_index,]

temp <- movielens[test_index,]

# Adding userId and movieId in validation set and edx set

validation <- temp %>% 
    semi_join(edx, by = "movieId") %>%
    semi_join(edx, by = "userId")

# Adding rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

# Removeing the unnecessary files

rm(dl, ratings, movies, test_index, temp, removed)

#Summary of training and test sets in tidy format

edx %>% as_tibble()

validation %>% as_tibble()

#Analysis section

#RMSE function

RMSE <- function(true_ratings, predicted_ratings){
    sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Calculating the average rating

mu_hat <- mean(edx$rating)

mu_hat

# Baseline estimation using the average rating

mu_rmse <- RMSE(validation$rating, mu_hat)

mu_rmse

rmse_results <- data_frame(Method = "Just using the average", RMSE = mu_rmse)

rmse_results %>% knitr::kable()

# Model with movie-specific bias
# Calculating movie-specific bias

mu <- mean(edx$rating)

movie_avgs <- edx %>% 
    group_by(movieId) %>% 
    summarize(b_i = mean(rating - mu))

movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))

# Estimating using the movie-specific effect
movie_specific <- mu + validation %>% 
    left_join(movie_avgs, by='movieId') %>%
    .$b_i

movie_specific_rmse <- RMSE(movie_specific, validation$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="Movie-specific Effect Model",  
                                     RMSE = movie_specific_rmse))

rmse_results %>% knitr::kable()

# Model with Movie and User-specific bias
# Calculating user-specific bias

user_avgs <- edx %>% 
    left_join(movie_avgs, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = mean(rating - mu - b_i))

user_avgs %>% qplot(b_u, geom ="histogram", bins = 20, data = ., color = I("black"))

# Estimating using movie and User-specific effects

user_specific <- validation %>% 
    left_join(movie_avgs, by='movieId') %>%
    left_join(user_avgs, by='userId') %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred

user_movie_rmse <- RMSE(user_specific, validation$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="Movie + User-specific Effects Model",  
                                     RMSE = user_movie_rmse))

rmse_results %>% knitr::kable()

# Model with Movie, User, and Genre-specific bias
# Calculating genres-specific bias

genres_avgs <- edx %>% 
    left_join(movie_avgs, by='movieId') %>%
    left_join(user_avgs, by='userId') %>%
    group_by(genres) %>%
    summarize(b_g = mean(rating - mu - b_i - b_u))


genres_avgs %>% qplot(b_g, geom ="histogram", bins = 20, data = ., color = I("black"))

# Estimating using Movie User and Genres-specific effects

genres_specific <- validation %>% 
    left_join(movie_avgs, by='movieId') %>%
    left_join(user_avgs, by='userId') %>%
    left_join(genres_avgs, by='genres') %>%
    mutate(pred2 = mu + b_i + b_u + b_g) %>%
    .$pred2

user_movie_genres_rmse <- RMSE(genres_specific, validation$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="Movie + User + genres-specific Effects Model",  
                                     RMSE = user_movie_genres_rmse))

rmse_results %>% knitr::kable()

#Regularisation

lambdas <- seq(4, 6, 0.02)

rmses <- sapply(lambdas, function(l){
    
    mu <- mean(edx$rating)
    
    b_i <- edx %>% 
        group_by(movieId) %>%
        summarize(b_i = sum(rating - mu)/(n()+l))
    
    b_u <- edx %>% 
        left_join(b_i, by="movieId") %>%
        group_by(userId) %>%
        summarize(b_u = sum(rating - b_i - mu)/(n()+l))
    
    b_g <- edx %>% 
        left_join(b_i, by="movieId") %>%
        left_join(b_u, by="userId") %>%
        group_by(genres) %>%
        summarize(b_g = sum(rating - b_i - b_u - mu)/(n()+l))
    
    predicted_regu <- 
        validation %>% 
        left_join(b_i, by = "movieId") %>%
        left_join(b_u, by = "userId") %>%
        left_join(b_g, by = "genres") %>%
        mutate(pred = mu + b_i + b_u + b_g) %>%
        pull(pred)
    
    return(RMSE(predicted_regu, validation$rating))
})

qplot(lambdas, rmses)  

# Estimating regularised model
lambda <- lambdas[which.min(rmses)]
lambda

rmse_results <- bind_rows(rmse_results,
                          data_frame(Method="Regularized Movie + User + Genres Effect Model",  
                                     RMSE = min(rmses)))

rmse_results %>% knitr::kable()

