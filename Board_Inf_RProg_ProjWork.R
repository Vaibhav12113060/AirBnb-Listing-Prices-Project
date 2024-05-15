
#  1. **Data Importing:** 
#   - Import the Airbnb data using readr or other relevant packages. This may include .csv files or other formats.

library(readr)

# Import the Airbnb data from the CSV file
airbnb_data <- read.csv('D:/R Programming/AB_NYC_2019.csv')

print(airbnb_data)

View(airbnb_data)


#  2. **Data Cleaning and Transformation:** 
#  2.1>   Use dplyr and tidyr to clean the data and prepare it for analysis. This may include handling missing values, outliers, or erroneous data.


install.packages("dplyr")
install.packages("tidyr")

library(dplyr)
library(tidyr)
      


    #  <i> HANDLING MISSING VALUES

# clean/delete the Empty space
clean_airbnb_data <- drop_na(airbnb_data)
View(clean_airbnb_data)

# Fill that particular empty spaces

# Calculate the mean of the variable
mean_Emp_spce <- mean(airbnb_data$reviews_per_month, na.rm = TRUE)

# Replace missing values with the mean
clean_airbnb_data1 <- airbnb_data %>%
  mutate(reviews_per_month = ifelse(is.na(reviews_per_month), mean_Emp_spce, reviews_per_month))

   # <ii>  Handling outliers:

# Calculate the lower and upper thresholds for outliers
lower <- quantile(airbnb_data$price)[2] - 1.5 * IQR(airbnb_data$price)
upper <- quantile(airbnb_data$price)[4] + 1.5 * IQR(airbnb_data$price)

# Remove outliers by filtering the data
clean_airbnb_data4 <- airbnb_data %>%
  filter(price >= lower & price <= upper)



   #  <iii>  Handling Erroneous data

# Identify and handle erroneous data
clean_airbnb_data5 <- airbnb_data %>%
  filter(price > 0 & price < 10000) %>%
  filter(minimum_nights >= 1 & minimum_nights <= 365) %>%
  filter(reviews_per_month >= 0 & reviews_per_month <= 31)
   # price <=0 && price>10000 are invalid
   # minimum31_nights<1 && minimum_nights>365 are invalid
   # reviews_per_month <0 && reviews_per_months >31 are invalid
  # It will filter out a/c to above conditions 


#       2.2> - Transform the data as necessary for analysis. This may include creating new variables, recoding variables, or restructuring the data.

#   <i> creating new variables 

tranform_airbnb_data1 <- airbnb_data %>%
  mutate(total_price = minimum_nights * price)


# <ii>  Recording variables

transform_airbnb_data2 <- airbnb_data %>%
  mutate(room_type = recode(room_type,
                            "Entire home/apt" = "Fully Access Home",
                            "Private room" = "Private Room",
                            "Shared room" = "Shared Room",
                            .default = "Other"))
View(transform_airbnb_data2)

# <iii> Restructuring the data

transform_airbnb_data3 <- airbnb_data %>%
  mutate(price = as.character(price)) %>%
  pivot_longer(cols = c(name, price, room_type), names_to = "variable", values_to = "value")

View(transform_airbnb_data3)

  #   3. **Exploratory Data Analysis:**

# 3.1>   Use various R functions and packages to explore the data. This can include summary statistics, correlations, and distributions.

    # <i>  Summary Statistics

# for calculating the summary of the whole datasets

summary(airbnb_data)

# for calculating summary statistics for specific variables

summary(airbnb_data[c("last_review", "availability_365")])


   # <ii>   Correlations 

correlation_matrix <- cor(airbnb_data[, c("price", "reviews_per_month", "availability_365")])
print(correlation_matrix)


#  <iii>   Distributions

  # Histogram
   hist(airbnb_data$availability_365)
   
  # Boxplot
   install.packages("ggplot2")
   library(ggplot2)
   ggplot(airbnb_data, aes(x = room_type, y = price)) +
     geom_boxplot()
   
   
  #  3.2>  Create visualizations using R's plotting capabilities. This can include scatter plots, boxplots, histograms, etc. 
   
   # <i> Scatter Plot
   
   plot(airbnb_data$price, airbnb_data$number_of_reviews, 
        xlab = "Price", ylab = "No. of reviews", 
        main = "Airbnb Price Listings")
   
   # <ii> Boxplots 
   
   boxplot(airbnb_data$availability_365, 
           xlab = "Availability ", 
           main = "Availability Distribution")
   
   #  <iii>  Histogram
   
   hist(airbnb_data$price, 
        xlab = "Price", 
        main = "Price Distribution")
   
   #   <iv>  Line Plot
   
   plot(airbnb_data$availability_365, airbnb_data$price,
        type = "l",
        xlab = "Availability",
        ylab = "Price",
        main = " Availabilty V/s Price")
   
    #  <v>  Pie Graph
   
   pie(table(airbnb_data$room_type),
       main = "Rooms Distribution",
       col = rainbow(length(unique(airbnb_data$room_type))))
   

  
 # 4. **Feature Engineering:** 

 #  - Engineer new features from the existing ones that may be useful for the prediction task. For example, one might use the latitude and longitude data to create a new feature that represents the distance from a popular landmark.

  
   
 install.packages("geosphere")
 library(geosphere)
 
 # Define the coordinates of the popular landmark
 landmark_lat <- 40.1101
 landmark_lon <- -74.5492
 
 # Calculate distance function
 calculate_distance <- function(lat, lon) {
   dist <- distHaversine(cbind(lon, lat), c(landmark_lon, landmark_lat))
   return(dist)
 }
 
 # Assuming you have a data frame named 'airbnb_data' with columns 'latitude' and 'longitude'
 # Create a new feature 'distance_to_landmark' using the calculate_distance function
 airbnb_data$distance_to_landmark <- calculate_distance(airbnb_data$latitude, airbnb_data$longitude)
 
 View(airbnb_data)
 
 
 
   #   5. **Modeling:**

    # 5.1> Split the data into a training set and a testing set.

   install.packages("rsample")
   library(rsample)

   set.seed(100)
   
   # Split the data into 70% training set and 30% testing set
   data_split <- initial_split(airbnb_data, prop = 0.7)
   
   # Get the training set
   training_set <- training(data_split)
   
   # Get the testing set
   testing_set <- testing(data_split)
   
   summary(training_set)
   summary(testing_set)
   
   # Boxplot of a variable
   boxplot(train_data$price, main = "Boxplot of Price in Training Set")
   boxplot(test_data$price, main = "Boxplot of Price in Testing Set")
   
   
  #  5.2> Build a regression model (or other appropriate model) to predict the price of a listing. Consider multiple different types of models, and evaluate their performance.


   
   # Load the required libraries
   install.packages("caret")
   install.packages("randomForest")
   install.packages("gbm")
   # Load required libraries
   # Load the required libraries
   library(caret)
   library(rpart)
   library(randomForest)
   library(gbm)
   
  
  # Gradient Regression
   
   
   # Read the dataset
   airbnb_data <- read.csv('D:/R Programming/AB_NYC_2019.csv')
   
   # Remove rows with missing values
   airbnb_data <- na.omit(airbnb_data)
   
   # Define the features to be used in the model
   features <- c("latitude", "longitude", "room_type", "minimum_nights", "number_of_reviews", "reviews_per_month")
   
   # Subset the data with the selected features
   subset_data <- airbnb_data[, c(features, "price")]
   
   # Split the data into a training set and a testing set
   set.seed(123)
   trainIndex <- createDataPartition(subset_data$price, p = 0.8, list = FALSE)
   train_data <- subset_data[trainIndex, ]
   test_data <- subset_data[-trainIndex, ]
   
   # Build the regression model using Gradient Boosting
   gbm_model <- train(price ~ ., data = train_data, method = "gbm",
                      trControl = trainControl(method = "cv", number = 5))
   
   # Evaluate the model on the test set
   gbm_pred <- predict(gbm_model, newdata = test_data)
   
 
   # 6. **Model Evaluation:** 
   
   #  6.1> Evaluate the model using appropriate metrics and techniques. This can include RMSE, etc. 
   
   
     # Evaluate performance metrics
   gbm_rmse <- caret::RMSE(gbm_pred, test_data$price)
   gbm_mae <- caret::MAE(gbm_pred, test_data$price)
   gbm_r2 <- caret::R2(gbm_pred, test_data$price)
 
   # Print the RMSE value
   cat("Gradient Boosting RMSE:", gbm_rmse, "\n")
   # Print the MAE value 
   cat("Gradient Boosting MAE:", lm_mae, "\n")
   # Print the R2 value
   cat("Gradient Boosting R-squared:", lm_r2, "\n")
   predictions <- data.frame(Actual = test_data$price,
                             GBM = gbm_pred)
  
   
   # 5.3>  Include the visualization of these models using R's capabilities.
   
   # Plot the scatter plot of predictions
   ggplot(predictions, aes(x = Actual, y = GBM)) +
     geom_point(color = "blue") +
     geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
     labs(x = "Actual Price", y = "Predicted Price", title = "Gradient Boosting Model") +
     theme_minimal()
   
   
   
   #Linear Regression
 
   # Read the dataset
   airbnb_data <- read.csv('D:/R Programming/AB_NYC_2019.csv')
   
   # Remove rows with missing values
   airbnb_data <- na.omit(airbnb_data)
   
   # Define the features to be used in the model
   features <- c("latitude", "longitude", "room_type", "minimum_nights", "number_of_reviews", "reviews_per_month")
   
   # Subset the data with the selected features
   subset_data <- airbnb_data[, c(features, "price")]
   
   # Split the data into a training set and a testing set
   set.seed(123)
   trainIndex <- createDataPartition(subset_data$price, p = 0.8, list = FALSE)
   train_data <- subset_data[trainIndex, ]
   test_data <- subset_data[-trainIndex, ]
   
   # Build the regression model using Gradient Boosting
   lm_model <- train(price ~ ., data = train_data, method = "lm",
                      trControl = trainControl(method = "cv", number = 5))
   
   # Evaluate the model on the test set
   lm_pred <- predict(lm_model, newdata = test_data)
 
   
   # 6. **Model Evaluation:** 
   
   #  6.1> Evaluate the model using appropriate metrics and techniques. This can include RMSE, etc. 
   
     
   # Evaluate performance metrics
   lm_rmse <- caret::RMSE(lm_pred, test_data$price)
   lm_mae <- caret::MAE(lm_pred, test_data$price)
   lm_r2 <- caret::R2(lm_pred, test_data$price)
   
   
    # Print the RMSE value
   cat("Linear Regression  RMSE:", lm_rmse, "\n")
   # Print the MAE value 
   cat("Linear Regression MAE:", lm_mae, "\n")
   # Print the R2 value
   cat("Linear Regression R-squared:", lm_r2, "\n")
   
   predictions <- data.frame(Actual = test_data$price,
                             LM = lm_pred)
   
   
   # 5.3>  Include the visualization of these models using R's capabilities.
   library(ggplot2)
   # Plot the scatter plot of predictions
   ggplot(predictions, aes(x = Actual, y = LM)) +
     geom_point(color = "green") +
     geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
     labs(x = "Actual Price", y = "Predicted Price", title = "Linear Regression Model") +
     theme_minimal()
   
   
  
  
  # 6. **Model Evaluation:** 

   #  6.2>  Interpret the model results and document findings. 
   
   # Compare models and select the best-performing one 
   
   # On the basis of RMSE
   
   best_model1 <- which.min(c(lm_rmse, gbm_rmse))
   if (best_model1 == 1) {
     cat("In RMSE metric Linear Regression model performed the best.\n")
   }  else {
     cat("In RMSE metric Gradient Boosting model performed the best.\n")
   }
   # On the basis of MAE
   
   best_model2 <- which.min(c(lm_mae, gbm_mae))
   if (best_model2 == 1) {
     cat("In MAE metric Linear Regression model performed the best.\n")
   }  else {
     cat("In MAE metric Gradient Boosting model performed the best.\n")
   }
   # On the basis of R2
   
   best_model3 <- which.min(c(lm_r2, gbm_r2))
   if (best_model3 == 1) {
     cat("In R2 metric Linear Regression model performed the best.\n")
   }  else {
     cat("In R2 metric Gradient Boosting model performed the best.\n")
   }
   

   
  