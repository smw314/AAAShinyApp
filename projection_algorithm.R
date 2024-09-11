# Load required libraries
library(tidyverse)
library(xgboost)
library(caret)
library(caTools)

# Function to prepare data and train XGBoost model
train_xgboost_model <- function(data, target_col, max_depth = 3, nrounds = 150) {
  # Remove rows with NA values
  data <- na.omit(data)
  
  # Split data into train and test sets
  set.seed(123)  # for reproducibility
  sample <- sample.split(data[[target_col]], SplitRatio = 0.8)
  train  <- subset(data, sample == TRUE)
  test   <- subset(data, sample == FALSE)
  
  # Prepare matrices for XGBoost
  train_x <- data.matrix(train[, -which(names(train) == target_col)])
  train_y <- train[[target_col]]
  test_x <- data.matrix(test[, -which(names(test) == target_col)])
  test_y <- test[[target_col]]
  
  # Create DMatrix objects
  xgb_train <- xgb.DMatrix(data = train_x, label = train_y)
  xgb_test <- xgb.DMatrix(data = test_x, label = test_y)
  
  # Train XGBoost model
  model <- xgboost(data = xgb_train, max.depth = max_depth, nrounds = nrounds)
  
  # Make predictions
  pred_y <- predict(model, xgb_test)
  
  # Calculate metrics
  mse <- mean((test_y - pred_y)^2)
  mae <- MAE(test_y, pred_y)
  rmse <- RMSE(test_y, pred_y)
  
  # Create comparison dataframe
  compare <- data.frame(
    actual = test_y,
    predicted = pred_y,
    diff = test_y - pred_y
  )
  
  # Return results
  list(model = model, metrics = c(MSE = mse, MAE = mae, RMSE = rmse), compare = compare)
}

# Plot results
plot_results <- function(compare, title) {
  p1 <- ggplot(compare, aes(x = actual, y = predicted)) +
    geom_point(alpha = 0.5) +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    labs(title = paste(title, "- Actual vs Predicted"),
         x = "Actual", y = "Predicted")
  
  p2 <- ggplot(compare, aes(x = diff)) + 
    geom_density(fill = "blue", alpha = 0.5) +
    labs(title = paste(title, "- Prediction Error Distribution"),
         x = "Prediction Error", y = "Density")
  
  list(scatter = p1, density = p2)
}

# Train and evaluate models
pfx_x_results <- train_xgboost_model(test, "pfx_x")
pfx_z_results <- train_xgboost_model(test, "pfx_z")

# Print metrics
cat("pfx_x metrics:\n")
print(pfx_x_results$metrics)
cat("\npfx_z metrics:\n")
print(pfx_z_results$metrics)

# Plot results
pfx_x_plots <- plot_results(pfx_x_results$compare, "pfx_x")
pfx_z_plots <- plot_results(pfx_z_results$compare, "pfx_z")

# Display plots
pfx_x_plots$scatter
pfx_x_plots$density
pfx_z_plots$scatter
pfx_z_plots$density

# Feature importance
importance_x <- xgb.importance(model = pfx_x_results$model)
importance_z <- xgb.importance(model = pfx_z_results$model)

# Plot feature importance
xgb.plot.importance(importance_x, top_n = 10, main = "Top 10 important features for pfx_x")
xgb.plot.importance(importance_z, top_n = 10, main = "Top 10 important features for pfx_z")
