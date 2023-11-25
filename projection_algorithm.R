pfx_x_proj <- test %>% 
  select(-pfx_z)

pfx_x_proj <- na.omit(pfx_x_proj)

pfx_z_proj <- test %>% 
  select(-pfx_x)

pfx_z_proj <- na.omit(pfx_z_proj)

library(xgboost)
library(caret)
library(caTools)


# pfx_x
sample <- sample.split(pfx_x_proj$pfx_x, SplitRatio = 0.8)
train  <- subset(pfx_x_proj, sample == TRUE)
test   <- subset(pfx_x_proj, sample == FALSE)

train_x = data.matrix(train[, -18])
train_y = train[, 18]

test_x = data.matrix(test[, -18])
test_y = test[, 18]

xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

xgb_pfx_x = xgboost(data = xgb_train, max.depth = 3, nrounds = 150)
pred_y = predict(xgb_pfx_x, xgb_test)

mse = mean((test_y - pred_y)^2)
mae = caret::MAE(test_y, pred_y)
rmse = caret::RMSE(test_y, pred_y)

cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)

compare <- cbind(test_y, pred_y)

compare <- as.data.frame(compare) %>% 
  mutate(diff = as.numeric(test_y) - as.numeric(pred_y))
# average difference: 0.0005669497

ggplot(compare, aes(x = test_y, y = pred_y)) +
  geom_point()

ggplot(compare, aes(x = diff)) + 
  geom_density()




# pfx_z
sample <- sample.split(pfx_z_proj$pfx_z, SplitRatio = 0.8)
train  <- subset(pfx_z_proj, sample == TRUE)
test   <- subset(pfx_z_proj, sample == FALSE)

train_x = data.matrix(train[, -18])
train_y = train[, 18]

test_x = data.matrix(test[, -18])
test_y = test[, 18]

xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

xgb_pfx_z = xgboost(data = xgb_train, max.depth = 3, nrounds = 150)
pred_y = predict(xgb_pfx_z, xgb_test)

mse = mean((test_y - pred_y)^2)
mae = caret::MAE(test_y, pred_y)
rmse = caret::RMSE(test_y, pred_y)

cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)

compare <- cbind(test_y, pred_y)

compare <- as.data.frame(compare) %>% 
  mutate(diff = as.numeric(test_y) - as.numeric(pred_y))
# average difference: 0.0003476689

ggplot(compare, aes(x = test_y, y = pred_y)) +
  geom_point()

ggplot(compare, aes(x = diff)) + 
  geom_density()
