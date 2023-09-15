library(readxl)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(shiny)
library(rsconnect)
library(DT)
library(devtools)
library(lattice)
library(latticeExtra)
library(TSA)
library(tseries)
library(pander)
library(zoo)
library(xts)
library(caret)
library(forecast) # forecasting time series ARIMA 
#library(prophet) # forecasting time series
library(highcharter) #Interactive Plot
library(xgboost)

set.seed(123)

economics

data <- economics %>% dplyr::select(date, unemploy)
data

extended_data <- data %>% 
  rbind(tibble::tibble(date = seq.Date(from = as.Date("2015-05-01"),
                                  by = "month", length.out = 12), 
                       unemploy = rep(NA, 12)))
tail(extended_data)

# Date column. split it into several columns, describing the granularity 
#of the time. 
extended_data_mod <- extended_data %>%
  dplyr::mutate(., 
                months = lubridate::month(date),
                years = lubridate::year(date))

extended_data_mod

#split the data into training set and prediction set
train <- extended_data_mod[1:nrow(data), ] # initial data

pred <- extended_data_mod[(nrow(data) + 1):nrow(extended_data), ] # extended time index

# In order to use xgboost we need to transform the data into a matrix form 
#and extract the target variable. Additionally we need to get rid of the 
#dates columns and just use the newly created ones
x_train <- (data.matrix(train %>%
            dplyr::select(months, years)))
x_pred <- (data.matrix(pred %>% 
          dplyr::select(months, years)))

y_train <- train$unemploy

#__xgboost prediction__
xgb_trcontrol <- caret::trainControl(
  method = "cv", 
  number = 5,
  allowParallel = TRUE, 
  verboseIter = FALSE, 
  returnData = FALSE
)

xgb_grid <- base::expand.grid(
  list(
    nrounds = c(100, 200),
    max_depth = c(10, 15, 20), # maximum depth of a tree
    colsample_bytree = seq(0.5), # subsample ratio of columns when construction each tree
    eta = 0.1, # learning rate
    gamma = 0, # minimum loss reduction
    min_child_weight = 1,  # minimum sum of instance weight (hessian) needed ina child
    subsample = 1 # subsample ratio of the training instances
  ))

# build the model using the tree models
xgb_model <- caret::train(
  x_train, y_train,
  trControl = xgb_trcontrol,
  tuneGrid = xgb_grid,
  method = "xgbTree",
  nthread = 1,
  verbose = FALSE,
  verbosity = 0
)

#check the best values that were chosen as hyperparameters
xgb_model$bestTune

# perform the forecast
xgb_pred <- xgb_model %>% stats::predict(x_pred)
xgb_pred

# _____forecast object_______
# prediction on a train set
fitted <- xgb_model %>%
  stats::predict(x_train) %>%
  stats::ts(start = zoo::as.yearmon(min(train$date)), 
            end = zoo::as.yearmon(max(train$date)),
            frequency = 12)

# prediction in a form of ts object
xgb_forecast <- xgb_pred %>%
  stats::ts(start = zoo::as.yearmon(min(pred$date)),
            end = zoo::as.yearmon(max(pred$date)),
            frequency = 12)

# original data as ts object
ts <- y_train %>% 
  stats::ts(start = zoo::as.yearmon(min(train$date)), 
            end = zoo::as.yearmon(max(train$date)), 
            frequency = 12)


# forecast object
forecast_list <- list(
  model = xgb_model$modelInfo,
  method = xgb_model$method,
  mean = xgb_forecast,
  x = ts, 
  fitted = fitted,
  residuals = as.numeric(ts) - as.numeric(fitted)
)
class(forecast_list) <- "forecast"

forecast_list

forecast::autoplot(forecast_list)









