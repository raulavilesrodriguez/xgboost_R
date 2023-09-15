library(xgboost)
library(caret)

boston = MASS::Boston
str(boston)

set.seed(12)

indexes <- createDataPartition(boston$medv, p = .85, list = F)
train <- boston[indexes, ]
test <- boston[-indexes, ]

train_x <- data.matrix(train[, -13])
train_y <- train[,13]

test_x <- data.matrix(test[, -13])
test_y <- test[, 13]

xgb_train <- xgb.DMatrix(data = train_x, label = train_y)
xgb_test <- xgb.DMatrix(data = test_x, label = test_y)


