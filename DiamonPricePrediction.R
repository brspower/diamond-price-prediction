#libraries needed
library(caret)
library(class)
library(dplyr)
library(e1071)
library(FNN) 
library(gmodels) 
library(psych)


diamonds <- read.csv("diamonds.csv")

data_reg <- diamonds

price_outcome <- data_reg %>% select(price)

data_reg <- data_reg %>% select(-price)

data_reg <- data_reg %>% select(-X)

data_reg[, c("carat", "depth", "table", "x", "y", "z")] <- scale(data_reg[, c("carat", "depth", "table", "x", "y", "z")])

str(data_reg)

data_reg$cut <- dummy.code(data_reg$cut)
data_reg$color <- dummy.code(data_reg$color)
data_reg$clarity <- dummy.code(data_reg$clarity)

print(head(data_reg))

set.seed(1234)

smp_size <- floor(0.75 * nrow(data_reg))

train_ind <- sample(seq_len(nrow(data_reg)), size = smp_size)

# creating test and training sets that contain all of the predictors
reg_pred_train <- data_reg[train_ind, ]
reg_pred_test <- data_reg[-train_ind, ]

price_outcome_train <- price_outcome[train_ind, ]
price_outcome_test <- price_outcome[-train_ind, ]

reg_results <- knn.reg(reg_pred_train, reg_pred_test, price_outcome_train, k = 10)
print(reg_results)

plot(price_outcome_test, reg_results$pred, xlab="y", ylab=expression(hat(y)))

#mean square prediction error
print("Mean Square prediction error")
print(mean((price_outcome_test - reg_results$pred) ^ 2))

#mean absolute error
print("Mean Absolute error")
print(mean(abs(price_outcome_test - reg_results$pred)))

rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}

# define helper function for getting knn.reg predictions
# note: this function is highly specific to this situation and dataset
make_knn_pred = function(k = 1, training, predicting, price_outcome_test, price_outcome_train) {
  pred = knn.reg(train = training, test = predicting, price_outcome_train, k = k)$pred
  act  = price_outcome_test
  rmse(predicted = pred, actual = act)
}

k = c(1, 5, 10, 25, 50, 250)

# get requested train RMSEs
knn_trn_rmse = sapply(k, make_knn_pred, 
                      training = reg_pred_train, 
                      predicting = reg_pred_train,
                      price_outcome_train = price_outcome_train,
                      price_outcome_test = price_outcome_test)
# get requested test RMSEs
knn_tst_rmse = sapply(k, make_knn_pred, 
                            training = reg_pred_train, 
                            predicting = reg_pred_test,
                            price_outcome_train = price_outcome_train,
                            price_outcome_test = price_outcome_test)

# determine "best" k
best_k = k[which.min(knn_tst_rmse)]

# find overfitting, underfitting, and "best"" k
fit_status = ifelse(k < best_k, "Over", ifelse(k == best_k, "Best", "Under"))

# summarize results
knn_results = data.frame(
  k,
  round(knn_trn_rmse, 2),
  round(knn_tst_rmse, 2),
  fit_status
)
colnames(knn_results) = c("k", "Train RMSE", "Test RMSE", "Fit?")

# display results
knitr::kable(knn_results, escape = FALSE, booktabs = TRUE)