library(FNN)

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
