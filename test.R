library(FNN)
library(MASS)
library(knitr)

diamonds <- read.csv("diamonds.csv")

x_diamonds = diamonds["x"]
y_diamonds = diamonds$price

lstat_grid = data.frame(lstat = seq(range(x_diamonds$x)[1], range(x_diamonds$x)[2], by = 0.01))
  
pred_001 = FNN::knn.reg(train = x_diamonds, test = lstat_grid, y = y_diamonds, k = 1)
pred_005 = FNN::knn.reg(train = x_diamonds, test = lstat_grid, y = y_diamonds, k = 5)
pred_010 = FNN::knn.reg(train = x_diamonds, test = lstat_grid, y = y_diamonds, k = 10)
pred_025 = FNN::knn.reg(train = x_diamonds, test = lstat_grid, y = y_diamonds, k = 25)
pred_050 = FNN::knn.reg(train = x_diamonds, test = lstat_grid, y = y_diamonds, k = 50)
pred_100 = FNN::knn.reg(train = x_diamonds, test = lstat_grid, y = y_diamonds, k = 100)


par(mfrow = c(3, 2))

plot(price ~ x, data = diamonds, cex = .8, col = "dodgerblue", main = "k = 1")
lines(lstat_grid$lstat, pred_001$pred, col = "darkorange", lwd = 0.25)

plot(price ~ x, data = diamonds, cex = .8, col = "dodgerblue", main = "k = 5")
lines(lstat_grid$lstat, pred_005$pred, col = "darkorange", lwd = 0.75)

plot(price ~ x, data = diamonds, cex = .8, col = "dodgerblue", main = "k = 10")
lines(lstat_grid$lstat, pred_010$pred, col = "darkorange", lwd = 1)

plot(price ~ x, data = diamonds, cex = .8, col = "dodgerblue", main = "k = 25")
lines(lstat_grid$lstat, pred_025$pred, col = "darkorange", lwd = 1.5)

plot(price ~ x, data = diamonds, cex = .8, col = "dodgerblue", main = "k = 50")
lines(lstat_grid$lstat, pred_050$pred, col = "darkorange", lwd = 2)

plot(price ~ x, data = diamonds, cex = .8, col = "dodgerblue", main = "k = 100")
lines(lstat_grid$lstat, pred_100$pred, col = "darkorange", lwd = 2)