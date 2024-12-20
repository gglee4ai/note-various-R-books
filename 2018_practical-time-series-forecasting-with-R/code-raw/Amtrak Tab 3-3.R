library(forecast)

Amtrak.data <- read.csv("Amtrak data.csv")
ridership.ts <- ts(Amtrak.data$Ridership, start = c(1991, 1), end = c(2004, 3), freq = 12)

fixed.nValid <- 36
fixed.nTrain <- length(ridership.ts) - fixed.nValid
stepsAhead <- 1
error <- rep(0, fixed.nValid - stepsAhead + 1)
percent.error <- rep(0, fixed.nValid - stepsAhead + 1)
for(j in fixed.nTrain:(fixed.nTrain + fixed.nValid - stepsAhead)) {
  train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, j))
  valid.ts <- window(ridership.ts, start = c(1991, j + stepsAhead), end = c(1991, j + stepsAhead))
  naive.pred <- naive(train.ts, h = stepsAhead)
  error[j - fixed.nTrain + 1] <- valid.ts - naive.pred$mean[stepsAhead]
  percent.error[j - fixed.nTrain + 1] <- error[j - fixed.nTrain + 1] / valid.ts
}
mean(abs(error))
sqrt(mean(error^2))
mean(abs(percent.error))


fixed.nValid <- 36
fixed.nTrain <- length(ridership.ts) - fixed.nValid
mae <- rep(0,fixed.nValid)
rmse <- rep(0,fixed.nValid)
mape <- rep(0,fixed.nValid)
for(i in 1:36) {
  stepsAhead <- i
  error <- rep(0, fixed.nValid - stepsAhead + 1)
  percent.error <- rep(0, fixed.nValid - stepsAhead + 1)
  for(j in fixed.nTrain:(fixed.nTrain + fixed.nValid - stepsAhead)) {
    train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, j))
    valid.ts <- window(ridership.ts, start = c(1991, j + stepsAhead), end = c(1991, j + stepsAhead))
    naive.pred <- naive(train.ts, h = stepsAhead)
    error[j - fixed.nTrain + 1] <- valid.ts - naive.pred$mean[stepsAhead]
    percent.error[j - fixed.nTrain + 1] <- error[j - fixed.nTrain + 1] / valid.ts
  }
  mae[i] <- mean(abs(error))
  rmse[i] <- sqrt(mean(error^2))
  mape[i] <- mean(abs(percent.error))
}
mean(mae)
mean(rmse)
mean(mape)

train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, fixed.nTrain))
valid.ts <- window(ridership.ts, start = c(1991, fixed.nTrain + 1), end = c(1991, fixed.nTrain + fixed.nValid))
naive.pred <- naive(train.ts, h = fixed.nValid)
snaive.pred <- snaive(train.ts, h = fixed.nValid)
accuracy(naive.pred, valid.ts)
accuracy(snaive.pred, valid.ts)

data.frame(naive.pred$mean)
data.frame(valid.ts)

valid.ts
naive.pred
snaive.pred

fixed.nValid <- 36
fixed.nTrain <- length(ridership.ts) - fixed.nValid
stepsAhead <- 1
error.naive <- rep(0, fixed.nValid - stepsAhead + 1)
percent.error.naive <- rep(0, fixed.nValid - stepsAhead + 1)
error.snaive <- rep(0, fixed.nValid - stepsAhead + 1)
percent.error.snaive <- rep(0, fixed.nValid - stepsAhead + 1)
for(j in fixed.nTrain:(fixed.nTrain + fixed.nValid - stepsAhead)) {
  train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, j))
  valid.ts <- window(ridership.ts, start = c(1991, j + stepsAhead), end = c(1991, j + stepsAhead))
  naive.pred <- naive(train.ts, h = stepsAhead)
  snaive.pred <- snaive(train.ts, h = stepsAhead)
  error.naive[j - fixed.nTrain + 1] <- valid.ts - naive.pred$mean[stepsAhead]
  percent.error.naive[j - fixed.nTrain + 1] <- error.naive[j - fixed.nTrain + 1] / valid.ts
  error.snaive[j - fixed.nTrain + 1] <- valid.ts - snaive.pred$mean[stepsAhead]
  percent.error.snaive[j - fixed.nTrain + 1] <- error.snaive[j - fixed.nTrain + 1] / valid.ts
}
mean(abs(error.naive))
sqrt(mean(error.naive^2))
mean(abs(percent.error.naive))

mean(abs(error.snaive))
sqrt(mean(error.snaive^2))
mean(abs(percent.error.snaive))
