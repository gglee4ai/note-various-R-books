library(forecast)

Amtrak.data <- read.csv("Amtrak data.csv")
ridership.ts <- ts(Amtrak.data$Ridership, start = c(1991, 1), end = c(2004, 3), freq = 12)


nValid <- 36
nTrain <- length(ridership.ts) - nValid
train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts <- window(ridership.ts, start = c(1991, nTrain + 1), end = c(1991, nTrain + nValid))
ridership.lm <-  tslm(train.ts ~ trend + I(trend^2))
ridership.lm.pred <- forecast(ridership.lm, h = nValid, level = 0)

res <- round(valid.ts - ridership.lm.pred$mean, digits = 3)
cbind(ridership.lm.pred$mean, valid.ts, res)

valid.ts <- window(ridership.ts, start = c(1991, nTrain + 1), end = c(1991, nTrain + nValid))
accuracy(ridership.lm.pred$mean, valid.ts)
