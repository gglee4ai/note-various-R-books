library(forecast)
library(lubridate)

bike.hourly.df <- read.csv("BikeSharingHourly.csv")
nTotal <- length(bike.hourly.df$cnt[13004:13747])  # 31 days * 24 hours/day = 744 hours
bike.hourly.msts <- msts(bike.hourly.df$cnt[13004:13747], seasonal.periods = c(24, 168), start = c(0, 1))

nTrain <- 21 * 24  # 21 days of hourly data
nValid <- nTotal - nTrain  # 10 days of hourly data
yTrain.msts <- window(bike.hourly.msts, start = c(0, 1), end = c(0, nTrain))
yValid.msts <- window(bike.hourly.msts, start = c(0, nTrain + 1), end = c(0, nTotal))

bike.hourly.dshw.pred <- dshw(yTrain.msts, h = nValid)
bike.hourly.dshw.pred.mean <- msts(bike.hourly.dshw.pred$mean, seasonal.periods = c(24, 168), start = c(0, nTrain + 1))
accuracy(bike.hourly.dshw.pred.mean, yValid.msts)

pdf("NewFig-dshw.pdf")
plot(yTrain.msts, xlim = c(0, 4 + 3/7), xlab = "Week", ylab = "Hourly Bike Rentals")
lines(bike.hourly.dshw.pred.mean, lwd = 2, col = "blue")
dev.off()

# Compare to stlm and tbats.
bike.hourly.stlm <- stlm(yTrain.msts, lambda = 0)
bike.hourly.stlm.pred <- forecast(bike.hourly.stlm, h = nValid)
bike.hourly.tbats <- tbats(yTrain.msts)
bike.hourly.tbats.pred <- forecast(bike.hourly.tbats, h = nValid) 

par(mfrow = c(1, 3))
plot(yTrain.msts, xlim = c(0,4 + 3/7), ylab = "Hourly Bike Rentals", ylim = c(0, 1000))
lines(bike.hourly.dshw.pred.mean, lwd = 2, col = "blue")
plot(bike.hourly.stlm.pred, ylim = c(0, 1000))
plot(bike.hourly.tbats.pred, ylim = c(0, 1000))