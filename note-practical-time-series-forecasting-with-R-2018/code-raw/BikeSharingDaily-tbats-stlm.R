library(forecast)

bike.daily.df <- read.csv("BikeSharingDaily.csv")
bike.daily.msts <- msts(bike.daily.df$cnt, seasonal.periods = c(7, 365.25))

bike.daily.tbats <- tbats(bike.daily.msts)
bike.daily.tbats.pred <- forecast(bike.daily.tbats, h = 365)

bike.daily.stlm <- stlm(bike.daily.msts, s.window = "periodic")
bike.daily.stlm.pred <- forecast(bike.daily.stlm, h = 365)

pdf("NewFig-tbats-stlm.pdf")
par(mfrow = c(2, 1))
plot(bike.daily.tbats.pred, ylim = c(0, 9000), xlab = "Year", ylab = "Daily Bike Rentals",main = "TBATS")
plot(bike.daily.stlm.pred, ylim = c(0, 9000), xlab = "Year", ylab = "Daily Bike Rentals",main = "STL + ETS")
dev.off()

