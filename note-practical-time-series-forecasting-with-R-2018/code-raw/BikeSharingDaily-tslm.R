library(forecast)
library(lubridate)

# Load and prepare data.
bike.df <- read.csv("BikeSharingDaily.csv")
bike.df$Date <- as.Date(bike.df$dteday, format = "%Y-%m-%d")
bike.df$Month <- month(bike.df$Date, label = TRUE)
bike.df$DOW <- wday(bike.df$Date, label = TRUE)
bike.df$WorkingDay <- factor(bike.df$workingday, levels = c(0, 1), labels = c("Not_Working", "Working"))
bike.df$Weather <- factor(bike.df$weathersit, levels = c(1, 2, 3), labels = c("Clear", "Mist", "Rain_Snow"))

# Create dummy variables.
Month.dummies <- model.matrix(~ 0 + Month, data = bike.df)
DOW.dummies <- model.matrix(~ 0 + DOW, data = bike.df)
WorkingDay_Weather.dummies <- model.matrix(~ 0 + WorkingDay:Weather, data = bike.df)

# Change the names of the dummy variables.
colnames(Month.dummies) <- gsub("Month", "", colnames(Month.dummies))
colnames(DOW.dummies) <- gsub("DOW", "", colnames(DOW.dummies))
colnames(WorkingDay_Weather.dummies) <- gsub("WorkingDay", "", colnames(WorkingDay_Weather.dummies))
colnames(WorkingDay_Weather.dummies) <- gsub("Weather", "", colnames(WorkingDay_Weather.dummies))
colnames(WorkingDay_Weather.dummies) <- gsub(":", "_", colnames(WorkingDay_Weather.dummies))

# Set up training and validation sets.
x <- as.data.frame(cbind(Month.dummies[, -12], DOW.dummies[, -7], WorkingDay_Weather.dummies[, -6]))
y <- bike.df$cnt
nTotal <- length(y)
nValid <- 90
nTrain <- nTotal - nValid
xTrain <- x[1:nTrain, ]
yTrain <- y[1:nTrain]
xValid <- x[(nTrain + 1):nTotal, ]
yValid <- y[(nTrain + 1):nTotal]

# Fit tslm.
yTrain.ts <- ts(yTrain)
(formula <- as.formula(paste("yTrain.ts", paste(c("trend", colnames(xTrain)), collapse = "+"), sep = "~")))
bike.tslm <- tslm(formula, data = xTrain, lambda = 1)
options(scipen = 999, digits = 6)
summary(bike.tslm)

# Make tslm forecasts.
bike.tslm.pred <-  forecast(bike.tslm, newdata = xValid)
accuracy(bike.tslm.pred$mean, yValid) 

pdf("NewFig-tslm.pdf")
plot(bike.tslm.pred, ylim = c(0, 9000), xlab = "Days", ylab = "Daily Bike Rentals")
dev.off()


# Fit tbats and make forecasts.
y.ts <- ts(y)
times.ts <- time(y.ts)
y.msts <- msts(y, seasonal.periods = c(7, 365.25))
times.msts <- time(y.msts)
yTrain.msts <- msts(yTrain, seasonal.periods = c(7, 365.25))
bike.tbats <- tbats(yTrain.msts, use.parallel = TRUE, num.cores = 12)
bike.tbats 
bike.tbats.pred <- forecast(bike.tbats, h = nValid)
accuracy(bike.tbats.pred$mean, yValid)

# Compare the forecasts of tslm and tbats.
par(mfrow = c(1, 2))
plot(bike.lm.pred, ylim = c(0, 9000))
# lines(window(y.ts, start = times.ts[nValid + 1]))
plot(bike.tbats.pred, ylim = c(0, 9000))
# lines(window(y.msts, start = times.msts[nValid + 1]))
