library(forecast) 

tumblr.data <- read.csv("Tumblr.csv")
people.ts <- ts(tumblr.data$People.Worldwide) / 1000000

# Run three exponential smoothing models: AAN, MMN, and MMdN.
# MMN stands for Multiplicative error, Multiplicative trend, and No seasonality. MMdN stands for Multiplicative error, Multiplicative damped trend, and No seasonality.
people.ets.AAN <- ets(people.ts, model = "AAN")
people.ets.MMN <- ets(people.ts, model = "MMN", damped = FALSE)
people.ets.MMdN <- ets(people.ts, model = "MMN", damped = TRUE)

# Create their prediction "cones" for 115 months into the future (Jun 2013 to Dec 2022).
people.ets.AAN.pred <- forecast(people.ets.AAN, h = 115, level = c(0.2, 0.4, 0.6, 0.8))
people.ets.MMN.pred <- forecast(people.ets.MMN, h = 115, level = c(0.2, 0.4, 0.6, 0.8))
people.ets.MMdN.pred <- forecast(people.ets.MMdN, h = 115, level = c(0.2, 0.4, 0.6, 0.8))

# Compare the three models' "forecast cones" visually.

pdf("TumblerNewFig3-6.pdf")
par(mfrow = c(1, 3)) # This command sets the plot window to show 1 row of 3 plots.
plot(people.ets.AAN.pred, xlab = "Month", ylab = "People (in millions)", ylim = c(0, 1000))
plot(people.ets.MMN.pred, xlab = "Month", ylab="People (in millions)", ylim = c(0, 1000))
plot(people.ets.MMdN.pred, xlab = "Month", ylab="People (in millions)", ylim = c(0, 1000))
dev.off()

# Examine the lower and upper limits of the MMN model's prediction cones.
people.ets.MMN.pred$lower 
people.ets.MMN.pred$upper
