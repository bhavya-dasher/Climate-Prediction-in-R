data = read.csv('F:/R_practice_ques/Bhavya_Project/DailyDelhiClimateTest.csv')
data
head(data)

rows <- nrow(data)  # Number of rows
cols <- ncol(data)  # Number of columns
rows
cols

data_types <- sapply(data, class)
data_types

humidity_summary <- summary(data$humidity)
humidity_sd <- sd(data$humidity)  # Standard deviation
humidity_count <- length(data$humidity)  # Count

humidity_summary
humidity_sd
humidity_count


missing_percentage <- (colSums(is.na(data)) / nrow(data)) * 100
missing_percentage

library(forecast)

# Assuming 'data' is your data frame with 'timestamp' and 'humidity' columns
# Create a time series object from your data
time_series <- ts(data$humidity, frequency = 1)  # Assumes a frequency of 1 (e.g., daily data)
time_series
# Fit an ARIMA model to your data
arima_model <- auto.arima(time_series)

# Define the forecast horizon (e.g., 30 days)
forecast_horizon <- 30

# Generate a forecast for the future
humidity_forecast <- forecast(arima_model, h = forecast_horizon)

# Print the forecasted values
print(humidity_forecast)


#I have used the Autocorrelation function and partial autocorrelation function plots to determine the parameters of the model.
library(ggplot2)
# Plot the Autocorrelation Function (ACF)
acf_plot <- acf(data$humidity, lag.max = 150, plot = FALSE)
plot(acf_plot, main = "ACF", alpha = 0.05)

# Plot the Partial Autocorrelation Function (PACF)
pacf_plot <- pacf(data$humidity, lag.max = 150, plot = FALSE)
plot(pacf_plot, main = "PACF", alpha = 0.05)

