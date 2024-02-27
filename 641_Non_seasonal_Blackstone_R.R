library(forecast)
library(tseries)
library(TSA)
library(astsa)
library(tidyverse)
library(xts)
library(ggplot2)
library(plotly)
library(rugarch)

data <- read.csv('/Users/seimonpatni/Desktop/MA641_Project datasets/BX.csv')

data 
# Data description
summary(data)
str(data)

# Check for null values
sum(is.na(data))

# Step 1: Data Visualization
data$Date <- as.POSIXct(data$Date)

ggplot(data, aes(x = Date, y = Close)) +
  geom_line() +
  labs(title = "Blackstone Stock Prices Over Time",
       x = "Date",
       y = "Closing Price") +
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")

# Distribution of Closing Prices
ggplot(data, aes(x = Close)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Blackstone Closing Prices",
       x = "Closing Price",
       y = "Frequency")

# Candlestick Plot
plot_ly(data, type = "candlestick", x = ~Date, open = ~Open, close = ~Close, high = ~High, low = ~Low) %>%
  layout(title = "Blackstone Stock Candlestick Plot",
         xaxis = list(title = "Date"),
         yaxis = list(title = "Stock Price"))

# Check if data is stationary
adf_test_result <- adf.test(data$Close)
cat("ADF Test p-value:", adf_test_result$p.value, "\n")

# apply differencing
if (adf_test_result$p.value >= 0.05) {
  # Calculate first differences
  data$Close_diff <- c(NA, diff(data$Close))
  
  # Remove NA for ADF test
  adf_test_result_diff <- adf.test(na.omit(data$Close_diff))
  
  cat("ADF Test p-value after differencing:", adf_test_result_diff$p.value, "\n", "Data is now stationary", "\n")
} else {
  cat("The dataset is stationary.\n")
}


data <- na.omit(data)
# Calculate ACF and PACF
acf_values <- acf(data$Close_diff, plot = FALSE)
pacf_values <- pacf(data$Close_diff, plot = FALSE)
par(mfrow = c(1, 2))
plot(acf_values, main = "Autocorrelation Function (ACF)", lwd = 2)
plot(pacf_values, main = "Partial Autocorrelation Function (PACF)", lwd = 2)

# Calculate EACF
eacf_values <- eacf(data$Close_diff)

# Range of p and q values to iterate over
p_values <- seq(0, 4)
d_values <- 1  # Assuming you've differenced the data once
q_values <- seq(0, 4)

best_aic <- Inf
best_bic <- Inf
best_params <- NULL

# Grid search for ARIMA parameters
for (p in p_values) {
  for (q in q_values) {
    order <- c(p, d_values, q)
    tryCatch({
      model <- Arima(data$Stationary_Close, order = order)
      aic <- AIC(model)
      bic <- BIC(model)
      
      # Print information for troubleshooting
      cat("Trying order:", order, " | AIC:", aic, " | BIC:", bic, "\n")
      
      # Compare AIC and BIC to find the best-fit model
      if (aic < best_aic && bic < best_bic) {
        best_aic <- aic
        best_bic <- bic
        best_params <- order
      }
    }, error = function(e) {
      cat("Error for order:", order, " - ", conditionMessage(e), "\n")
    })
  }
}

cat("Best ARIMA Model Parameters (p, d, q):", best_params, "\n")
cat("Best AIC:", best_aic, "\n")
cat("Best BIC:", best_bic, "\n")

# If best_params is still NULL, print a message indicating the issue
if (is.null(best_params)) {
  cat("No valid ARIMA model parameters found.\n")
}

# Fit the best model
best_model <- Arima(data$Stationary_Close, order = best_params)
# Check for Parameter Redundancy
check_parameters <- checkresiduals(best_model)
# Print the check results
cat("Parameter Redundancy Check:\n")
print(check_parameters)

# Check residuals
residuals <- residuals(best_model)
checkresiduals(residuals)

#forecasting
last_date <- max(data$Date)
last_date
forecast_horizon <- 60
future_dates <- seq(last_date + 1, by = "day", length.out = forecast_horizon)

forecast_values <- forecast(best_model, h = forecast_horizon)

# Back-transform forecasted values
last_observation <- tail(data$Close, 1)
forecast_values_original <- diffinv(forecast_values$mean, xi = last_observation)

# Create a sequence of dates for the forecast
forecast_dates <- seq(max(data$Date) + 1, by = "days", length.out = length(forecast_values_original))

# Plot the original time series and forecast
plot(data$Date, data$Close, type = "l", col = "blue", lwd = 2,
     main = "ARIMA Forecast with Back-Transformation",
     xlab = "Date", ylab = "Original Value")

lines(forecast_dates, forecast_values_original, col = "red", lty = 2, lwd = 2)
legend("topright", legend = c("Original", "Forecast"), col = c("blue", "red"), lty = 1:2, cex = 0.8)

