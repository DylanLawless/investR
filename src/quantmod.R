# Install and load the quantmod package
# if (!requireNamespace("quantmod", quietly = TRUE))
    # install.packages("quantmod")



library(quantmod)
library(ggplot2)
library(dplyr)
library(tidyr)

# Set the stock symbol for a company (e.g., AAPL for Apple Inc.)
symbol <- "AAPL"

# Get stock data from Yahoo Finance
getSymbols(symbol, src = "yahoo", from = "2018-01-01", to = "2020-12-31")

# Display the head of the data to inspect what was retrieved
print("Initial data retrieved:")
head(AAPL)


# transform ----

# Convert the xts object to a dataframe for ggplot compatibility
aapl_df <- data.frame(Date = index(AAPL), coredata(AAPL))
print("Dataframe version of the stock data:")
print(head(aapl_df))

# Create Plots Individually ----

# Function to plot a single financial metric
plot_metric <- function(df, metric) {
	ggplot(df, aes_string(x = "Date", y = metric)) +
		geom_line() +
		labs(title = paste(metric, "Over Time"), x = "Date", y = metric) +
		theme_minimal()
}

# Plotting each metric individually
plot_open <- plot_metric(aapl_df, "AAPL.Open")
plot_high <- plot_metric(aapl_df, "AAPL.High")
plot_low <- plot_metric(aapl_df, "AAPL.Low")
plot_close <- plot_metric(aapl_df, "AAPL.Close")
plot_volume <- plot_metric(aapl_df, "AAPL.Volume")
plot_adjusted <- plot_metric(aapl_df, "AAPL.Adjusted")

# Print plots to check them
(plot_open | plot_high | plot_low) / ( plot_close | plot_volume | plot_adjusted)

# Identifying Local Maxima (Potential Selling Points) ----

# Using quantmod to identify local peaks in the close price
aapl_peaks <- findPeaks(AAPL$AAPL.Close)

# Plotting with highlighted peaks
p_selling_points <- ggplot(aapl_df, aes(x = Date, y = AAPL.Close)) + 
	geom_line() +
	geom_point(data = aapl_df[aapl_peaks, ], aes(x = Date[aapl_peaks], y = AAPL.Close[aapl_peaks]), colour = "red") +
	labs(title = "AAPL Close Price with Selling Points Highlighted", y = "Close Price")

print(p_selling_points)

# Fitting the ARIMA Model ----

library(forecast)
# Prepare the Close prices data for ARIMA
aapl_close <- Cl(AAPL)  # Extracting the 'Close' prices

# Fit an ARIMA model
arima_model <- auto.arima(aapl_close, seasonal = FALSE)  # Assuming non-seasonal data

# Summary of the model
summary(arima_model)


# Interpretation and Comments
# Model Fit: The summary(arima_model) will provide insights into the quality of the fit, including residuals and coefficients. A good fit suggests the model can reasonably forecast future values.
# Forecasting: The forecast shows the expected price movement for AAPL over the next 90 days. The confidence intervals around the predictions indicate the uncertainty in the forecast.
# Potential Highs: By examining the forecast plot, look for peaks in the forecasted data which might suggest good potential selling points.
# Plot Insights: The combined plot of historical and forecasted data helps visualize how the model extends the existing trends into the future, providing a graphical interpretation of potential future highs.

# ### Model Specification
# - **Series**: This indicates the data that the model was fitted on, here `aapl_close`.
# - **ARIMA(5,2,0)**: This specifies the type of ARIMA model used. The parameters (5,2,0) indicate:
#   - **5** as the order of the autoregressive part (AR). This means the model uses the previous five values in the prediction.
#   - **2** as the degree of differencing (d). This means the data has been differenced twice to make it stationary.
#   - **0** as the order of the moving average part (MA). This means there is no moving average component in the model.
# 
# ### Coefficients
# - **ar1 to ar5**: These are the coefficients for the five autoregressive terms. They show the relationship between the current value and the values at previous time steps.
#   - For instance, `ar1 = -0.9666` suggests a strong negative correlation with the first lag.
# - **s.e.**: These are the standard errors associated with each autoregressive coefficient, indicating the precision of the coefficient estimates.
# 
# ### Model Fit Statistics
# - **sigma^2 = 3.149**: This is the variance of the residuals, showing the average squared deviations of the predictions from the actual values, which is a measure of the noise in the model.
# - **Log likelihood = -1498.38**: This value measures the likelihood of the model given the data, where higher values (less negative) are better as they indicate a model that better fits the data.
# - **AIC = 3008.76**: The Akaike Information Criterion, a measure of the model quality that penalizes complexity; lower values are better.
# - **AICc = 3008.87**: The corrected Akaike Information Criterion for small sample sizes.
# - **BIC = 3036.5**: The Bayesian Information Criterion, another measure of model quality that also penalizes complexity but tends to favor simpler models than AIC; again, lower values are better.
# 
# ### Training Set Error Measures
# These are metrics calculated on the training dataset to measure the accuracy of the model:
# - **ME (Mean Error)**: The average of the residuals (forecast errors). Here it is very close to zero, indicating no bias.
# - **RMSE (Root Mean Squared Error)**: Measures the standard deviation of the residuals; lower values indicate a better fit.
# - **MAE (Mean Absolute Error)**: The average of the absolute values of the residuals; it’s similar to RMSE but less sensitive to outliers.
# - **MPE (Mean Percentage Error)**: The average of the percentage errors; close to zero which indicates good model accuracy on average.
# - **MAPE (Mean Absolute Percentage Error)**: Like MAE but in percentage terms; lower values indicate better fit.
# - **MASE (Mean Absolute Scaled Error)**: Compares the MAE to the MAE of a naïve model; values less than one indicate a model performing better than a naïve model.
# - **ACF1**: The first autocorrelation of residuals; should be near zero for a good model which indicates that residuals are white noise.
# 
# ### Interpretation
# This ARIMA model shows reasonable fit metrics and suggests that past values significantly influence the current value of AAPL's closing price, as indicated by the autoregressive terms. The low AIC and BIC values relative to other models would generally suggest a good fit, though it's always recommended to compare against other models and validate the model on out-of-sample data. The error metrics provide a comprehensive overview of the model’s predictive accuracy, showing it performs adequately in terms of forecasting accuracy.
# Maxima gainers -----

# Calculate differences ----

# Calculate percent change and differences correctly
# Calculate percent change and differences correctly
aapl_df <- aapl_df %>%
	mutate(
		Close_Diff = c(NA, diff(AAPL.Close)),  # Difference from previous day
		Close_Pct_Change = c(NA, AAPL.Close[-1] / AAPL.Close[-length(AAPL.Close)] - 1)  # Percentage change from previous day
	)

# Check the modified dataframe
print(head(aapl_df, 10))

# Classify changes ----
aapl_df <- aapl_df %>%
	mutate(
		Trend = case_when(
			Close_Diff > 0 ~ "Increase",
			Close_Diff < 0 ~ "Decrease",
			TRUE ~ "No Change"
		)
	)

print("Trends classified:")
print(head(aapl_df))

# Feature engineering ----
aapl_df <- aapl_df %>%
	mutate(
		MA_7 = rollmean(AAPL.Close, 7, fill = NA),  # 7-day moving average
		MA_30 = rollmean(AAPL.Close, 30, fill = NA)  # 30-day moving average
	)

print("Features for prediction engineered:")
print(head(aapl_df))

# Predictive modeling ----

# Using the features, build a model to predict the next day's decrease. Here, you can use logistic regression or any machine learning model to classify whether the next day will be a 'Decrease' or not.

# Assuming a simple logistic regression model for demonstration
# More complex models like Random Forest, SVM, or Neural Networks can be used as needed

# Recommendations for Improvement ----
# Enhancing Model Robustness
# 
# Regularization: Implement logistic regression with regularization (e.g., ridge or lasso) to reduce overfitting and help with convergence issues.
# Feature Selection and Engineering: Re-evaluate the features used. You might want to add more features like volume, other technical indicators (RSI, MACD), or use polynomial or interaction terms.
# Model Complexity: Consider simpler models or reduce the number of features. Sometimes, simpler models are more robust.
# Cross-validation: Instead of a simple train-test split, use k-fold cross-validation to ensure that your model's performance is stable across different subsets of data.

# Splitting data into train and test
aapl_df <- aapl_df |> na.omit()

# Adjusting Trend to be binary (1 for Decrease, 0 otherwise)
aapl_df <- aapl_df %>%
	mutate(
		Trend_Binary = ifelse(Trend == "Decrease", 1, 0)
	)

print("Trend variable adjusted for binary logistic regression:")
print(head(aapl_df))

# Preparing data for logistic regression
set.seed(123)

# random sample training data
# train_indices <- sample(1:nrow(aapl_df), 0.8 * nrow(aapl_df)) 
# train_data <- aapl_df[train_indices, ]
# test_data <- aapl_df[-train_indices, ]

# Calculate the index for splitting the dataset into training (first 80%) and testing sets
split_index <- floor(0.5 * nrow(aapl_df))
train_data <- aapl_df[1:split_index, ]
test_data <- aapl_df[(split_index + 1):nrow(aapl_df), ]

first_test_date <- first(test_data$Date)

# Fit logistic regression model using the binary trend
fit <- glm(Trend_Binary ~ Close_Pct_Change + MA_7 + MA_30, data = train_data, family = binomial())

print("Model summary:")
summary(fit)

# Making predictions on the test set
predictions <- predict(fit, test_data, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Evaluate model performance
conf_matrix <- table(Predicted = predicted_classes, Actual = test_data$Trend_Binary)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
precision <- conf_matrix[2,2] / sum(conf_matrix[2,])
recall <- conf_matrix[2,2] / sum(conf_matrix[,2])

print("Confusion Matrix:")
print(conf_matrix)
print(paste("Accuracy:", accuracy))
print(paste("Precision:", precision))
print(paste("Recall:", recall))

# Plot actual vs. predicted trends
names(test_data)

# full data viz ----

# Append predictions to the test dataset for visualization
test_data <- mutate(test_data, Prediction = predicted_classes)

# Combine test data back with training data for a full timeline plot
full_data <- bind_rows(
	select(train_data, Date, AAPL.Close, Trend_Binary),
	select(test_data, Date, AAPL.Close, Trend_Binary, Prediction)
)

# Adding discrepancy indicator to the combined dataset
full_data <- full_data %>%
	mutate(
		Prediction = if_else(is.na(Prediction), Trend_Binary, Prediction), # Fill NA predictions for training data
		Discrepancy = ifelse(Trend_Binary == Prediction, "Match", "Mismatch") # Discrepancy calculation
	)

# Plot actual vs. predicted trends with discrepancies
p_recall <- ggplot(full_data, aes(x = Date, y = AAPL.Close)) +
	geom_line(color = "gray") +
	geom_vline(xintercept = first_test_date, linetype = "dotted") +
	geom_point(aes(color = Discrepancy), shape = 4, size = 2, alpha = 0.75) +
	labs(
		title = "Full Timeline of AAPL Stock with Actual vs. Predicted Trends",
		subtitle = paste("Accuracy:", round(accuracy, 2), "Precision:", round(precision, 2), "Recall:", round(recall, 2)),
		x = "Date",
		y = "Closing Price",
		color = "Trend Accuracy"
	) +
	scale_color_manual(values = c("Match" = "green", "Mismatch" = "red")) +
	theme_minimal() +
	theme(legend.position = "bottom")

ggsave("../output/plot_quantmod_p_recall.pdf", p_recall, width = 8, height = 6)
ggsave("../output/plot_quantmod_p_recall.png", p_recall, width = 8, height = 6)



# anlysis function ----

# Step 1: Define the Objective
# Define what constitutes a "significant drop" and "optimal selling point." For instance, you might define a significant drop as a decrease of more than 5% from the peak within a certain time frame.
# 
# Step 2: Enhance Feature Engineering
# Enhance your features to capture more temporal dynamics, such as:
	# 
	# Longer moving averages to understand longer-term trends.
# Relative strength index (RSI) or MACD (Moving Average Convergence Divergence) which are common technical indicators that help identify overbought or oversold conditions.
# Volatility measures, such as historical volatility or Bollinger bands, to understand the price stability before making selling decisions.
# Step 3: Develop the Analysis Function
# Develop a function that simulates different selling strategies based on predicted trends and actual price movements. This function could iterate over various scenarios to find the strategy that maximizes returns or minimizes losses on the training set.

library(TTR)  # For RSI, MACD
library(quantmod)  # For additional financial indicators

simulate_trading <- function(data, lookback = 20, threshold_drop = 0.05) {
	# Initialize an empty data frame for results
	results <- data.frame(
		Start_Date = as.Date(character()),
		Sell_Date = as.Date(character()),
		Max_Price = numeric(),
		Closing_Price = numeric(),
		Drop = numeric(),
		stringsAsFactors = FALSE
	)
	
	for (i in seq_len(nrow(data) - lookback)) {
		# Calculate peak within the lookback period
		local_max <- max(data$AAPL.Close[i:(i + lookback - 1)])
		# Determine the closing price at the end of the lookback period
		closing_price <- data$AAPL.Close[i + lookback]
		# Check for significant drop
		if (local_max > closing_price && (local_max - closing_price) / local_max >= threshold_drop) {
			# Append row to results data frame
			results <- rbind(results, data.frame(
				Start_Date = data$Date[i],
				Sell_Date = data$Date[i + lookback],
				Max_Price = local_max,
				Closing_Price = closing_price,
				Drop = (local_max - closing_price) / local_max,
				stringsAsFactors = FALSE
			))
		}
	}
	return(results)
}

# Run simulation on training data
train_results <- simulate_trading(train_data)

# Check the output
print(head(train_results))

# Evaluate the best scenario
best_scenario <- train_results[which.max(train_results$Max_Price - train_results$Closing_Price), ]
print(best_scenario)

# Apply best scenario insight on test data
test_results <- simulate_trading(test_data)

# Compare performance
summary(test_results$Drop)

# Visualizing the trading outcomes on test data
p1 <- ggplot(test_data, aes(x = Date, y = AAPL.Close)) +
	geom_line() +
	geom_vline(data = test_results, aes(xintercept = as.Date(Sell_Date)), linetype = "dotted", color = "red") +
	geom_point(data = test_results, aes(x = as.Date(Sell_Date), y = Closing_Price), color = "red") +
	labs(title = "Test Data: Selling Points Identified by the Strategy", y = "Closing Price") +
	theme_minimal()

# Enhancing the Function for Predictive Selling ----

library(TTR)  # For technical trading indicators
library(quantmod)  # For market data manipulation


simulate_predictive_trading <- function(data, lookback = 20, threshold_drop = 0.05, rsi_threshold = 70) {
	# Calculate RSI and moving averages
	data$RSI <- RSI(data$AAPL.Close, n = lookback)
	data$MA_Short <- SMA(data$AAPL.Close, n = lookback)
	data$MA_Long <- SMA(data$AAPL.Close, n = lookback * 2)
	
	# Optionally, remove rows where any calculation resulted in NA (adjust based on your data's needs)
	data <- na.omit(data)
	
	# Initialize an empty data frame for results
	results <- data.frame(
		Start_Date = as.Date(character()),
		Predicted_Sell_Date = as.Date(character()),
		Max_Price = numeric(),
		Predicted_Closing_Price = numeric(),
		Predicted_Drop = numeric(),
		stringsAsFactors = FALSE
	)
	
	for (i in seq_len(nrow(data) - lookback)) {
		local_max <- max(data$AAPL.Close[i:(i + lookback - 1)])
		rsi_peak <- data$RSI[i + lookback - 1]
		short_ma <- data$MA_Short[i + lookback - 1]
		long_ma <- data$MA_Long[i + lookback - 1]
		
		# Check for NA in the loop to avoid the error
		if (!is.na(local_max) && !is.na(rsi_peak) && !is.na(short_ma) && !is.na(long_ma) &&
			 local_max > short_ma && rsi_peak > rsi_threshold && short_ma > long_ma) {
			predicted_sell_date <- data$Date[i + lookback]
			predicted_closing_price <- data$AAPL.Close[i + lookback]
			
			# Calculate the expected drop if selling now
			predicted_drop <- (local_max - predicted_closing_price) / local_max
			
			results <- rbind(results, data.frame(
				Start_Date = data$Date[i],
				Predicted_Sell_Date = predicted_sell_date,
				Max_Price = local_max,
				Predicted_Closing_Price = predicted_closing_price,
				Predicted_Drop = predicted_drop,
				stringsAsFactors = FALSE
			))
		}
	}
	return(results)
}

is.na()
# Run the enhanced simulation on training data
train_results <- simulate_predictive_trading(train_data)
best_scenario <- train_results[which.min(train_results$Predicted_Drop), ]
print(best_scenario)

# Apply the insights on test data
test_results <- simulate_predictive_trading(test_data)
names(test_data)

# Visualization of outcomes
p2 <- ggplot(test_data, aes(x = Date, y = AAPL.Close)) +
	geom_line() +
	geom_vline(data = test_results, aes(xintercept = as.Date(Predicted_Sell_Date)), linetype = "dashed", color = "blue") +
	geom_point(data = test_results, aes(x = as.Date(Predicted_Sell_Date), y = Predicted_Closing_Price), color = "blue", size = 3) +
	labs(title = "Test Data: Predictive Selling Points Identified by the Strategy",
		  y = "Closing Price") +
	theme_minimal() 

p_combined <- p1 / p2

ggsave("../output/plot_quantmod_p_combined.pdf", p_combined, width = 10, height = 12)
ggsave("../output/plot_quantmod_p_combined.png", p_combined, width = 10, height = 12)



# Enhanced logging and feedback ----

library(TTR)  # For RSI, MACD, etc.
library(quantmod)  # For market data manipulation
library(TTR)  # Load TTR for technical trading indicators
library(TTR)  # For technical trading indicators

simulate_predictive_trading <- function(data, lookback = 20, threshold_drop = 0.05, rsi_threshold = 70, debug = FALSE) {
	# Calculate RSI, MACD, and Bollinger Bands
	data$RSI <- RSI(data$AAPL.Close, n = lookback)
	macd_data <- MACD(data$AAPL.Close, nFast = 12, nSlow = 26, nSig = 9)
	data$MACD <- macd_data[, "macd"]
	data$Signal <- macd_data[, "signal"]
	bbands_data <- BBands(data$AAPL.Close, n = 20)
	
	# Extracting Bollinger Bands components
	data$BB_Upper <- bbands_data[, "up"]
	data$BB_Middle <- bbands_data[, "mavg"]
	data$BB_Lower <- bbands_data[, "dn"]
	
	data <- na.omit(data)  # Remove NA values which are produced by indicators
	
	results <- data.frame(
		Start_Date = as.Date(character()),
		Predicted_Sell_Date = as.Date(character()),
		Max_Price = numeric(),
		Predicted_Closing_Price = numeric(),
		Predicted_Drop = numeric(),
		stringsAsFactors = FALSE
	)
	
	for (i in seq_len(nrow(data) - lookback)) {
		local_max <- max(data$AAPL.Close[i:(i + lookback - 1)])
		current_close <- data$AAPL.Close[i + lookback]
		rsi_peak <- data$RSI[i + lookback]
		macd_val <- data$MACD[i + lookback]
		upper_band <- data$BB_Upper[i + lookback]
		
		if (debug) {
			print(paste("Date:", data$Date[i + lookback], "RSI:", rsi_peak, "MACD:", macd_val, "Upper Bollinger Band:", upper_band))
		}
		
		# Predictive condition for a selling point
		if (current_close > upper_band && rsi_peak > rsi_threshold && macd_val < data$Signal[i + lookback]) {
			predicted_sell_date <- data$Date[i + lookback]
			
			results <- rbind(results, data.frame(
				Start_Date = data$Date[i],
				Predicted_Sell_Date = predicted_sell_date,
				Max_Price = local_max,
				Predicted_Closing_Price = current_close,
				Predicted_Drop = (local_max - current_close) / local_max,
				stringsAsFactors = FALSE
			))
		}
	}
	return(results)
}

# Run the enhanced simulation with debug information
train_results <- simulate_predictive_trading(train_data, debug = TRUE)
print(head(train_results))



# Visualization and further analysis can follow based on the printed debug information
MAKE THIS BETTER


# Merging the train_results with the original data for visualization
train_data_with_results <- train_data %>%
	left_join(train_results, by = c("Date" = "Predicted_Sell_Date"))

# Plotting the results
p <- ggplot(train_data, aes(x = Date, y = AAPL.Close)) +
	geom_line(color = "black") +  # Line plot for closing prices
	geom_point(data = train_data_with_results, aes(x = as.Date(Start_Date), y = Max_Price), color = "blue", size = 3, shape = 4) +  # Blue X for max price points
	geom_point(data = train_data_with_results, aes(x = as.Date(Date), y = Predicted_Closing_Price), color = "red", size = 3, shape = 17) +  # Red triangles for predicted sell points
	labs(title = "Train Data: Predicted Selling Points and Max Prices",
		  x = "Date", y = "AAPL Closing Price",
		  caption = "Blue X: Max Price before predicted drop, Red Triangle: Predicted Sell Points") +
	theme_minimal()

# Display the plot
print(p)

# # forecast 1 ----
# 
# # Extract the last date from the AAPL.Close series
# last_date <- index(aapl_close)[length(aapl_close)]
# 
# # Generate future dates starting from the day after the last date in the existing data
# forecast_dates <- seq(from = last_date + 1, by = "day", length.out = 20)
# 
# # Forecast the next 90 days
# future_forecast <- forecast(arima_model, h = 20)
# 
# # Create a dataframe for the forecast including dates
# forecast_df <- data.frame(
# 	Date = forecast_dates,
# 	Price = future_forecast$mean,
# 	Lower_80 = future_forecast$lower[, "80%"],
# 	Upper_80 = future_forecast$upper[, "80%"],
# 	Lower_95 = future_forecast$lower[, "95%"],
# 	Upper_95 = future_forecast$upper[, "95%"],
# 	Type = "Forecast"
# )
# 
# 
# # Assuming you already have the forecasted data in forecast_df and actual data in actual_data
# # Adjust actual_data to include the forecasted intervals placeholders
# actual_data <- actual_data %>%
# 	mutate(Lower_80 = NA,  # Placeholder for lower 80% confidence interval
# 			 Upper_80 = NA,  # Placeholder for upper 80% confidence interval
# 			 Lower_95 = NA,  # Placeholder for lower 95% confidence interval
# 			 Upper_95 = NA)  # Placeholder for upper 95% confidence interval
# 
# # Rename the actual price column in actual_data to match forecast_df
# names(actual_data)[names(actual_data) == "AAPL.Close"] <- "Price"
# 
# 
# # Combine actual and forecast data
# combined_data <- rbind(actual_data, forecast_df)
# 
# head(combined_data)
# head(actual_data)
# head(forecast_df)
# 
# # Plotting using ggplot2
# p_forecast <- ggplot(combined_data, aes(x = Date, y = Price, color = Type)) +
# 	geom_line() +
# 	geom_ribbon(data = forecast_df, aes(ymin = Lower_95, ymax = Upper_95), fill = "red", alpha = 0.2) +
# 	geom_ribbon(data = forecast_df, aes(ymin = Lower_80, ymax = Upper_80), fill = "blue", alpha = 0.3) +
# 	labs(title = "Forecast of AAPL Closing Prices", x = "Date", y = "Price") +
# 	scale_color_manual(values = c("Actual" = "black", "Forecast" = "green")) +
# 	theme_minimal()
# 
# # Print the plot
# print(p_forecast)
# 
# 
# library(ggplot2)
# 
# # Plotting the forecast
# autoplot(future_forecast) +
# 	# autolayer(aapl_close, series = "AAPL Close", colour = "blue") +  # Add actual data
# 	ggtitle("AAPL Stock Price Forecast") +
# 	xlab("Date") +
# 	ylab("Price") +
# 	theme_minimal() +
# 	scale_color_manual(values = c("Forecast" = "red", "AAPL Close" = "blue"))

