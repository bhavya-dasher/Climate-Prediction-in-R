library(caret)  # For data preprocessing
library(randomForest)  # For regression model
library(ggplot2)
data = read.csv('F:/R_practice_ques/Bhavya_Project/DailyDelhiClimateTest.csv')
View(data)

# Split the data into training and testing sets
set.seed(42)  # For reproducibility
train_indices <- sample(1:rows, 0.7 * rows)  # 70% for training
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Define the predictor variables (features) and the target variable
predictors <- c("meantemp", "wind_speed", "meanpressure")
target <- "humidity"

# Extract the predictor and target data from the data frame
predictor_data <- data[, predictors]
target_data <- data[, target]

# Train a regression model (Random Forest in this case)
model <- randomForest(x = predictor_data, y = target_data, ntree = 500)

# Make predictions on the test set
# Assuming you have already trained your random forest model
predictions <- predict(model, newdata = test_data)

predictions
test_data[, target]

# Calculate Mean Absolute Error (MAE)
mae <- mean(abs(predictions - test_data[, target]))
cat("Mean Absolute Error:", mae, "\n")

# Calculate Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((predictions - test_data[, target])^2))
cat("Root Mean Squared Error:", rmse, "\n")


ggplot(data, aes(x = cut(meantemp, breaks = seq(0, 45, by = 5)), y = humidity)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Humidity by Temperature Range", x = "Temperature Range", y = "Humidity")


ggplot(data, aes(x = meantemp, y = humidity)) +
  geom_point(color = "blue") +
  labs(title = "Temperature vs. Humidity", x = "Mean Temperature", y = "Humidity")

test_data$Date <- as.Date(test_data$date)

# Create a data frame with test data, actual humidity, and predicted humidity
#comparison_data <- data.frame(
#  Date = test_data$Date,
#  Actual_Humidity = test_data$humidity,
#  Predicted_Humidity = predictions
#)
#ggplot(comparison_data, aes(x = Date, y = Actual_Humidity)) +
#  geom_point(color = "blue", shape = 16, size = 3) +
#  geom_point(aes(y = Predicted_Humidity), color = "red", shape = 1, size = 3) +
#  labs(title = "Actual vs. Predicted Humidity", x = "Date", y = "Humidity") +
#  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week")

comparison_data <- data.frame(
  Date = test_data$Date,
  Actual_Humidity = test_data$humidity,
  Predicted_Humidity = predictions
)

ggplot(comparison_data, aes(x = Date, y = Actual_Humidity)) +
  geom_point(aes(color = "Actual"), shape = 16, size = 3) +
  geom_point(aes(x = Date, y = Predicted_Humidity, color = "Predicted"), shape = 1, size = 3) +
  labs(title = "Actual vs. Predicted Humidity", x = "Date", y = "Humidity") +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week") +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  guides(color = guide_legend(title = "Data Type")) +
  theme_minimal()