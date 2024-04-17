
library(dplyr)
library(caret)

data <- read.csv("/Users/indiramallela/Dropbox/Northwood/ML/ML course data download week1/studentInfo2.csv") # Replace "your_dataset.csv" with the path to your dataset

# Data preprocessing
# Convert categorical variables to factors
data$code_module <- as.factor(data$code_module)
data$code_presentation <- as.factor(data$code_presentation)
data$gender <- as.factor(data$gender)
data$region <- as.factor(data$region)
data$highest_education <- as.factor(data$highest_education)
data$imd_band <- as.factor(data$imd_band)
data$age_band <- as.factor(data$age_band)
data$num_of_prev_attempts <- as.factor(data$num_of_prev_attempts)
data$disability <- as.factor(data$disability)
data$final_result <- as.factor(data$final_result)

# Split the data into training and testing sets (80% training, 20% testing)
set.seed(123) # For reproducibility
train_index <- createDataPartition(data$final_result, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Train the classification model (logistic regression)
model <- train(final_result ~ ., data = train_data, method = "glm", family = "binomial")

# Make predictions on the test data
predictions <- predict(model, newdata = test_data)

# Evaluate the model
confusionMatrix(predictions, test_data$final_result)


summary(model)
print(model)


# Display the confusion matrix as a chart
confusion_chart <- ggplot(data.frame(confusion_matrix), aes(x = predictions, y = Var2, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), size = 10, color = "white") +
  xlab("Predicted") +
  ylab("Actual") +
  ggtitle("Confusion Matrix") +
  theme_minimal()
# Display the confusion matrix as a chart
confusion_chart

# Calculate predicted probabilities
predicted_probs <- predict(model, newdata = test_data, type = "prob")

# Combine predictions and probabilities into a data frame
sigmoid_data <- data.frame(Predictions = predictions, Probabilities = predicted_probs[, "Withdrawn"]) # Assuming "Withdrawn" is the positive class

# Sigmoid curve plot
sigmoid_chart <- ggplot(sigmoid_data, aes(x = Probabilities, fill = Predictions)) +
  geom_density(alpha = 0.5) +
  xlab("Predicted Probability") +
  ylab("Density") +
  ggtitle("Sigmoid Curve") +
  theme_minimal()
sigmoid_chart

# Extract metrics from confusion matrix
accuracy <- cm$overall["Accuracy"]
precision <- cm$byClass["Precision"]
recall <- cm$byClass["Recall"]
f1 <- cm$byClass["F1"]
specificity <- cm$byClass["Specificity"]

# Create a data frame with the metrics
metrics_df <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "F1 Score", "Specificity"),
  Value = c(accuracy, precision, recall, f1, specificity)
)

# Create a bar chart of the metrics
metric_chart <- ggplot(metrics_df, aes(x = Metric, y = Value)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Metric") +
  ylab("Value") +
  ggtitle("Model Performance Metrics") +
  theme_minimal()

# Display the metric chart
print(metric_chart)
