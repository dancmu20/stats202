# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(corrplot)

# Load the datasets
train_data <- read.csv("C:/dan/study/stats202/group project/training.csv")
test_data <- read.csv("C:/dan/study/stats202/group project/test.csv")

# Create squared terms for the relevant predictors, excluding sig3, sig4, and sig5
train_data <- train_data %>%
  mutate(query_length_sq = query_length^2,
         sig1_sq = sig1^2,
         sig2_sq = sig2^2,
         sig6_sq = sig6^2,
         sig7_sq = sig7^2)

test_data <- test_data %>%
  mutate(query_length_sq = query_length^2,
         sig1_sq = sig1^2,
         sig2_sq = sig2^2,
         sig6_sq = sig6^2,
         sig7_sq = sig7^2)

# Set seed to 678
set.seed(678)

# Split the data into training and validation sets
train_index <- sample(seq_len(nrow(train_data)), size = 0.8 * nrow(train_data))
train_set <- train_data[train_index, ]
validation_set <- train_data[-train_index, ]

# Train a logistic regression model with original and squared predictors, excluding sig3, sig4, sig5, is_homepage_sq, and sig8_sq
model <- glm(relevance ~ query_length + is_homepage + sig1 + sig2 + sig6 + sig7 + sig8 +
             query_length_sq + sig1_sq + sig2_sq + sig6_sq + sig7_sq,
             data = train_set, family = binomial)

# Summary of the model
summary(model)

# Predict on the validation set
validation_preds <- predict(model, newdata = validation_set, type = "response")
validation_preds <- ifelse(validation_preds > 0.5, 1, 0)

# Evaluate the model
# Manually creating a confusion matrix
confusion_matrix <- table(Predicted = validation_preds, Actual = validation_set$relevance)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Print confusion matrix and accuracy
print(confusion_matrix)
print(paste("Accuracy:", accuracy))

# Predict on the test dataset
test_preds <- predict(model, newdata = test_data, type = "response")
test_preds <- ifelse(test_preds > 0.5, 1, 0)

# Create the submission file
submission <- data.frame(id = test_data$id, relevance = test_preds)
write.csv(submission, "C:/dan/study/stats202/group project/Submission.csv", row.names = FALSE)