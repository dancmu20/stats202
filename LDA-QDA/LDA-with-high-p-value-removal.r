	# Load necessary libraries
	library(tidyverse)
	library(ggplot2)
	library(corrplot)
	
	# Load the datasets
	train_data <- read.csv("C:/dan/study/stats202/group project/training.csv")
	test_data <- read.csv("C:/dan/study/stats202/group project/test.csv")
	
	# Select the relevant predictors, excluding sig3, sig4, and sig5
	predictors <- train_data %>% select(query_length, is_homepage, sig1, sig2, sig6, sig7, sig8)
	target <- train_data$relevance
	
	# Display the first few rows of the training data
	head(train_data)
	
	# Summary statistics
	summary(train_data)
	
	# Check for missing values
	colSums(is.na(train_data))
	
	# Set seed to 678
	set.seed(678)
	
	# Split the data into training and validation sets
	train_index <- sample(seq_len(nrow(train_data)), size = 0.8 * nrow(train_data))
	train_set <- train_data[train_index, ]
	validation_set <- train_data[-train_index, ]
	
	# Train a logistic regression model without sig3, sig4, and sig5
	model <- glm(relevance ~ query_length + is_homepage + sig1 + sig2 + sig6 + sig7 + sig8,
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
