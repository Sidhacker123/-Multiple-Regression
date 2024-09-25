# Lab: Classification Methods


## The Stock Market Data

###
library(class)
library(ISLR)

## set train 
train_data <- read.csv("parkinsons_train.csv")
test_data <- read.csv("parkinsons_test.csv")

train_data$status <- as.factor(train_data$status)
test_data$status <- as.factor(test_data$status)



# Logistic Regression Model
glm.fits <- glm(status ~MDVP.Fo.Hz. + MDVP.Fhi.Hz. + MDVP.Flo.Hz. + MDVP.Jitter... + MDVP.Jitter.Abs. + MDVP.RAP + MDVP.PPQ + Jitter.DDP + MDVP.Shimmer + MDVP.Shimmer.dB. + Shimmer.APQ3 + Shimmer.APQ5 + MDVP.APQ + Shimmer.DDA + NHR + HNR + RPDE + D2 + DFA + spread1 + spread2 + PPE, data = train_data, family = binomial)

# Predict on training data
train_pred_logistic <- predict(glm.fits, newdata = train_data, type = "response")
train_pred_logistic <- ifelse(train_pred_logistic > 0.5, 1, 0)

# Predict on test data
test_pred_logistic <- predict(glm.fits, newdata = test_data, type = "response")
test_pred_logistic <- ifelse(test_pred_logistic > 0.5, 1, 0)

# Calculate training and test errors
train_error_logistic <- mean(train_pred_logistic != train_data$status)
test_error_logistic <- mean(test_pred_logistic != test_data$status)

cat("Logistic Regression - Training Error:", train_error_logistic, "\n")
cat("Logistic Regression - Test Error:", test_error_logistic, "\n")



#LDA 

# Step 1: Remove the 'name' column and constant variables
train_data_cleaned <- train_data[, !names(train_data) %in% c("name")]
test_data_cleaned <- test_data[, !names(test_data) %in% c("name")]

# Remove constant variables
constant_vars <- sapply(train_data_cleaned, function(x) length(unique(x)) == 1)
train_data_cleaned <- train_data_cleaned[, !constant_vars]
test_data_cleaned <- test_data_cleaned[, !constant_vars]

# Step 2: Identify numeric columns (excluding 'status')
numeric_columns <- sapply(train_data_cleaned, is.numeric)

# Step 3: Scale the numeric features for both training and test data
train_data_scaled <- as.data.frame(scale(train_data_cleaned[, numeric_columns]))
test_data_scaled <- as.data.frame(scale(test_data_cleaned[, numeric_columns]))

# Add back the 'status' column
train_data_scaled$status <- train_data_cleaned$status
test_data_scaled$status <- test_data_cleaned$status

# Step 4: Fit the LDA model without the 'name' column
lda.fits <- lda(status ~ ., data = train_data_scaled)

# Step 5: Make predictions on the training and test data
lda_pred_train <- predict(lda.fits, train_data_scaled)$class
lda_pred_test <- predict(lda.fits, test_data_scaled)$class

# Step 6: Calculate training and testing errors
lda_train_error <- mean(lda_pred_train != train_data_scaled$status)
lda_test_error <- mean(lda_pred_test != test_data_scaled$status)

# Print the training and test errors
print(paste("Training Error: ", lda_train_error))
print(paste("Test Error: ", lda_test_error))



_______________________________________________________________

# QDA Model
library(class)
# Step 1: Remove the 'name' column
train_data_cleaned <- train_data[, !names(train_data) %in% c("name")]
test_data_cleaned <- test_data[, !names(test_data) %in% c("name")]

# Step 2: Identify numeric columns (excluding 'status')
numeric_columns <- sapply(train_data_cleaned, is.numeric)

# Step 3: Scale the numeric features for both training and test data
train_data_scaled <- as.data.frame(scale(train_data_cleaned[, numeric_columns]))
test_data_scaled <- as.data.frame(scale(test_data_cleaned[, numeric_columns]))

# Add back the 'status' column
train_data_scaled$status <- train_data_cleaned$status
test_data_scaled$status <- test_data_cleaned$status

# Step 4: Fit the QDA model (from the 'MASS' package)
qda_model <- qda(status ~ ., data = train_data_scaled)

# Step 5: Make predictions on both the training and test datasets
train_pred_qda <- predict(qda_model, train_data_scaled)$class
test_pred_qda <- predict(qda_model, test_data_scaled)$class

# Step 6: Calculate training and test errors
train_error_qda <- mean(train_pred_qda != train_data_scaled$status)
test_error_qda <- mean(test_pred_qda != test_data_scaled$status)

# Print the training and test errors
print(paste("QDA Training Error: ", train_error_qda))
print(paste("QDA Test Error: ", test_error_qda))



______________________________

# Naive Bayes Model
library(e1071)
nb_model <- naiveBayes(status ~ ., data = train_data)

# Predict on training data
train_pred_nb <- predict(nb_model, newdata = train_data)

# Predict on test data
test_pred_nb <- predict(nb_model, newdata = test_data)

# Calculate training and test errors
train_error_nb <- mean(train_pred_nb != train_data$status)
test_error_nb <- mean(test_pred_nb != test_data$status)

cat("Naive Bayes - Training Error:", train_error_nb, "\n")
cat("Naive Bayes - Test Error:", test_error_nb, "\n")


