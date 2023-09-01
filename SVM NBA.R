library(caret)
library(readxl)
library(e1071)

# Set the file path of the excel spreadsheet
file_path <- "C:/Users/gabe9/Desktop/Espn_college_stats.xlsx"

# Read the data from the excel file into a data frame
data <- read_excel(file_path)

# Remove the first two columns from the data frame
data <- data[, -c(1:2)]

# Replace 'G' with 1, 'F' with 2, and 'C' with 3
data$POS <- factor(ifelse(data$POS == "G", 1,
                          ifelse(data$POS == "F", 2, 3)))

# Split the data into an 80% training set and a 20% testing set
trainIndex <- createDataPartition(data$POS, p = .8, list = FALSE, times = 1)
training <- data[trainIndex, ] # keep all columns in the training set
testing <- data[-trainIndex, ] # keep all columns in the testing set


#perform K-fold to find best cost and gamma
set.seed(123)

tune_grid <- expand.grid(C = c(0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, 128),
                         sigma = c(0.1, 0.01, 0.001))

ctrl <- trainControl(method = "cv", number = 10)
svm_tune_model <- train(POS ~ ., data = training, method = "svmRadial",
                   trControl = ctrl, tuneLength = 10, tuneGrid=tune_grid)
svm_tune_model
#best cost was 4 and best gamma was .01


# Train the SVM model
set.seed(123)

svm_model <- svm(POS ~ ., data = training, kernel = "radial", cost = 4, gamma = 0.01)
  
# Predict the positions of the players in the test set
svm_pred <- predict(svm_model, testing)

# Compute the confusion matrix and the metrics
confusionMatrix(svm_pred, testing$POS)

#the model has an overall accuracy of 86.87% however it wasn't able
#to identify any players as centers, I believe this is because the 
#data only has 15 centers out of 500 samples, it at least classifed 
#the centers as forwards which is far better than centers being classified
#as guards