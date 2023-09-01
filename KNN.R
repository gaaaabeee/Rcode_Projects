#load in the excel file
Espn_college_stats <- read_excel("Desktop/Espn_college_stats.xlsx")

#Define the predictor and response variables
predictors <- c("PTS", "FGM", "FGA", "3PM", "3PA", "FTM", "FTA","REB","AST","STL", "BLK", "TO")
response <- c("POS")

#Randomly split the dataset into training and testing sets
hoop_train <- Espn_college_stats[sample(c(1:400)), predictors ] #80%
hoop_test <- Espn_college_stats[sample(c(401:500)), predictors] #20

hoop_train_label <- Espn_college_stats[1:400, response ]
hoop_test_label <- Espn_college_stats[401:500,response]

#Train the k-NN model with k=20
set.seed(1)
library(class)
knnPred <- knn(train = hoop_train, test = hoop_test, cl = hoop_train_label$POS , k = 1)


#Tables
table(knnPred)
table(hoop_test_label)
plot(knnPred)






