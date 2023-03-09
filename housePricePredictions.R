#install packages - only need to run once
#install.packages("rpart")
#install.packages("rpart.plot")

#libraries
library(rpart)
library(rpart.plot)

#functions
create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

inc <- function(x) {
  eval.parent(substitute(x <- x + 1))
}


#retains accuracy of decimal places
options(digits = 10)



#read in data of train file and test file, from SAME DIRECTORY AS THIS FILE
data <- read.csv("train.csv")
final_test <- read.csv("test.csv")

#get outliers from boxplot data
outliers <- boxplot(data$TARGET.PRICE_IN_LACS., plot = FALSE)$out

#add column to indicate of a row is an outlier
data$is_outlier <- ifelse(data$TARGET.PRICE_IN_LACS. %in% boxplot.stats(data$TARGET.PRICE_IN_LACS.)$out, 1, 0)

#remove rows with outliers
data_no_outlier <- subset(data,is_outlier==0)

#finds and removes sqft outliers
outliers_sqft <- boxplot(data_no_outlier$SQUARE_FT, plot = FALSE)$out
data_no_outlier$is_outlier <- ifelse(data_no_outlier$SQUARE_FT %in% boxplot.stats(data_no_outlier$SQUARE_FT)$out, 1, 0)
data_no_outlier <- subset(data_no_outlier,is_outlier==0)

#clean data
clean_data <- data_no_outlier[ , !names(data_no_outlier) %in% c("is_outlier", "ADDRESS")]

#create test and train out of accurate data
data_train <- create_train_test(clean_data, 0.8, train = TRUE)
data_test <- create_train_test(clean_data, 0.8, train = FALSE)

#create the basic decision tree
tree <- rpart(TARGET.PRICE_IN_LACS. ~., data=data_train, method = 'anova')
rpart.plot(tree)
#create target price column and fill with zeros
#final_test$TARGET.PRICE_IN_LACS. <- c(0)

#predictions
prd <- predict(tree, data_test)

#makes table with predictions
table_p <- table(data_test$TARGET.PRICE_IN_LACS., prd)
#next, hone accuracy, compute on final test data, fill results

#fill results to dataset for comparison
justnumbers <- as.numeric(prd)
data_test$PREDICTED.PRICE_IN_LACS. <- justnumbers

#compare predictions with actual
counter <- 0
for(i in 1:dim(data_test)[1]) {
  if(data_test$PREDICTED.PRICE_IN_LACS.[i] <= (data_test$TARGET.PRICE_IN_LACS.[i] + 15) & data_test$PREDICTED.PRICE_IN_LACS.[i] >= (data_test$TARGET.PRICE_IN_LACS.[i] - 15)) {
    inc(counter)
  }
}

#get accuracy
acc <- counter/nrow(data_test)

#print the accuracy to screen
print("Accuracy for tree 1, no settings tweaks: ")
print(acc)

#tweak tree values
control <- rpart.control(cp = 0.005)

#do process again
tree <- rpart(TARGET.PRICE_IN_LACS. ~., data=data_train, method = 'anova', control = control)
prd <- predict(tree, data_test)
table_p <- table(data_test$TARGET.PRICE_IN_LACS., prd)
justnumbers <- as.numeric(prd)
data_test$PREDICTED.PRICE_IN_LACS. <- justnumbers
counter <- 0
for(i in 1:dim(data_test)[1]) {
  if(data_test$PREDICTED.PRICE_IN_LACS.[i] <= (data_test$TARGET.PRICE_IN_LACS.[i] + 15) & data_test$PREDICTED.PRICE_IN_LACS.[i] >= (data_test$TARGET.PRICE_IN_LACS.[i] - 15)) {
    inc(counter)
  }
}
acc <- counter/nrow(data_test)
print("Accuracy for tree 1, with settings tweaks: ")
print(acc)
rpart.plot(tree)

#FIND ACCURACY ON TRAINING DATA
#check variance
tree <- rpart(TARGET.PRICE_IN_LACS. ~., data=data_train, method = 'anova')
prd <- predict(tree, data_train)
table_p <- table(data_train$TARGET.PRICE_IN_LACS., prd)
justnumbers <- as.numeric(prd)
data_train$PREDICTED.PRICE_IN_LACS. <- justnumbers
counter <- 0
for(i in 1:dim(data_train)[1]) {
  if(data_train$PREDICTED.PRICE_IN_LACS.[i] <= (data_train$TARGET.PRICE_IN_LACS.[i] + 15) & data_train$PREDICTED.PRICE_IN_LACS.[i] >= (data_train$TARGET.PRICE_IN_LACS.[i] - 15)) {
    inc(counter)
  }
}
acc <- counter/nrow(data_train)
print("Accuracy for training set: ")
print(acc)
rpart.plot(tree)

#Variance of the training dataset
#High variance the data is generally further from the mean. Low variance does not deviate much from the mean
var(data_train)

