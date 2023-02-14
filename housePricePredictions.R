#libraries
library(rpart)
library(rpart.plot)

#retains accuracy of decimal places
options(digits = 10)

#read in data of train file and test file, from SAME DIRECTORY AS THIS FILE
data <- read.csv("train.csv")
test <- read.csv("test.csv")

#data separated into own vectors
posted <- as.character(unlist(data[1]))
contstruction <- as.logical(unlist(data[2]))
rera <- as.logical(unlist(data[3]))
rooms <- as.integer(unlist(data[4]))
type <- as.character(unlist(data[5]))
sqft <- as.numeric(unlist(data[6]))
ready <- as.logical(unlist(data[7]))
resale <- as.logical(unlist(data[8]))
address <- as.character(unlist(data[9]))
long <- as.numeric(unlist(data[10]))
lat <- as.numeric(unlist(data[11]))
price <- as.numeric(unlist(data[12]))

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

#create the basic decision tree
tree <- rpart(TARGET.PRICE_IN_LACS. ~., data=clean_data)

#create target price column and fill with zeros
test$TARGET.PRICE_IN_LACS. <- c(0)

#preditctions
prd <- predict(tree, test)

#makes table with predictions
table_p <- table(test$TARGET.PRICE_IN_LACS., prd)
