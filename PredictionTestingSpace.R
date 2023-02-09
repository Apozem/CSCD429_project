options(digits = 10)

data <- read.csv("C:\\Users\\Capnb\\Downloads\\HousePricesInIndia\\train.csv")
View(data)
prices_data <- data[12]

prices <- as.double(unlist(prices_data))
prices_sorted <- sort(prices)
print(data[order(data$TARGET.PRICE_IN_LACS., decreasing = TRUE), ])

print("Performing Calculations on Pricing:")
paste("Min = ", min(prices_sorted))
paste("Max = ", max(prices_sorted))
paste("Mean = ", mean(prices_sorted))
paste("Median = ", median(prices_sorted))
print("Range: ")
paste(range(prices_sorted))
print("Five Number Summary: ")
paste(fivenum(prices_sorted))
fivenum <- fivenum(prices_sorted) #Stored as [min, q1, median, q3, max]
paste("Variance = ", var(prices_sorted)) #Measure of how far values are from their average

paste("Diff = ", diff(prices_sorted))

#WORKING WITH PRICE
quartiles <- quantile(prices_sorted, probs = c(0.25, 0.75), na.rm = FALSE)
IQR <- IQR(prices_sorted)
Q1 <- quartiles[1] - 1.5*IQR
Q3 <- quartiles[2] + 1.5*IQR

price_data_no_outlier <- subset(prices_sorted, prices_sorted > Q1 & prices_sorted < Q3)
#paste(price_data_no_outlier)

boxplot(prices)
boxplot(price_data_no_outlier, ylab = "Price (in LACS)")
#######################

#WORKING WITH SQUARE FT
SQFTdata <- data[6]
SQFT <- as.integer(unlist(SQFTdata))
SQFT <- sort(SQFT)
quartiles_SQFT <- quantile(SQFT, probs = c(0.25, 0.75), na.rm = FALSE)
IQR_SQFT <- IQR(SQFT)
Q1_SQFT <- quartiles_SQFT[1]
Q3_SQFT <- quartiles_SQFT[2]
SQFT_no_outlier <- subset(SQFT, SQFT > Q1_SQFT & SQFT < Q3_SQFT)
boxplot(SQFT_no_outlier, ylab = "SQ. FT.")
#######################

#KEEPING PRICE & SQFT IN DATAFRAME FOR QQPLOT
qqnorm(data$TARGET.PRICE_IN_LACS., frame = FALSE)
qqline(data$TARGET.PRICE_IN_LACS., col = "blue")

#ATTEMPTING TO BUILD A SCATTERPLOT WITH REMOVED OUTLIERS
price_quartiles <- quantile(data$TARGET.PRICE_IN_LACS., probs = c(0.25, 0.75), na.rm = FALSE)
price_IQR <- IQR(data$TARGET.PRICE_IN_LACS.)
price_Q1 <- price_quartiles[1] - 1.5*price_IQR
price_Q3 <- price_quartiles[2] + 1.5*price_IQR
price_no_outlier <- subset(data, data$TARGET.PRICE_IN_LACS. > price_Q1 & data$TARGET.PRICE_IN_LACS. < price_Q3)

SQFT_quartiles <- quantile(data$SQUARE_FT, probs = c(0.25, 0.75), na.rm = FALSE)
SQFT_IQR <- IQR(SQFT_quartiles)
SQFT_Q1 <- SQFT_quartiles[1] - 1.5*SQFT_IQR
SQFT_Q3 <- SQFT_quartiles[2] + 1.5*SQFT_IQR
SQFT_no_outlier <- subset(data, data$SQUARE_FT > SQFT_Q1 & data$SQUARE_FT < SQFT_Q3)

qqplot(price_no_outlier, SQFT_no_outlier)
qqline(price_no_outlier, col = "blue")

#CORRELATION VALUES
cor(price_no_outlier$TARGET.PRICE_IN_LACS., price_no_outlier$SQUARE_FT, method = c("pearson", "kendall", "spearman"))
cor(price_no_outlier$TARGET.PRICE_IN_LACS., price_no_outlier$UNDER_CONSTRUCTION, method = c("pearson", "kendall", "spearman"))
cor(price_no_outlier$TARGET.PRICE_IN_LACS., price_no_outlier$RERA, method = c("pearson", "kendall", "spearman"))
cor(price_no_outlier$TARGET.PRICE_IN_LACS., price_no_outlier$BHK_NO., method = c("pearson", "kendall", "spearman"))
cor(price_no_outlier$TARGET.PRICE_IN_LACS., price_no_outlier$READY_TO_MOVE, method = c("pearson", "kendall", "spearman"))
cor(price_no_outlier$TARGET.PRICE_IN_LACS., price_no_outlier$RESALE, method = c("pearson", "kendall", "spearman"))
cor(price_no_outlier$TARGET.PRICE_IN_LACS., price_no_outlier$LONGITUDE, method = c("pearson", "kendall", "spearman"))
cor(price_no_outlier$TARGET.PRICE_IN_LACS., price_no_outlier$LATITUDE, method = c("pearson", "kendall", "spearman"))

length(price_no_outlier)
length(SQFT_no_outlier)

price_no_outlier_no_dupes <- price_no_outlier[!duplicated(price_no_outlier), ]

plot(price_no_outlier$SQUARE_FT, price_no_outlier$TARGET.PRICE_IN_LACS.)
plot(price_no_outlier$SQUARE_FT, price_no_outlier$TARGET.PRICE_IN_LACS., xlim = c(0, 1000000))

#PLOTS FOR CORRELATION:
plot(price_no_outlier$SQUARE_FT, price_no_outlier$TARGET.PRICE_IN_LACS., xlim = c(0, 15000))

plot(price_no_outlier$UNDER_CONSTRUCTION, price_no_outlier$TARGET.PRICE_IN_LACS.)
hist(price_no_outlier$UNDER_CONSTRUCTION, col = "red")
hist(price_no_outlier$TARGET.PRICE_IN_LACS., col = "blue", add = TRUE)
mean(price_no_outlier[price_no_outlier$UNDER_CONSTRUCTION > 0, "TARGET.PRICE_IN_LACS."])
mean(price_no_outlier[price_no_outlier$UNDER_CONSTRUCTION < 1, "TARGET.PRICE_IN_LACS."])

plot(price_no_outlier$RERA, price_no_outlier$TARGET.PRICE_IN_LACS.)
hist(price_no_outlier$RERA, col = "red")
hist(price_no_outlier$TARGET.PRICE_IN_LACS., col = "blue", add = TRUE)
mean(price_no_outlier[price_no_outlier$RERA > 0, "TARGET.PRICE_IN_LACS."])
mean(price_no_outlier[price_no_outlier$RERA < 1, "TARGET.PRICE_IN_LACS."])

plot(price_no_outlier$BHK_NO., price_no_outlier$TARGET.PRICE_IN_LACS.)

plot(price_no_outlier$READY_TO_MOVE, price_no_outlier$TARGET.PRICE_IN_LACS.)
hist(price_no_outlier$READY_TO_MOVE, col = "red")
hist(price_no_outlier$TARGET.PRICE_IN_LACS., col = "blue", add = TRUE)
mean(price_no_outlier[price_no_outlier$READY_TO_MOVE > 0, "TARGET.PRICE_IN_LACS."])
mean(price_no_outlier[price_no_outlier$READY_TO_MOVE < 1, "TARGET.PRICE_IN_LACS."])

plot(price_no_outlier$RESALE, price_no_outlier$TARGET.PRICE_IN_LACS.)
hist(price_no_outlier$READY_TO_MOVE, col = "red")
hist(price_no_outlier$TARGET.PRICE_IN_LACS., col = "blue", add = TRUE)

plot(price_no_outlier$LONGITUDE, price_no_outlier$TARGET.PRICE_IN_LACS.)

plot(price_no_outlier$LATITUDE, price_no_outlier$TARGET.PRICE_IN_LACS.)

hist(price_no_outlier$ADDRESS, col = "red")
hist(price_no_outlier$TARGET.PRICE_IN_LACS., col = "blue", add = TRUE)

mean(price_no_outlier[price_no_outlier$POSTED_BY == "Dealer", "TARGET.PRICE_IN_LACS."])
mean(price_no_outlier[price_no_outlier$POSTED_BY == "Owner", "TARGET.PRICE_IN_LACS."])

abline(lm(price_no_outlier$SQUARE_FT ~ price_no_outlier$TARGET.PRICE_IN_LACS.), pch = 16, col = "blue")

plot(price_no_outlier_no_dupes$SQUARE_FT, price_no_outlier_no_dupes$TARGET.PRICE_IN_LACS., xlim = c(0, 15000))
abline(lm(price_no_outlier$SQUARE_FT ~ price_no_outlier$TARGET.PRICE_IN_LACS.), pch = 16, col = "blue")

#NEW IDEA: Store a list of quartiles for each respective variable in data
list_quantiles <- tapply(data$SQUARE_FT, data$TARGET.PRICE_IN_LACS., quantile)
Q1s <- sapply(1:2)
###################################################

print("Plot Creation:")
plot(data$TARGET.PRICE_IN_LACS., data$UNDER_CONSTRUCTION, xlim=c(0, 15000))
plot(data$TARGET.PRICE_IN_LACS., data$RERA, xlim=c(0, 15000)) #RERA approved?
plot(data$TARGET.PRICE_IN_LACS., data$BHK_NO., xlim=c(0, 15000))

# abline(lm(y ~ x, data = mtcars), col = "blue")
plot(data$TARGET.PRICE_IN_LACS., data$SQUARE_FT, xlim=c(0, 10000), ylim=c(0, 20000))
abline(lm(data$SQUARE_FT ~ data$TARGET.PRICE_IN_LACS.), col = "blue")

plot(data$TARGET.PRICE_IN_LACS., data$RESALE, xlim=c(0, 15000))
plot(data$TARGET.PRICE_IN_LACS., data$LONGITUDE, xlim=c(0, 15000))
plot(data$TARGET.PRICE_IN_LACS., data$LATITUDE, xlim=c(0, 15000))

coplot(data$SQUARE_FT ~ data$TARGET.PRICE_IN_LACS. | data)
boxplot(prices_sorted, ylab="Price")
hist(prices_sorted, breaks = 100)

#Module 4:
#Libraries:
#1)Information Gain Library
library(FSelector)
#2)Naive Bayesian Classification Libraries
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)

#1)Calculate Information Gain
information.gain(formula(data), data)
information.gain(formula(price_no_outlier), price_no_outlier)

#2)Naive Bayesian Classification
xtabs(~Launch+Rank, data = data)
pairs.panels(data[-1])
ind <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))
train <- #Read this CSV Later
test <- data
model <- naive_bayes(Launch ~ ., data = train, usekernel = T)
plot(model)