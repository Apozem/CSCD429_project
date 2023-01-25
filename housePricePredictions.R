#retains accuracy of decimal places
options(digits = 10)

#read in data of train file, from SAME DIRECTORY AS THIS FILE
data <- read.csv("train.csv")

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

print(contstruction)