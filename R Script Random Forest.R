install.packages("randomForest")
library(randomForest)
library(caret)

#STEP 1: Import Data, Develop initial dataframe
#setup folders and filename
datafilename <-"Raw_data_final.csv"
datafolder <- "C:/Users/despm/OneDrive/Documents/My PhD Work/FINAL DATA WORK/"
datafile <- paste(datafolder, datafilename, sep="")

#read rawdata from csv. rawdata=mydata
mydata <- read.table(datafile, header=TRUE, sep=",")

#give name to retrieve columns names of mydata
columns_names <- colnames(mydata)

#select columns needed in mydata_selected
selected_columns <- c(6,7,9,10,11,12,13,14,15,17,19,23)

#develop data frame
mydata_selected <- mydata[,selected_columns]

#STEP 2: Data Preparation
#count na by column
sapply(mydata_selected, function(x) sum(is.na(x)))

class(mydata_selected)

#find  columns where data is numeric
columns_numeric_logical <- sapply(mydata_selected,is.numeric)
columns_numeric <- which(columns_numeric_logical == TRUE)

#imputing data
for (i in columns_numeric)  {
  mydata_selected[,i]  = ifelse(is.na( mydata_selected[,i] ), median(mydata_selected[,i], na.rm =TRUE),mydata_selected[,i] )
}

sapply(mydata_selected, function(x) sum(is.na(x)))

#omit na
mydata_selected <- na.omit(mydata_selected)
sapply(mydata_selected, function(x) sum(is.na(x)))

#STEP 3:Explore the data

head(mydata_selected)

str(mydata_selected)

summary(mydata_selected)


#STEP 4: Split into Train and Validation sets in the ratio 70:30
# Training Set : Validation Set = 70 : 30 (random)

set.seed(1500)
train <- sample(nrow(mydata_selected), 0.7*nrow(mydata_selected), replace = FALSE)
TrainSet <- mydata_selected[train,]
ValidSet <- mydata_selected[-train,]
summary(TrainSet)
summary(ValidSet)

#STEP 5: Create a Random Forest model with default parameters (ntrees=500, mtry=2)
model1 <- randomForest(mid_spread ~ ., data = TrainSet, mtry=3, importance = TRUE, do.trace=TRUE)
print(model1)
plot(model1)
tuneRF(TrainSet[,-12], TrainSet$mid_spread, mtry=3, ntree=500, stepFactor=1, improve=0.05,
       trace=TRUE, plot=TRUE, doBest = =TRUE)



#STEP 6: Predicting on train set, checking classification accuracy
# Predicting on train set
predTrain <- predict(model1, TrainSet, type = "response")


# Checking classification accuracy
table(predTrain, TrainSet$mid_spread)

#STEP 7: Predicting on Validation set
predValid <- predict(model1, ValidSet, type = "response")
# Checking classification accuracy
mean(predValid == ValidSet$mid_spread)                    
table(predValid,ValidSet$mid_spread)

#STEP 8: To check important variables
importance(model1)        
varImpPlot(model1)