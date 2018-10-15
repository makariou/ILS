install.packages("rmarkdown")

#STEP 1: Import Raw Data 
#setup folders and filename
datafilename <-"Raw_data_final.csv"
datafolder <- "C:/Users/despm/OneDrive/Documents/My PhD Work/FINAL DATA WORK/"
datafile <- paste(datafolder, datafilename, sep="")

#read rawdata from csv. rawdata=mydata
mydata <- read.table(datafile, header=TRUE, sep=",")

#give name to retrieve columns names of mydata
columns_names <- colnames(mydata)

#STEP 2: Develop initial dataframe

#select columns needed in mydata_selected
selected_columns <- c(6,7,9,10,11,12,13,14,15,17,19,23)

#develop data frame
mydata_selected <- mydata[,selected_columns]

#STEP 3: Impute Numeric Data, Omit n/a
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

#STEP 4: Data Partition in proportion 70:30

ind <- sample(2, nrow(mydata_selected), replace=TRUE, prob = c(0.7, 0.3))
train <- mydata_selected[ind==1,]
test <- mydata_selected[ind==2,]

#STEP 5: RANDOM FOREST MODEL
library(randomForest)
set.seed(222)
rf <- randomForest(mid_spread~., data=train)
print(rf)
attributes(rf)
#For each attribute i can write rf$attribute eg. rf$mse if I want to see something further

#PREDICTION - TRAIN DATA- not for regression?
library(caret)
#prediction 1=p1
p1 <- predict(rf, train)
head(p1)
head(train$mid_spread)
#cannot do confusion matrix for regression

#OUT OF BAG ERROR_for regression R-Sq and RMSE
actual <- dat$carat
predicted <- unname(predict(rfPar, dat))
#calculate the R-squared value after the fact by taking the predictions that result from your training data and your trained model and comparing them to the actual values:
R2 <- 1 - (sum((train$mid_spread-p1)^2)/sum((train$mid_spread-mean(train$mid_spread))^2))
library(caret)
RMSE(p1,train$mid_spread)

#PREDICTION - TEST DATA
p2 <- predict(rf, test)
R2 <- 1 - (sum((test$mid_spread-p2)^2)/sum((test$mid_spread-mean(test$mid_spread))^2))
library(caret)
RMSE(p2,test$mid_spread)

#ERROR RATE OF RANDOM FOREST
plot(rf)

#TUNE mtry
t <- tuneRF(train[,-12], train[,12], stepFactor = 0.5, plot= TRUE, ntreeTry = 300, trace = TRUE, improve = 0.05)
#with stepFactor one, no result, try with 0.5
#after trying stepFactor one, ntreeTry=300 going back to rf and add few more things..

set.seed(222)
rf <- randomForest(mid_spread~., data=train, ntree=300, mtry=3, importance=TRUE, proximity=TRUE)
print(rf)

#No of nodes for the trees
hist(treesize(rf), main= 'No of Nodes for the trees', col = 'green')

#Variable Importance
#plot
varImpPlot(rf, sort=TRUE, main = 'Variable Importance')
#as quantitative values
importance(rf)
#which vars are actually used in the random forest
varUsed(rf)




#Extract Single Tree
#for getting info on the first tree out of the 300
getTree(rf, 1, labelVar = TRUE)

#Multi Dimensional Scaling Plot of Proximity Matrix
MDSplot(rf, train$mid_spread)
variance


#trial
library(VSURF)
thres <- VSURF_thres(train[c(-12)], train$mid_spread, mtry=100)
thres