#The steps I will use in this script are the following

#STEP1
# I do not partition data. I am building the Random Forest using 100% of data.
#Produce variable importance based on 100% of the data.
#I do this using two methodologies; i) standard VI of random.Forest ii) VI according to depth using ggRandomForest 

#STEP2
#I perform a 5th fold cross validation to find the hyperparameter which minimizes the error across all sets.


#setup folders and filename
datafilename <-"Raw_data_final.csv"
datafolder <- "C:/Users/despm/OneDrive/Documents/My PhD Work/FINAL DATA WORK/"
datafile <- paste(datafolder, datafilename, sep="")

#read data from csv
mydata <- read.table(datafile, header=TRUE, sep=",")

#give name to retrieve columns names of mydata
columns_names <- colnames(mydata)

#select columns needed in data frame. 10 reg per and 17 AP excl
selected_columns <- c(6,7,9,11,12,13,14,15,17,19,23)

#calculate load
#mydata_selected[, 11] <-mydata_selected[, 11] - mydata_selected[, 10]

#develop data frame
mydata_selected <- mydata[,selected_columns]

#count na by column
sapply(mydata_selected, function(x) sum(is.na(x)))

class(mydata_selected)

#find  columns where data is numeric
columns_numeric_logical <- sapply(mydata_selected,is.numeric)
columns_numeric <- which(columns_numeric_logical == TRUE)

#imputing data for numeric columns
for (i in columns_numeric)  {
  mydata_selected[,i]  = ifelse(is.na( mydata_selected[,i] ), 
                                median(mydata_selected[,i], na.rm =TRUE),mydata_selected[,i] )
}

sapply(mydata_selected, function(x) sum(is.na(x)))

#omit na for categorical columns
mydata_selected <- na.omit(mydata_selected)
sapply(mydata_selected, function(x) sum(is.na(x)))

#dependent variables

columns_names_dependent <- colnames(mydata_selected)
mydata_selected_dependent_vector <- c(11)
mydata_selected_dependent <- mydata_selected[,mydata_selected_dependent_vector]

mydata_selected_independent_all <- mydata_selected[,-mydata_selected_dependent_vector]
mydata_selected_names_independent_all <- colnames(mydata_selected_independent_all)
#mydata_selected_independent_vector <- c(1,2,...)
mydata_selected_independent <- mydata_selected_independent_all[,mydata_selected_names_independent_all]

for (i in colnames(mydata_selected_dependent) ){
  print(i)
}

#remove outliers

#data.iqr <- mydata_selected

# Create a variable/vector/collection of the column names you want to remove outliers on.
#vars <- c("term", "size", "NT_EL" )

# Create a variable/vector/collection of the column names you want to remove outliers on.
#vars <- c("mid_spread")

# Create a variable to store the row id's to be removed
#Outliers <- c()

# Loop through the list of columns you specified
#for(i in vars){
  
  # Get the Min/Max values
  #max <- quantile(data.iqr[,i],0.75, na.rm=TRUE) + (IQR(data.iqr[,i], na.rm=TRUE) * 1.5 )
  #min <- quantile(data.iqr[,i],0.25, na.rm=TRUE) - (IQR(data.iqr[,i], na.rm=TRUE) * 1.5 )
  
  # Get the id's using which
  #idx <- which(data.iqr[,i] < min | data.iqr[,i] > max)
  
  # Output the number of outliers in each variable
  #print(paste(i, length(idx), sep=''))
  
  # Append the outliers list
  #Outliers <- c(Outliers, idx) 
#}

# Sort, I think it's always good to do this
#Outliers <- sort(Outliers)

# Remove the outliers
#data.iqr2 <- data.iqr[-Outliers,]

#Explore the data

head(mydata_selected)
dim(mydata_selected)
str(mydata_selected)
summary(mydata_selected)



#TRAIN MODEL WITH DEFAULT PARAMETERS
set.seed(222)
library(randomForest)
rf_default <- randomForest(mydata_selected[,-11], mydata_selected[,11], importance=TRUE, keep.forest = TRUE, keep.inbag = TRUE)
#rf_default <- randomForest(data.iqr2[,-10], data.iqr2[,10], importance=TRUE, keep.forest = TRUE, keep.inbag = TRUE)
rf_default


#Plot the error rates or MSE of a randomForest object. I do not need to tune the parameter ntree-500 works fine
plot(rf_default, main= "MSE of Random Forest")


#CROSS VALIDATION FOR TUNING mtry
#packages
library(randomForest)
library(mlbench)
library(caret)

#Grid Search for mtry-tAnother search is to define a grid of algorithm parameters to try.
#Each axis of the grid is an algorithm parameter,
#and points in the grid are specific combinations of parameters. 
#Because we are only tuning one parameter, the grid search is a linear search through a vector of candidate values.
#I use grid search and not random search because mtry has an upper bound which is the number of independent variables in my data set

control <- trainControl(method="cv", number=5, search="grid")
set.seed(222)
metric<- "Rsquared"
tunegrid <- expand.grid(.mtry=c(1:10))
rf_gridsearch <- train(mydata_selected[,-11], mydata_selected[,11], method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch, main="5-fold CV for mtry tuning")

#imputation
#rfimp<-rfImpute(mydata_selected[,-11], mydata_selected[,11], importance=TRUE, mtry=3, iter=5, ntree=500)
#rf_imp<-randomForest(rfimp[,-1], rfimp[,1], importance=TRUE, mtry=3)

IMP<-importance(rf_imp, type=1, scale=TRUE)
varImp<-varImpPlot(rf_imp, sort=TRUE,type=1, main = 'Variable Importance Plot, %IncMSE', scale=TRUE)
getTree(rf_imp, k=1, labelVar = FALSE)




#TUNED MODEL WITH MTRY=4 BASED ON 5TH FOLD CV
set.seed(222)
rf_tuned<-randomForest(mydata_selected[,-11], mydata_selected[,11], importance=TRUE, mtry=3)
rf_tuned





#VARIABLE IMPORTANCE
set.seed(222)
IMP<-importance(rf_default, type=1, scale=TRUE)
varImp<-varImpPlot(rf_default, sort=TRUE,type=1, main = 'Variable Importance Plot, %IncMSE', scale=TRUE)
getTree(rf, k=1, labelVar = FALSE)
#min depth

library(randomForestSRC)
library(ggRandomForests)
set.seed(222)
#varselect<-var.select(mid_spread~., data=mydata_selected, method='md', ntree=500, mtry=3, nodesize=5, block.size=1, nsplit=0)
varselect<-var.select(mid_spread~., data=mydata_selected, method='md', ntree=500, nodesize=5, mtry=3, block.size=1, conservative = "medium", refit = TRUE)
mdimp<- gg_minimal_depth(varselect)
plot(mdimp)+ theme_bw()+ggtitle("Variable Importance, Minimal Depth")+theme(plot.title = element_text(hjust = 0.5))

#find interactions
set.seed(222)
rfsrc <- rfsrc(mid_spread~., data=mydata_selected, ntree=500, nodesize=5, mtry=3,  block.size=1)
interaction_rf <- find.interaction(rfsrc)
data(interaction_rf)
# Plot the results in a single panel.
plot(gg_interaction(interaction_rf), panel=TRUE)





set.seed(222)

varselect_vimp<-var.select(mid_spread~., data=mydata_selected, method='', ntree=500, mtry=4)

plot(varimp_mmd$variable,IMP)
set.seed(222)
rftuneOOB<- tuneRF(mydata_selected[,-11], mydata_selected[,11], mtryStart=3, ntreeTry=500, stepFactor=2, improve=0.05,
                   trace=TRUE, plot=TRUE, doBest=FALSE)

#COMPARE TWO DIFFERENT RANKINGS OF VAR IMPORTANCE
#CREATE DATA FRAME FOR MIN DEPTH IMP

Vimp_depth <- c(42333.684, 431.289, -1199.406, -2807.698, 163.124, 823.086, -898.783, -1243.682, -616.839)
variable <- c("NT_EL", "diversifier", "size", "rate_SnP", "term", "limit_type", "trigger", "loss_year", "vendor")

varimp_mmd <- data.frame(variable,Vimp_depth)



