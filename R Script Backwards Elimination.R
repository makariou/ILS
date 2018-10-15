#setup folders and filename
## tianlin
datafilename <-"Raw_data_final.csv"
datafolder <- "C:/Users/despm/OneDrive/Documents/My PhD Work/FINAL DATA WORK/"
datafile <- paste(datafolder, datafilename, sep="")

#read data from csv
mydata <- read.table(datafile, header=TRUE, sep=",")

#give name to retrieve columns names of mydata
columns_names <- colnames(mydata)

#select columns needed in data frame
selected_columns <- c(6,7,9,10,11,12,13,14,15,17,19,23)

#develop data frame
mydata_selected <- mydata[,selected_columns]

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

#dependent variables

columns_names_dependent <- colnames(mydata_selected)
mydata_selected_dependent_vector <- c(12)
mydata_selected_dependent <- mydata_selected[,mydata_selected_dependent_vector]

mydata_selected_independent_all <- mydata_selected[,-mydata_selected_dependent_vector]
mydata_selected_names_independent_all <- colnames(mydata_selected_independent_all)
#mydata_selected_independent_vector <- c(1,2,...)
mydata_selected_independent <- mydata_selected_independent_all[,mydata_selected_names_independent_all]

for (i in colnames(mydata_selected_dependent) ){
  print(i)
}

#Backwards elimination

#Model_1: Mid spread ~ all independent vars (total of 11)
model_1 <- lm(mydata_selected_dependent ~ ., data =  mydata_selected_independent)
summary(model_1)

#Model_2: -  trigger (5)
model_2 <- lm(mydata_selected_dependent ~ ., data =  mydata_selected_independent[c(-5)])
summary(model_2)

#Model_3: - vendor (8)
model_3 <- lm(mydata_selected_dependent ~ ., data =  mydata_selected_independent[c(-5,-8)])
summary(model_3)

#Model_4: - loss year (9)
model_4 <- lm(mydata_selected_dependent ~ ., data =  mydata_selected_independent[c(-5,-8, -9)])
summary(model_4)

#Model_5: - term (1)
model_5 <- lm(mydata_selected_dependent ~ ., data =  mydata_selected_independent[c(-5,-8, -9, -1)])
summary(model_5)

#Model_6: - region peril (4)
model_6 <- lm(mydata_selected_dependent ~ ., data =  mydata_selected_independent[c(-5,-8, -9, -1, -4)])
summary(model_6)

#Library for Regression Diagnostics
library(olsrr)

#A.RESIDUAL DIAGNOSTICS

#1. Residual QQ Plot: Graph for detecting violation of normality assumption.
ols_plot_resid_qq(model_6)

#2. Residual Normality Test: Test for detecting violation of normality assumption.
ols_test_normality(model_6)
#ALSO
#Correlation between observed residuals and expected residuals under normality.
ols_test_correlation(model_6)

#3.Residual vs Fitted Values Plot
ols_plot_resid_fit(model_6)

#4.Residual Histogram
ols_plot_resid_hist(model_6)

#B. HETEROSCEDASTICITY
#Method I: Through Statistical Tests
#1. Breusch Pagan Test

#Use fitted values of the model
ols_test_breusch_pagan(model_6)

#Use independent variables of the model
ols_test_breusch_pagan(model_6, rhs = TRUE)

#Method II: Graphically

#1. Residuals vs Fitted Values #2. Standardized Residuals vs Fitted Values #3. Residuals vs Leverage #4. Also use normal 
plot(model_6)
#NOTE: If there is absolutely no heteroscedastity, you should see a completely random, equal distribution of points throughout the range of X axis and a flat red line.

#C. COLLINEARITY DIAGNOSTICS
ols_coll_diag(model_6)

#MODEL FIT ASSESSMENT_ Diagnostics Panel 
#Panel of plots for regression diagnostics
ols_plot_diagnostics(model_6)

