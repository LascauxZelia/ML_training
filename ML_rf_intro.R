library(dplyr) #Data manipulation
library(caret) #Make prediction
library(randomForest) #Model
library(e1071) 
library(caTools) #For Logistic regression
library(ROCR) #Performance metrics

## Import data
data(iris) 
str(iris) 

# Splitting data in train and test data 
split <- sample.split(iris, SplitRatio = 0.7) 
split 
  
train <- subset(iris, split == "TRUE") 
test <- subset(iris, split == "FALSE")

## Fitting Random Forest to the train dataset
set.seed(120)  # Setting seed 
classifier_RF = randomForest(x = train[-5], 
                             y = train$Species, 
                             ntree = 500) 
  
classifier_RF 

##Output
#Call:
# randomForest(x = train[-5], y = train$Species, ntree = 500) 
#               Type of random forest: classification
#                     Number of trees: 500
#No. of variables tried at each split: 2
#        OOB estimate of  error rate: 6.67%
#Confusion matrix:
#           setosa versicolor virginica class.error
#setosa         30          0         0  0.00000000
#versicolor      0         28         2  0.06666667
#virginica       0          4        26  0.13333333

## That give us the classficiation error for each species. 

# Predicting the Test set results 
y_pred = predict(classifier_RF, newdata = test[-5]) 
  
# Confusion Matrix 
confusion_mtx = table(test[, 5], y_pred) 
confusion_mtx 
##Output
#            y_pred
#             setosa versicolor virginica
#  setosa         20          0         0
#  versicolor      0         16         4
#  virginica       0          0        20

##So, 20 Setosa are correctly classified as Setosa. Out of 23 versicolor,
##20 Versicolor are correctly classified as Versicolor, and 3 are classified as virginica. 
##17 virginica are correctly classified as virginica.

# Plotting model 
plot(classifier_RF) 
##Error rate is stabilized with an increase in the number of trees

# Importance plot 
importance(classifier_RF) 
#Output 
#             MeanDecreaseGini
#Sepal.Length         6.114620
#Sepal.Width          1.621302
#Petal.Length        27.327293
#Petal.Width         24.226785

## Meaning that: Petal.Width is the most important feature followed by Petal.Length, Sepal.Width and Sepal.Length.

# Variable importance plot 
varImpPlot(classifier_RF) 

######### Random Forest for Regression ##############

#Import data
print(head(airquality))

# Create random forest for regression 
ozone.rf <- randomForest(Ozone ~ ., data = airquality, mtry = 3, 
                         importance = TRUE, na.action = na.omit)
## Train the model 
##RandomForest(formula, ntree=n, mtry=FALSE, maxnodes = NULL)
##Arguments:
##- Formula: Formula of the fitted model
##- ntree: number of trees in the forest
##- mtry: Number of candidates draw to feed the algorithm. By default, it is the square of the number of columns.
##- maxnodes: Set the maximum amount of terminal nodes in the forest
##- importance=TRUE: Whether independent variables importance in the random forest be assessed
##
## We need to find the best model 
## -Evaluate the model with the default setting
## -Find the best number of mtry
## -Find the best number of maxnodes
## -Find the best number of ntrees
## -Evaluate the model on the test dataset

print(ozone.rf)
##Output
#Call:
# randomForest(formula = Ozone ~ ., data = airquality, mtry = 3,      importance = TRUE, na.action = na.omit) 
#               Type of random forest: regression
#                     Number of trees: 500
#No. of variables tried at each split: 3
#          Mean of squared residuals: 301.1784
#                    % Var explained: 72.55

plot(ozone.rf)

##How to find the optimal mtry value
mydata= read.csv("data_test.csv")

mtry <- tuneRF(mydata[-1],mydata$Creditability, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)
## That allow us to select mtry value with minimum out of bag (OOB) error. 

## Try the model with the best mtry value
set.seed(71)
rf <-randomForest(Creditability~.,data=mydata, mtry=best.m, importance=TRUE,ntree=500)
print(rf)
#Evaluate variable importance
importance(rf)
varImpPlot(rf)

## Basic intro to Random Forest using R

## First we need to split ou data set between training and validation sets
# Set random seed to make results reproducible:
set.seed(17)
# Calculate the size of each of the data sets:
data_set_size <- floor(nrow(iris)/2)
data_set_size
# Generate a random sample of "data_set_size" indexes
indexes <- sample(1:nrow(iris), size = data_set_size)
indexes
# Assign the data to the correct sets
training <- iris[indexes,]
validation1 <- iris[-indexes,]

# Perform training:
rf_classifier = randomForest(Species ~ ., data=training, ntree=100, mtry=2, importance=TRUE)
##-The first parameter specifies our formula: Species ~ . (we want to predict Species using each of the remaining columns of data).
##–ntree: number of trees to be generated. Test a range of values for this parameter and choose the one that minimises the OOB.
rf_classifier
varImpPlot(rf_classifier)
# Validation set assessment #1: looking at confusion matrix
prediction_for_table <- predict(rf_classifier,validation1[,-5])
table(observed=validation1[,5],predicted=prediction_for_table)
##Another way of assessing the performance of our classifier is to generate a ROC curve:
# Validation set assessment #2: ROC curves and AUC
# Calculate the probability of new observations belonging to each class
# prediction_for_roc_curve will be a matrix with dimensions data_set_size x number_of_classes
prediction_for_roc_curve <- predict(rf_classifier,validation1[,-5],type="prob")
# Use pretty colours:
pretty_colours <- c("#F8766D","#00BA38","#619CFF")
# Specify the different classes 
classes <- levels(validation1$Species)
# For each class
for (i in 1:3)
{
 # Define which observations belong to class[i]
 true_values <- ifelse(validation1[,5]==classes[i],1,0)
 # Assess the performance of classifier for class[i]
 pred <- prediction(prediction_for_roc_curve[,i],true_values)
 perf <- performance(pred, "tpr", "fpr")
 if (i==1)
 {
     plot(perf,main="ROC Curve",col=pretty_colours[i]) 
 }
 else
 {
     plot(perf,main="ROC Curve",col=pretty_colours[i],add=TRUE) 
 }
 # Calculate the AUC and print it to screen
 auc.perf <- performance(pred, measure = "auc")
 print(auc.perf@y.values)
}
