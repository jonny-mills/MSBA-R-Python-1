
#Author: Jonny Mills
# In this problem, I used R to analyze a data set (called Weekly, which contains stock outcomes and information from 1990-2010).
# Here, I use classification methods to predict stock outcomes

rm(list=ls())
options(warn=-1)   # Supress warning messages
library(ISLR)
set.seed(5072)
attach(Weekly)
#1089 rows, 9 columns. Years: 1990-2010

confusion_matrix_results <- function(actual,predicted){
      t <- table(actual, predicted) #compare the predicted results to the actual results
      (prop_correct_predictions <- (t[1] + t[4])/sum(t)) #total correct predictions/total results in table
      (overall_error_rate <- 1 - prop_correct_predictions)
      (Type_1_error_rate <-  t[3]/sum(t[1,])) #FP/N   proportion of false positives
      (Type_2_error_rate <- t[2]/sum(t[2,])) #FN/P
      (Power <- t[4]/sum(t[2,])) #all of the correctly classified positives/all of the true positives
      (Precision <- t[4]/sum(t[,2])) #all of the correctly classified positives/all the predictor said were positive
      print(t)
      print(paste("Proportion of correct predictions:",round(prop_correct_predictions,3)))
      print(paste("Overall error rate:", round(overall_error_rate,3)))
      print(paste("Type 1 error rate: ",round(Type_1_error_rate,3)))
      print(paste("Type 2 error rate: ",round(Type_2_error_rate,3)))
      print(paste("Power: ",round(Power,3)))
      print(paste("Precision: ",round(Precision,3)))
}

#Here, I use the full data set to logistic regression with Direction as the response and the 5 lag variables plus Volume as predictors
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 +Lag4 +Lag5 +Volume, data=Weekly, family = binomial)
summary(glm.fit)
#Lag2 is a significant variable, all others are not significant

#display a confusion matrix

glm.probs = predict(glm.fit, type = "response") #predict the direction
glm.pred = rep("Down", length(glm.probs)) #make all the entries down temporarily
glm.pred[glm.probs > 0.5] <- "Up" #if the value in glm.probs is over .5, change the entry in glm.pred to "up"

confusion_matrix_results(Direction, glm.pred)

#assume that down is the null hypothesis, and up is the alternate hypothesis
#it seems that when the direction is down, the predictor often misclassifies as "up" (indicated by the high Type 1 error)
#when the direction is "up", the predictor often corretly classifies (indicated by the low type II error)


#Next, I train the data to predict outcomes from 1990-2008. I then use this to Predict outcomes in 2009 and 2010
#Compare your prediction to the actual data in 2009 and 2010, and then create a confusion matrix to display the accuracy of my results
train = (Year < 2009) #train will create a subset of data from Weekly, only years 1990-2008
Weekly.9008 = Weekly[train, ] #1990-2008 data
Weekly.0910 = Weekly[!train, ] #the 2009 and 2010 data
glm.fit = glm(Direction ~ Lag2, data = Weekly.9008, family = binomial) #do a logistic regression on 90_08 data with 1 predictor (Lag 2)
glm.probs = predict(glm.fit, Weekly.0910, type = "response") #predict the response of 90_10 data based on 90_08 data
glm.pred = rep("Down", length(glm.probs))
glm.pred[glm.probs > 0.5] = "Up"

Direction.0910 = Direction[!train]


#5 parameters of accuracy
confusion_matrix_results(Direction.0910,glm.pred)
#again, the model tends to overclassify the Direction as "up", leading one to potentially "tune" the model and only 
#classify Direction higher than say 0.7 as "up" rather than 0.5

#Do the same for LDA
library(MASS)
lda.fit <- lda(Direction ~ Lag2, data=Weekly, subset = train) #using linear discriminant analysis, predict direction on lag2 from 1990-2008
lda.pred <- predict(lda.fit,Weekly.0910)

confusion_matrix_results(Direction.0910, lda.pred$class)

#Do the same for KNN with k=5 nearest neighbor
library(class)
train.x <- as.matrix(Lag2[train]) #vector of lag2 as a mx from 1990-2008
test.x <- as.matrix(Lag2[!train]) #vector of lag2 as a mx from 2009-2010
train.Direction <- Direction[train] #direction of stock from 1990-2008
test.Direction <- Direction[!train]

knn.pred <- knn(train.x, test.x, train.Direction, k=5)
confusion_matrix_results(test.Direction, knn.pred)

#which of these methods appears to be giving the best results?
#in comparing the proportion of correction predictions, that logistic regression and LDA with lag2 being the only predictor appears to yield the best result
#at .625 % accuracy
