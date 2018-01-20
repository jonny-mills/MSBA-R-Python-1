#####################
#### QUESTION 1 ####
#####################
#Author: Jonny Mills
#In this problem, I use the Weekly data set, which is part of the ISLR package,
#and contains lag variables and direction about stock outcomes from 1990-2010.

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

#####################
#### QUESTION 2 ####
#####################
#Overall objective: predict how much MPG a car gets based on certain predictors
rm(list=ls())
library(ISLR)

set.seed(5072)
attach(Auto)

#create a binary variable, mpg01
#assign if value above median, and 0 otherwise
med <- median(Auto$mpg)
mpg01 <- rep(0,length(Auto$mpg))
mpg01[Auto$mpg > med] <- 1 #assign a value of 1 to mpg01 whenever the index is greater than the median
my_data <- data.frame(Auto, mpg01)

n <- nrow(my_data)

trainprop <- .8
testprop <- .2

train <- sample(n,n*trainprop)
test <- sample(setdiff(1:n,train),testprop * n)


train_data <- my_data[train,]
test_data <- my_data[test,]


#perform logistic regression on training data
glm.fit = glm(mpg01 ~ cylinders + displacement + weight, data = train_data, 
              family = binomial)
glm.probs <- predict(glm.fit, test_data, type = "response") #predict glm01 using the test data
glm.pred <- rep(0,length(glm.probs))
glm.pred[glm.probs > 0.5] <- 1

mpg01.test <- mpg01[test]
t <- table(mpg01.test,glm.pred)

#5 parameters of accuracy
(prop_correct_predictions <- (t[1] + t[4])/sum(t)) #total correct predictions/total results in table
(overall_error_rate <- 1 - prop_correct_predictions)
(Type_1_error_rate <-  t[3]/sum(t[1,])) #FP/N   proportion of false positives
(Type_2_error_rate <- t[2]/sum(t[2,])) #FN/P
(Power <- t[4]/sum(t[2,])) #all of the correctly classified positives/all of the true positives
(Precision <- t[4]/sum(t[,2])) #all of the correctly classified positives/all the predictor said were positive

#LDA
library(MASS)
lda.fit <- lda(mpg01 ~ cylinders + displacement + weight, data = train_data, 
              family = binomial)
lda.pred <- predict(lda.fit,test_data, type="response")

t <- table(mpg01.test, lda.pred$class)
#5 parameters of accuracy

(prop_correct_predictions <- (t[1] + t[4])/sum(t)) #total correct predictions/total results in table
(overall_error_rate <- 1 - prop_correct_predictions)
(Type_1_error_rate <-  t[3]/sum(t[1,])) #FP/N   proportion of false positives
(Type_2_error_rate <- t[2]/sum(t[2,])) #FN/P
(Power <- t[4]/sum(t[2,])) #all of the correctly classified positives/all of the true positives
(Precision <- t[4]/sum(t[,2])) #all of the correctly classified positives/all the predictor said were positive



#QDA
#5 parameters of accuracy
qda.fit <- qda(mpg01 ~ cylinders + displacement + weight, data=train_data, family = binomial)
qda.predict <- predict(qda.fit, test_data, type="response")
t <- table(mpg01.test,qda.predict$class)

(prop_correct_predictions <- (t[1] + t[4])/sum(t)) #total correct predictions/total results in table
(overall_error_rate <- 1 - prop_correct_predictions)
(Type_1_error_rate <-  t[3]/sum(t[1,])) #FP/N   proportion of false positives
(Type_2_error_rate <- t[2]/sum(t[2,])) #FN/P
(Power <- t[4]/sum(t[2,])) #all of the correctly classified positives/all of the true positives
(Precision <- t[4]/sum(t[,2])) #all of the correctly classified positives/all the predictor said were positive

#KNN, k=1

library(class)
train.x <- cbind(cylinders,displacement,weight)[train, ] #how to have multiple variables from train
test.x <- cbind(cylinders,displacement,weight)[test,]
train.mpg01 <- mpg01[train]

knn.pred <- knn(train.x, test.x, train.mpg01, k=1)
t <- table(mpg01.test,knn.pred)

#5 parameters of accuracy
(prop_correct_predictions <- (t[1] + t[4])/sum(t)) #total correct predictions/total results in table
(overall_error_rate <- 1 - prop_correct_predictions)
(Type_1_error_rate <-  t[3]/sum(t[1,])) #FP/N   proportion of false positives
(Type_2_error_rate <- t[2]/sum(t[2,])) #FN/P
(Power <- t[4]/sum(t[2,])) #all of the correctly classified positives/all of the true positives
(Precision <- t[4]/sum(t[,2])) #all of the correctly classified positives/all the predictor said were positive

#KNN, various values of k

numreps <- 10
knn.error <- rep(0,numreps)

for(k in 1:numreps) {
      knn.pred <- knn(train.x, test.x, train.mpg01, k=k)
      knn.error[k] <- mean(knn.pred != mpg01.test)
}
which.min(knn.error)

#knn value of 3 returns the lowest error
knn.pred <- knn(train.x, test.x, train.mpg01, k=3)
t <- table(mpg01.test,knn.pred)

(prop_correct_predictions <- (t[1] + t[4])/sum(t)) #total correct predictions/total results in table
(overall_error_rate <- 1 - prop_correct_predictions)
(Type_1_error_rate <-  t[3]/sum(t[1,])) #FP/N   proportion of false positives
(Type_2_error_rate <- t[2]/sum(t[2,])) #FN/P
(Power <- t[4]/sum(t[2,])) #all of the correctly classified positives/all of the true positives
(Precision <- t[4]/sum(t[,2])) #all of the correctly classified positives/all the predictor said were positive

#which methods appears to provide the best results?
# looking at the proportions of correct prediction, doing a test in which knn=3 is tied for the best with 
# the test with qda


#####################
#### QUESTION 3 ####
#####################
rm(list=ls())
set.seed(5072)
library(MASS)

#suburb has crime rate above or below the median
#assign a 0 if below the median, assign a 1 if above the median

attach(Boston)
crime01 = rep(0,length(crim)) #make a list of numbers the length of crime, all with 0. We will populate
crime01[crim > median(crim)] = 1 #assign a 1 if the datapoint is greater than the median
Boston = data.frame(Boston, crime01) #append crime01 to the Boston dataframe

#80/20 test to training

n <- nrow(Boston)
train = sample(n,.8 * n)
test = sample(setdiff(1:n,train), .2 * n)

Boston.train = Boston[train,] 
Boston.test = Boston[test,]
crime01.test = crime01[test]


#logistic regression
glm.fit = glm(crime01 ~ nox + rad + dis, data = Boston.train, family = binomial) #train the model
glm.probs = predict(glm.fit, Boston.test, type = "response")
glm.pred = rep(0,nrow(Boston.test))
glm.pred[glm.probs > .5] = 1

#make a table

t <- table(crime01.test,glm.pred)

(prop_correct_predictions <- (t[1] + t[4])/sum(t)) #total correct predictions/total results in table
(overall_error_rate <- 1 - prop_correct_predictions)
(Type_1_error_rate <-  t[3]/sum(t[1,])) #FP/N   proportion of false positives
(Type_2_error_rate <- t[2]/sum(t[2,])) #FN/P
(Power <- t[4]/sum(t[2,])) #all of the correctly classified positives/all of the true positives
(Precision <- t[4]/sum(t[,2])) #all of the correctly classified positives/all the predictor said were positive



#LR confusion mx evaluation

#LDA
lda.fit <- lda(crime01 ~ nox + rad + dis, data=Boston.train) #using linear discriminant analysis, predict direction on lag2 from 1990-2008
lda.pred <- predict(lda.fit, Boston.test)
mean(lda.pred$class != crime01.test)
t <- table(crime01.test, lda.pred$class) #comparing the direction we predicted for 2009-2010 vs. the actual direction

#5 parameters of accuracy
(prop_correct_predictions <- (t[1] + t[4])/sum(t)) #total correct predictions/total results in table
(overall_error_rate <- 1 - prop_correct_predictions)
(Type_1_error_rate <-  t[3]/sum(t[1,])) #FP/N   proportion of false positives
(Type_2_error_rate <- t[2]/sum(t[2,])) #FN/P
(Power <- t[4]/sum(t[2,])) #all of the correctly classified positives/all of the true positives
(Precision <- t[4]/sum(t[,2])) #all of the correctly classified positives/all the predictor said were positive

#KNN

library(class)
train.x = cbind(nox, rad, dis, crime01)[train,]
test.x = cbind(nox, rad, dis, crime01)[test, ]
train.crime01 = crime01[train]


numreps <- 10
knn.error <- rep(0,numreps)

for(k in 1:numreps) {
      knn.pred <- knn(train.x, test.x, train.crime01, k=k)
      knn.error[k] <- mean(knn.pred != crime01.test)
}
which.min(knn.error)

knn.pred = knn(train.x,test.x,train.crime01,k=3)
mean(knn.pred != crime01.test)


t <- table(crime01.test, lda.pred$class) #comparing the direction we predicted for 2009-2010 vs. the actual direction

#5 parameters of accuracy
(prop_correct_predictions <- (t[1] + t[4])/sum(t)) #total correct predictions/total results in table
(overall_error_rate <- 1 - prop_correct_predictions)
(Type_1_error_rate <-  t[3]/sum(t[1,])) #FP/N   proportion of false positives
(Type_2_error_rate <- t[2]/sum(t[2,])) #FN/P
(Power <- t[4]/sum(t[2,])) #all of the correctly classified positives/all of the true positives
(Precision <- t[4]/sum(t[,2])) #all of the correctly classified positives/all the predictor said were positive


#####################
#### QUESTION 4 ####
#####################
rm(list=ls())
set.seed(5072)
x=rnorm(100)
y = x - 2 * x^2 + rnorm(100)

Data <- data.frame(x,y)
plot(x,y)

set.seed(123)

library(boot)
#model i 
glm.fit = glm(y~x) #create a least squares model
cv.glm(Data,glm.fit)$delta #the LOOCV for simple linear regression model

#model ii
glm.fit = glm(y~poly(x,2))
cv.glm(Data,glm.fit)$delta

#model iii
glm.fit = glm(y~poly(x,3))
cv.glm(Data,glm.fit)$delta

#model iv
glm.fit = glm(y~poly(x,4))
cv.glm(Data,glm.fit)$delta

#model 2 had the lower cv, followed closely by model 3
set.seed(456)
#model i 
glm.fit1 = glm(y~x) #create a least squares model.
cv.glm(Data,glm.fit1)$delta #the LOOCV for simple linear regression model

#model ii
glm.fit2 = glm(y~poly(x,2))
cv.glm(Data,glm.fit2)$delta
#compares data with glm.fit

#model iii
glm.fit3 = glm(y~poly(x,3))
cv.glm(Data,glm.fit3)$delta
summary(glm.fit)

#model iv
glm.fit4 = glm(y~poly(x,4))
cv.glm(Data,glm.fit4)$delta

#yes, they are the same as what we got in d. This makes sense because the true form of Y doesn't 
#y = x - 2 * x^2 + rnorm(100). Even if X changes, it will still fit the model 

#the model with 2 degrees had the smallest LOOCV error. This is what I expected because the model is a quadratic 
# model with 2 degrees matches the true y model (as it also is a 2 degreee model)

summary(glm.fit1)
summary(glm.fit2)
summary(glm.fit3)
summary(glm.fit4)

# the first model has statistical significance for X
# the seccond model has statistical significance for X and X^2
# the third model has statistical significance for X and X^2
# the fourth model has statistical significance for X and X^2
# this agrees with CV conclusions because the CV model has a y = x - 2 * x^2 + rnorm(100),
# so it actually uses x and x^2 in the true form

