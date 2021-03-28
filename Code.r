data <- read.csv("2019.csv") #import dataset
library(dplyr) #import all libraries
library(pROC)
library(dplyr)
library(caTools)
library(Metrics)

library(lmtest) # all ml model tests are in this library
options(warn=-1)

#data summary task
nrow(data) 
summary(data)
data <- data[,-2] #dropping names of countries becuase they're insignificant
columns<-colnames(data, do.NULL = TRUE, prefix = "col") 
columns

#correlation between happiness score and everyother attribute
cor(data$Score, data$GDP.per.capita)
cor(data$Score, data$Social.support)
cor(data$Score, data$Healthy.life.expectancy)
cor(data$Score, data$Generosity)
cor(data$Score, data$Freedom.to.make.life.choices)
cor(data$Score, data$Perceptions.of.corruption)



#Train test split
set.seed(2)
split<-sample.split(data, SplitRatio=0.7)
split
train<-subset(data, split="TRUE")
test<-subset(data, split="FALSE")


Model2 <-lm(formula = data$Score ~data$GDP.per.capita,data=data)

#a loop for training model of score attribute with everyother attribute one by one and then checking accuracies using 3 methods and testing model with 3 tests
counter=1
for (val in columns) {
  #filterdData=data[val]
  print(counter)
  print(val)  
  
  #Linear regression model is being trained with score and  each attribute from loop
  Model <-lm(formula = train$Score ~train[,counter],data=train)
  #print(summary(Model))
  
  pred<-predict(Model,test) #trained model is being predicted
  pred
  
  #accuracy 3 techniques
  #coefficent of determination or r squared
  Y_test<- test$Score
  error <- Y_test - pred
  R2=1-sum(error^2)/sum((Y_test- mean(Y_test))^2)
  print(paste("R2",R2))
  
  #mean squared error
  mean_squared_error <- mse(test$Score,pred)
  print(paste("mean_squared_error",mean_squared_error))
  
  #root mean squared error
  root_mean_squared_error <- rmse(test$Score,pred)
  print(paste("root mean_squared_error",root_mean_squared_error))

  # linear regression validation/testing
  #https://www.rdocumentation.org/packages/lmtest/versions/0.9-38
  #https://www.youtube.com/watch?v=SiUEJWC4zok&t=59s
  #https://en.wikipedia.org/wiki/Ramsey_RESET_test
  RamseyRESET<-resettest(Model,type="regressor",data=data)
  print(RamseyRESET)
  
  
  #https://www.rdocumentation.org/packages/lmtest/versions/0.9-38/topics/bptest
  #https://en.wikipedia.org/wiki/Breusch%E2%80%93Pagan_test
  BPTEST<-bptest(Model, varformula = NULL, studentize = TRUE, data = data)
  print(BPTEST)
  #This is the basis of the Breusch-Pagan test. It is a chi-squared test: the test statistic is distributed n??2 with k degrees of freedom. If the test statistic has a p-value below an appropriate threshold (e.g. p < 0.05) then the null hypothesis of homoskedasticity is rejected and heteroskedasticity(varience of error and inability of model to perfectly predict. the variable which is predicting is not giving perfect insight) assumed.
  
  
  #https://www.rdocumentation.org/packages/lmtest/versions/0.9-38/topics/jtest
  #https://mpra.ub.uni-muenchen.de/14637/1/ON_THE_J_TEST_FOR_NON-NESTED_HYPOTHESES_August_2008.pdf
  #coxtest(Model, Model2)
  JTEST<-jtest(Model, Model2)
  
  #encomptest(Model, Model2)
  print(JTEST) #compares two fitted models gdp with score and score with some other column and always shows gdp has better with score then anyother attribute. 
  
    #rmse
  counter<-counter+1
}

