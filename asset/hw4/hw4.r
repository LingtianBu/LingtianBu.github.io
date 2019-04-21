library(glmnet)
library(AER)
rm(list = ls())
setwd("D:/All courses/Data Analysis for Economics(Microeconometrics)/HW4")
data<-read.csv("hw4.csv")

#split into training and test data sets
n<-nrow(data)
train_pos<-sample(n,round(n/2))
train<-data[train_pos,]
test<-data[-train_pos,]

#logistic regression
logit<-glm(gunlaw~.,train,family='binomial',control=list(maxit=100))
coeftest(logit)

#predict on test data
pre=predict(logit,test,type='response')
logitpred<-round(pre)
table(logitpred,test$gunlaw,dnn=c("predicted","Disagree"))
logiterr<-1-mean(logitpred==test$gunlaw)
logiterr

#Regularized by LASSO
# Fit on training data
x<-model.matrix(gunlaw~.,train) 
y<-train$gunlaw
lassofit.all<-glmnet(x,y,alpha=1,family="binomial") 
plot(lassofit.all,xvar="lambda")

# Cross Validation
cv.lasso <- cv.glmnet(x,y,alpha=1,family="binomial") 
plot(cv.lasso)
lambda.star <- cv.lasso$lambda.min
lassofit.star <- glmnet(x,y,alpha=1,lambda=lambda.star,family="binomial") 
coef(lassofit.star)

# Predict on test data
newx <- model.matrix(gunlaw ~.,test)
lassopred <- predict(lassofit.star,newx,type="class")
table(lassopred,test$gunlaw,dnn=c("predicted","Disagree"))
lassoerr <- 1-mean(lassopred==test$gunlaw)
lassoerr

#Regularized by Elastic Net
x<-model.matrix(gunlaw~.,train)
y<-train$gunlaw
elastic<-glmnet(x,y,alpha=0.3,family="binomial") 
plot(elastic,xvar="lambda")


#Cross Validation
cv.elastic<-cv.glmnet(x,y,alpha=0.3,family="binomial")
plot(cv.elastic)
lambda.star_elastic<-cv.elastic$lambda.min
elastic.star<-glmnet(x,y,alpha=0.3,lambda=lambda.star_elastic,family="binomial")
coef(elastic.star)

#Predict on the test data
newx<-model.matrix(gunlaw~.,test)
elasticpred<-predict(elastic.star,newx,type='class')
table(elasticpred,test$gunlaw,dnn=c("Predicted","Disagree"))
elasticerr<-1-mean(elasticpred==test$gunlaw)
elasticerr