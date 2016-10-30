
library(caret)
library(ISLR)
library(MASS)
library(class)
library(ROCR)

attach(Weekly)



#(a) Produce some numerical and graphical summaries of the Weekly data. Do there appear to
#be any patterns?

summary(Weekly)
str(Weekly)
pairs(~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data = Auto)



#There is a similarity between the data in lag 1 to 5 when compared between each other and the volume. 



#(b) Use the full data set to perform a logistic regression with Direction as the response and
#the five lag variables plus Volume as predictors. Use the summary function to print the
#results. Do any of the predictors appear to be statistically significant? If so, which ones?

#created model using Lag variables and volume on full dataset

logistic_model = glm(Direction ~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Weekly, family="binomial")
summary(logistic_model)

#Lag2 appears to be significant



#(c) Compute the confusion matrix and overall fraction of correct predictions. Explain what
#the confusion matrix is telling you about the types of mistakes made by logistic regression.

#predict 

logistic_probs = predict(logistic_model, Weekly , type = "response")
logistic_pred_y = rep("Down", nrow(Weekly)) # default assignment
logistic_pred_y[logistic_probs> 0.6] = "Up"
table(logistic_pred_y, Weekly$Direction)
mean(logistic_pred_y != Weekly$Direction)
confmx=table(logistic_pred_y, Weekly$Direction)
overall_accuracy=sum(diag(confmx))/sum(confmx)
overall_accuracy

#Overall accuracy is 47.38%
#There is a heavy number pf false negative predictions.
#True Negative : 433
#True positive: 83
#False negative: 522
#Flase negative: 51
##############################################

#(d) Now fit the logistic regression model using a training data period from 1990 to 2008, with
#Lag2 as the only predictor. Compute the confusion matrix and the overall fraction of correct
#predictions for the held out data (that is, the data from 2009 and 2010)

set.seed(1)

train = Year <= 2008
test = !train
training_data = Weekly[train,]
testing_data = Weekly[test,]
train_y= Direction[train]
testing_y = Direction[test]
str(testing_data)

logistic_model2 = glm(Direction ~Lag2,data=training_data, family="binomial")
summary(logistic_model2)
logistic_probs2 = predict(logistic_model2, testing_data , type = "response")
logistic_pred_y2 = rep("Down", nrow(testing_data)) # default assignment
logistic_pred_y2[logistic_probs2> 0.5] = "Up"
table(logistic_pred_y2, testing_y)
mean(logistic_pred_y2 != testing_y)

#mean error rate = 0.375

################################

#(e) Repeat (d) using LDA.


lda_model = lda(Direction~Lag2, data=training_data)
summary(lda_model)

lda_pred_y = predict(lda_model, testing_data)
table(lda_pred_y$class, testing_y)
mean(lda_pred_y$class != testing_y)

  #mean error rate = 0.375
  
  ##################
  
  #(f) Repeat (d) using QDA.
  
  qda_model = qda(Direction~Lag2, data=training_data)
  qda_pred_y = predict(qda_model, testing_data)
  table(qda_pred_y$class, testing_y)
mean(qda_pred_y$class != testing_y)

#mean error rate = 0.413

#######################

#(g) Repeat (d) using KNN with K = 1.


train.x = as.data.frame(cbind(Lag2)[train,])
test.x = as.data.frame(cbind(Lag2)[!train,])

class(train.x)
knn_pred=knn(train.x, test.x, train_y, k=1)
conf= table(knn_pred,testing_y)
conf
mean(knn_pred != testing_y)
specificity(conf, negative = rownames(conf)[1])
sensitivity(conf, positive = rownames(conf)[2])

#mean error rate - 0.5


#################################################

#(h) Which of these methods appears to provide the best results on this data?

#The best method for this model seems to be logistic and lda with minimum misclassification rate 
#of 0.375


################################################

#(i) Experiment with different combinations of predictors, including possible transformations
#and interactions, for each of the methods. Report the variables, method, and associated
#confusion matrix that appears to provide the best results on the held out data. Note that you
#should also experiment with values for K in the KNN classifier.



#Interaction-------------------------------------------------------------------
gl_15 = glm(Direction ~Lag2+Lag1*Lag5,data=training_data,family="binomial")
glmprobs <- predict(gl_15,testing_data,type = "response")
log_pred = rep("Down", nrow(testing_data)) # default assignment
log_pred[glmprobs> 0.5] = "Up"
#confusion matrix
table(log_pred, testing_y)
mean(log_pred != testing_y)

#misclassifcation rate: 0.4615385

gl_25 = glm(Direction ~Lag2+Lag2*Lag5,data=training_data,family="binomial")
glmprobs <- predict(gl_25,testing_data,type = "response")
log_pred = rep("Down", nrow(testing_data)) # default assignment
log_pred[glmprobs> 0.5] = "Up"
#confusion matrix
table(log_pred, testing_y)


mean(log_pred != testing_y)

#misclassifcation rate: 0.4038462


gl_34 = glm(Direction ~Lag2+Lag3*Lag4,data=training_data,family="binomial")
glmprobs <- predict(gl_34,testing_data,type = "response")
log_pred = rep("Down", nrow(testing_data)) # default assignment
log_pred[glmprobs> 0.5] = "Up"

#confusion matrix
table(log_pred, testing_y)

mean(log_pred != testing_y)

#misclassifcation rate: 0.3653846


gl_35 = glm(Direction ~Lag2+Lag3*Lag5,data=training_data,family="binomial")
glmprobs <- predict(gl_35,testing_data,type = "response")
log_pred = rep("Down", nrow(testing_data)) # default assignment
log_pred[glmprobs> 0.5] = "Up"

#confusion matrix
table(log_pred, testing_y)
mean(log_pred != testing_y)

#misclassifcation rate: 0.375


#Interaction--------------------------------------------------------------
gl_22 = glm(Direction ~Lag2+I(Lag2^2),data=training_data,family="binomial")
glmprobs <- predict(gl_22,testing_data,type = "response")
log_pred = rep("Down", nrow(testing_data)) # default assignment
log_pred[glmprobs> 0.5] = "Up"

#confusion matrix
table(log_pred, testing_y)
mean(log_pred != testing_y)

#misclassifcation rate: 0.375

gl_23 = glm(Direction ~Lag2+I(Lag3^2),data=training_data,family="binomial")
glmprobs <- predict(gl_23,testing_data,type = "response")
log_pred = rep("Down", nrow(testing_data)) # default assignment
log_pred[glmprobs> 0.5] = "Up"

#confusion matrix
table(log_pred, testing_y)
mean(log_pred != testing_y)

#misclassifcation rate: 0.3846154


### lda model experimented

lda_model5 = lda(Direction~Lag2+Lag1*Lag5, data=training_data)
lda_pred_y5 = predict(lda_model5, testing_data)

#confusion matrix
table(lda_pred_y5$class, testing_y)
mean(lda_pred_y5$class != testing_y)

#misclassification rate = 0.4519231

lda_model6 = lda(Direction~Lag2+Lag2*Lag5, data=training_data)
lda_pred_y6 = predict(lda_model6, testing_data)

#confusion matrix
table(lda_pred_y6$class, testing_y)
mean(lda_pred_y6$class != testing_y)

#misclassification rate = 0.4038462

lda_model7 = lda(Direction~Lag2+Lag3*Lag4, data=training_data)
lda_pred_y7 = predict(lda_model7, testing_data)

#confusion matrix
table(lda_pred_y7$class, testing_y)
mean(lda_pred_y7$class != testing_y)

#misclassification rate =  0.3653846

lda_model8 = lda(Direction~Lag2+Lag3*Lag5, data=training_data)
lda_pred_y8 = predict(lda_model8, testing_data)

#confusion matrix
table(lda_pred_y8$class, testing_y)
mean(lda_pred_y8$class != testing_y)

#misclassification rate = 0.3846154


lda_model9 = lda(Direction~Lag2+I(Lag2^2), data=training_data)
lda_pred_y9 = predict(lda_model9, testing_data)


#confusion matrix
table(lda_pred_y9$class, testing_y)
mean(lda_pred_y9$class != testing_y)

#misclassification rate = 0.3846154

lda_model10 = lda(Direction~Lag2+I(Lag3^2), data=training_data)
lda_pred_y10 = predict(lda_model10, testing_data)

#confusion matrix
table(lda_pred_y10$class, testing_y)
mean(lda_pred_y10$class != testing_y)

#misclassification rate = 0.375

##################


qda_model5 = qda(Direction~Lag2+Lag1*Lag5, data=training_data)
qda_pred_y5 = predict(qda_model5, testing_data)

#confusion matrix
table(qda_pred_y5$class, testing_y)
mean(qda_pred_y5$class != testing_y)

#misclassification rate = 0.4711538

qda_model6 = qda(Direction~Lag2+Lag2*Lag5, data=training_data)
qda_pred_y6 = predict(qda_model6, testing_data)

#confusion matrix
table(qda_pred_y6$class, testing_y)
mean(qda_pred_y6$class != testing_y)

#misclassification rate = 0.4711538

qda_model7 = qda(Direction~Lag2+Lag3*Lag4, data=training_data)
qda_pred_y7 = predict(qda_model7, testing_data)

#confusion matrix
table(qda_pred_y7$class, testing_y)
mean(qda_pred_y7$class != testing_y)

#misclassification rate = 0.4903846


qda_model8 = qda(Direction~Lag2+Lag3*Lag5, data=training_data)
qda_pred_y8 = predict(qda_model8, testing_data)

#confusion matrix
table(qda_pred_y8$class, testing_y)
mean(qda_pred_y8$class != testing_y)

#misclassification rate =0.4711538

qda_model9 = qda(Direction~Lag2+I(Lag2^2), data=training_data)
qda_pred_y9 = predict(qda_model9, testing_data)

#confusion matrix
table(qda_pred_y9$class, testing_y)
mean(qda_pred_y9$class != testing_y)

#misclassification rate =0.375


qda_model10 = qda(Direction~Lag2+I(Lag3^2), data=training_data)
qda_pred_y10 = predict(qda_model10, testing_data)

#confusion matrix
table(qda_pred_y10$class, testing_y)
mean(qda_pred_y10$class != testing_y)

#misclassification rate =0.5480769

knn.train.X = cbind(Lag2, Lag1*Lag5)[train,]
knn.test.X = cbind(Lag2, Lag1*Lag5)[test,]
knn.pred1 <- knn(knn.train.X,knn.test.X,train_y,k=1)

#confusion matrix
table(knn.pred1, testing_y)
mean(knn.pred1 != testing_y)

#misclassification rate =0.4519231

knn.pred1 <- knn(knn.train.X,knn.test.X,train_y,k=2)

#confusion matrix
table(knn.pred1,testing_y)
mean(knn.pred1==testing_y)

#misclassification rate =0.5


knn.train.X = cbind(Lag2, Lag2*Lag5)[train,]
knn.test.X = cbind(Lag2, Lag2*Lag5)[test,]
knn.pred2 <- knn(knn.train.X,knn.test.X,train_y,k=1)

#confusion matrix
table(knn.pred2, testing_y)
mean(knn.pred2 != testing_y)

#misclassification rate =0.4903846

knn.pred2 <- knn(knn.train.X,knn.test.X,train_y,k=2)

#confusion matrix
table(knn.pred2,testing_y)
mean(knn.pred2==testing_y)

#misclassification rate =0.5192308

knn.train.X = cbind(Lag2, Lag3*Lag4)[train,]
knn.test.X = cbind(Lag2, Lag3*Lag4)[test,]
knn.pred3 <- knn(knn.train.X,knn.test.X,train_y,k=1)

#confusion matrix
table(knn.pred3, testing_y)
mean(knn.pred3 != testing_y)

#misclassification rate = 0.5961538

knn.pred3 <- knn(knn.train.X,knn.test.X,train_y,k=2)

#confusion matrix
table(knn.pred3,testing_y)
mean(knn.pred3==testing_y)

#misclassification rate =0.4903846

knn.train.X = cbind(Lag2, Lag3*Lag5)[train,]
knn.test.X = cbind(Lag2, Lag3*Lag5)[test,]
knn.pred4 <- knn(knn.train.X,knn.test.X,train_y,k=1)

#confusion matrix
table(knn.pred4, testing_y)
mean(knn.pred4 != testing_y)

#misclassification rate= 0.4134615

knn.pred4 <- knn(knn.train.X,knn.test.X,train_y,k=2)

#confusion matrix
table(knn.pred4,testing_y)
mean(knn.pred4==testing_y)

#misclassification rate =0.5192308

#Interaction--------------------------------------------------------------
knn.train.X = cbind(Lag2,I(Lag2^2))[train,]
knn.test.X = cbind(Lag2,I(Lag2^2))[test,]
knn.pred5 <- knn(knn.train.X,knn.test.X,train_y,k=1)

#confusion matrix
table(knn.pred5, testing_y)
mean(knn.pred5 != testing_y)

#misclassification rate = 0.4903846

knn.pred5 <- knn(knn.train.X,knn.test.X,train_y,k=2)

#confusion matrix
table(knn.pred5,testing_y)
mean(knn.pred5==testing_y)

#misclassification rate = 0.5384615

knn.train.X = cbind(Lag2,I(Lag3^2))[train,]
knn.test.X = cbind(Lag2,I(Lag3^2))[test,]
knn.pred6 <- knn(knn.train.X,knn.test.X,train_y,k=1)

#confusion matrix
table(knn.pred6, testing_y)
mean(knn.pred6 != testing_y)

#misclassification rate =0.4615385

knn.pred6 <- knn(knn.train.X,knn.test.X,train_y,k=2)

#confusion matrix
table(knn.pred6,testing_y)
mean(knn.pred6==testing_y)

#misclassification rate =0.5288462

###############################################

# Conclusion:

#The best technique for classification is LDA or Logistic regression and 
#the best model is :
#  Lag2+Lag3*Lag4
#It has the lowest misclassification rate of 0.365


#2. Perform ROC analysis and present the results for all the classification techniques used for
#the previous question.


roc.curve =function(s,print=FALSE){
  Ps=(logistic_probs2>s)*1
  FP=sum((Ps==1)*(testing_y=="Down"))/sum(testing_y=="Down")
  TP=sum((Ps==1)*(testing_y=="Up"))/sum(testing_y=="Up")
  
  if(print==TRUE){
    print(table(Observed=testing_y,Predicted=Ps))
  }
  vect =c(FP,TP)
  names(vect)=c("FPR","TPR")
  return(vect)
}

threshold = 0.5
roc.curve(threshold,print=TRUE)
ROC.curve=Vectorize(roc.curve)
M.ROC=ROC.curve(seq(0,1,by=.01))
plot(M.ROC[1,],M.ROC[2,],col="green",lwd=2,type="l")


roc.curve =function(s,print=FALSE){
  
  Ps=(lda_pred_y$posterior[,2]>s)*1
  FP=sum((Ps==1)*(testing_y=="Down"))/sum(testing_y=="Down")
  TP=sum((Ps==1)*(testing_y=="Up"))/sum(testing_y=="Up")
  
  if(print==TRUE){
    print(table(Observed=testing_y,Predicted=Ps))
  }
  
  vect =c(FP,TP)
  names(vect)=c("FPR","TPR")
  return(vect)
}

threshold = 0.5
roc.curve(threshold,print=TRUE)
ROC.curve=Vectorize(roc.curve)
M.ROC=ROC.curve(seq(0,1,by=.01))
lines(M.ROC[1,],M.ROC[2,],col="black",lwd=2,type="l")


roc.curve =function(s,print=FALSE){
  
  Ps=(qda_pred_y$posterior[,2]>s)*1
  FP=sum((Ps==1)*(testing_y=="Down"))/sum(testing_y=="Down")
  TP=sum((Ps==1)*(testing_y=="Up"))/sum(testing_y=="Up")
  
  if(print==TRUE){
    print(table(Observed=testing_y,Predicted=Ps))
  }
  vect =c(FP,TP)
  names(vect)=c("FPR","TPR")
  return(vect)
}

threshold = 0.5
roc.curve(threshold,print=TRUE)
ROC.curve=Vectorize(roc.curve)
M.ROC=ROC.curve(seq(0,1,by=.01))
lines(M.ROC[1,],M.ROC[2,],col="red",lwd=2,type="l")


knn_pred=knn(train.x, test.x, train_y, k=1, prob = TRUE)
roc.curve =function(s,print=FALSE){
  
  Ps=(attr(knn_pred, "prob")>s)*1
  FP=sum((Ps==1)*(testing_y=="Down"))/sum(testing_y=="Down")
  TP=sum((Ps==1)*(testing_y=="Up"))/sum(testing_y=="Up")
  
  if(print==TRUE){
    print(table(Observed=testing_y,Predicted=Ps))
  }
  
  vect =c(FP,TP)
  names(vect)=c("FPR","TPR")
  return(vect)
}

threshold = 0.5
roc.curve(threshold,print=TRUE)
ROC.curve=Vectorize(roc.curve)
M.ROC=ROC.curve(seq(0,1,by=.01))
lines(M.ROC[1,],M.ROC[2,],col="blue",lwd=2,type="l")


# Conclusion
# As per the graph of ROC the highet lift we are getting is for Logistic and LDA 


