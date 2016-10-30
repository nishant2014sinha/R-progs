
#3. Textbook Chapter 4 - Question 11
#In this problem, you will develop a model to predict whether a given car gets high or low gas
#mileage based on the Auto data set.

#Loading classes
library(ISLR)
library(MASS)
library(class)
attach(Auto)


#(a) Create a binary variable, mpg01 that contains a 1 if mpg contains a value above its
#median, and a 0 if mpg contains a value below its median. You can compute the median using
#the median( ) function. Note you may find it helpful to use the data.frame( ) function to
#create a single data set containing both mpg01 and the other Auto variables.

#Calculating median and binding the mpg01 column
med_mpg <- median(mpg)
mpg01 <- as.numeric(ifelse(mpg>med_mpg, 1, 0)) 
Auto_New <- cbind(Auto,mpg01)

names(Auto_New) <- c("mpg", "cylinders","displacement", "horsepower", "weight", "acceleration", "year", "origin", "name", "mpg01")

attach(Auto_New)

#(b) Explore the data graphically in order to investigate the association between mpg01 and the
#other features. Which of the other features seem most likely to be useful in predicting mpg01?
#Scatterplots and Boxplots may be useful tools to answer this question. Describe your
#findings

#Boxplots
boxplot(cylinders~mpg01, data = Auto_New, ylab = "cylinder", xlab = "mpg01" )
boxplot(displacement~mpg01, data = Auto_New, ylab = "displacement", xlab = "mpg01" )
boxplot(horsepower~mpg01, data = Auto_New, ylab = "HP", xlab = "mpg01" )
boxplot(acceleration~mpg01, data = Auto_New, ylab = "acceleration", xlab = "mpg01" )
boxplot(mpg~origin, data = Auto_New, ylab = "mpg", xlab = "origin")

#Scatterplots
plot(cylinders~mpg)
plot(displacement~mpg)
plot(horsepower~mpg)
plot(acceleration~mpg)
plot(origin~mpg)


# cylinders, displacement, horsepower, acceleration, origin seems significant and most useful 

#(c) Split the data into a training set and a test set.
#Splitting into training and test data

set.seed(123)
train <- sample(nrow(Auto_New),(nrow(Auto_New)*0.8))
training_auto <- Auto_New[train,]
#Test data sampled from auto_new with rows of train removed
test_auto <- Auto_New[-train,]

#Checking co-orelation
df <- cor(Auto_New[,-9])
setwd("C:/Users/Jumper/Documents/Jumper/Study/SEM3/ISEN 613")
write.csv(df,"zzz.csv")

#The most asccoiated predictors were cylinders, displacement, horsepower, acceleration, origin

#Trying out models
lda_auto <- lda(mpg01~cylinders+displacement+acceleration+horsepower+weight+year+origin, data = training_auto)
lda_auto <- lda(mpg01~cylinders+displacement+horsepower, data = training_auto)

#(d) Perform LDA on the training data in order to predict mpg01 using the variables that
#seemed most associated with mpg01 in (b). What is the test error of the model obtained?
#LDA
lda_auto <- lda(mpg01~cylinders+displacement+acceleration+horsepower+origin, data = training_auto)

lda_auto_pred <- predict(lda_auto, test_auto)
table(lda_auto_pred$class,test_auto$mpg01)
mean(lda_auto_pred$class==test_auto$mpg01)
mean(lda_auto_pred$class!=test_auto$mpg01)

#Test error rate = 0.08860759


#(e) Perform QDA on the training data in order to predict mpg01 using the variables that
#seemed most associated with mpg01 in (b). What is the test error of the model obtained?
#QDA
qda_auto <- qda(mpg01~cylinders+displacement+acceleration+horsepower+origin, data = training_auto)

qda_auto_pred <- predict(qda_auto, test_auto)
table(qda_auto_pred$class,test_auto$mpg01)
mean(qda_auto_pred$class==test_auto$mpg01)
mean(qda_auto_pred$class!=test_auto$mpg01)

#Test error rate = 0.08860759

#(f) Perform logistic regression on the training data in order to predict mpg01 using the
#variables that seemed most associated with mpg01 in (b). What is the test error of the model
#obtained?
#LOGISTIC
log_auto <- glm(mpg01~cylinders+displacement+acceleration+horsepower+origin, data = training_auto, family = "binomial")
log_auto_probs <- predict(log_auto, test_auto, type = "response")
log_auto_pred <- rep("0",nrow(test_auto))
log_auto_pred[log_auto_probs>0.5] = 1
table(log_auto_pred,test_auto$mpg01)
mean(log_auto_pred==test_auto$mpg01)
mean(log_auto_pred!=test_auto$mpg01)

#Test error rate = 0.08860759

#(g) Perform KNN on the training data, with several values of K, in order to predict mpg01.
#Use only the variables that seemed most associated with mpg01 in (b). What test errors do
#you obtain? Which value of K seems to perform the best on this data set?

#KNN
train_auto.X <- cbind(cylinders,displacement,acceleration,horsepower,origin)[train,]
test_auto.X <- cbind(cylinders,displacement,acceleration,horsepower,origin)[-train,]

knn_auto_pred <- knn(train_auto.X,test_auto.X,training_auto$mpg01,k=1)
table(knn_auto_pred,test_auto$mpg01)
mean(knn_auto_pred==test_auto$mpg01)
mean(knn_auto_pred!=test_auto$mpg01)

#Test Error rate =0.05063291

