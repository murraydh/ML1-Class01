lots
head(Smarket)
#The cor() function produces a matrix that contains all of the pairwise correlations among the predictors in a data set.
cor(Smarket[,-9]) 
#Use the corrplot function to plot a Correlation Matrix (Need to install.packages("corrplot")
require(corrplot)
corrplot(cor(Smarket[,-9]), mar=c(0,0,1,0))
plot(Smarket$Volume)
#Create a training set of observations from 2001 to 2004 and a test set of observations from 2005 
train<-Smarket$Year<2005
Smarket.2005<-Smarket[!train,]             #Smarket.2005 is a data frame of rows that are not in train - this is the test set 
dim(Smarket.2005)
Direction.2005<-Smarket$Direction[!train]  #Direction.2005 is a factor of "Up" and "Down" from the test data frame (needed for the table() function)
length(Direction.2005)
#So...
# train are the T/'Fs defining the rows of Smarket that compose the training set 
# Smarket.2005 is the test df (includes the Y's)
# Direction.2005 is the test Y's alone

# Linear Discriminant Analysis
#install.packages("MASS")
library(MASS)                              #Contains the lda() function
lda.fit<-lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
lda.fit
 #Observe:
#    The Priors:       These are 0.492 and 0.508 for Down and Up respectively
#    The Group means:  These are the average of each predictor
#                      within each class, and are used by LDA 
#                      as estimates of the mu's for each class
#    The coefficients: These produce the linear discriminants in the LDA decision boundry with p>1 (slide 77). 
#                      If -0.642 × Lag1 - 0.514 × Lag2 is large, then the LDA classifier will predict a market   
#                      increase, and if it is small, then the LDA classifier will predict a market decline.

#The plot() function plots the distributions of the linear discriminants, obtained by computing
# -0.642 × Lag1 - 0.514 × Lag2 for each of the training observations .
plot(lda.fit)
#Use the model to make predictions on the test set
lda.pred<-predict(lda.fit, Smarket.2005)
names(lda.pred)
#The predict() function returns a list with three elements.
##The "class" element contains LDA's predictions about the movement of the market (using 0.5 as the cutoff)
##The "posterior" element is the probability of the prediction in the class element
##The "x" element is a matrix of the linear discriminants described earlier
head(lda.pred$class,20)
#Create a confusion matrix (always put the actuals in the rows abd the predictions in the columns)
mytable<-table(Direction.2005,lda.pred$class)
mytable
#Compute the error rate 2 ways
(mytable["Up","Down"]+mytable["Down","Up"])/sum(mytable)
1-mean(lda.pred$class==Direction.2005)
#Compute the success rate 2 ways
(mytable["Up","Up"]+mytable["Down","Down"])/sum(mytable)
mean(lda.pred$class==Direction.2005)
#Predict a direction for 2 new observations
predict(lda.fit,newdata=data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-.08)),type='response')
#Notice that the LBA model's posteriers are probabilities of the market going Down
# Quadratic Discriminant Analysis

qda.fit<-qda(Direction~Lag1+Lag2,data=Smarket,subset=train)
qda.fit
qda.pred<-predict(qda.fit,Smarket.2005)
table(Direction.2005,qda.pred$class)
mean(qda.pred$class==Direction.2005)

# K-Nearest Neighbors (pp. 164)

#The knn() function requires 4 parameters:
#   1. A matrix containing the predictors associated with the training data, here train.X
#   2. A matrix containing the predictors associated with the data for which we wish to make predictions, here test.X
#   3. A vector containing the class labels for the training observations, here train.Direction
#   4. A value for K, the number of nearest neighbors to be used by the classifier.
require(class)          #Contains the knn() function
train.X<-cbind(Smarket$Lag1,Smarket$Lag2)[train,]
head(train.X)
nrow(train.X)
test.X<-cbind(Smarket$Lag1,Smarket$Lag2)[!train,]
head(test.X)
nrow(test.X)
train.Direction<-Smarket$Direction[train]
head(train.Direction)
length(train.Direction)
set.seed(460)           #Ties in nearest neighbors are broken randomly
#Fit a knn model with k=1
knn.pred<-knn(train.X,test.X,train.Direction,k=1)
mytable<-table(Direction.2005,knn.pred)
mytable
(mytable["Up","Up"]+mytable["Down","Down"])/sum(mytable)
#Since the error rate with k=1 is so high, try with a less flexible model with k=3.
#Fit a knn model with k=3
knn.pred<-knn(train.X,test.X,train.Direction,k=3)
mytable<-table(Direction.2005,knn.pred)
mytable
(mytable["Up","Up"]+mytable["Down","Down"])/sum(mytable)
#Still not great. Looks like QDA with success rate of 0.5992063 beats LDA's 0.5595238 and KNN's 0.5277778
 

