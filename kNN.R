Auto.train<-read.csv('D:/2018 first semester/datamining/AutoTrain.csv',header = TRUE)
Auto.test<-read.csv('D:/2018 first semester/datamining/AutoTest.csv',header = TRUE)
AUto.all<-rbind(Auto.train,Auto.test)
##quesion 1##
## STAT318/462 kNN regression function

kNN <- function(k,x.train,y.train,x.pred) {
# 
## This is kNN regression function for problems with
## 1 predictor
#
## INPUTS
#
# k       = number of observations in nieghbourhood 
# x.train = vector of training predictor values
# y.train = vector of training response values
# x.pred  = vector of predictor inputs with unknown
#           response values 
#
## OUTPUT
#
# y.pred  = predicted response values for x.pred

## Initialize:
n.pred <- length(x.pred);		y.pred <- numeric(n.pred)

## Main Loop
for (i in 1:n.pred){
  d <- abs(x.train - x.pred[i])
  dstar = d[order(d)[k]]
  y.pred[i] <- mean(y.train[d <= dstar])		
}
## Return the vector of predictions
invisible(y.pred)
}
y_pred1<-kNN(1,Auto.train$weight,Auto.train$mpg,Auto.test$weight)
y_pred5<-kNN(5,Auto.train$weight,Auto.train$mpg,Auto.test$weight)
y_pred10<-kNN(10,Auto.train$weight,Auto.train$mpg,Auto.test$weight)
y_pred20<-kNN(20,Auto.train$weight,Auto.train$mpg,Auto.test$weight)
y_pred35<-kNN(35,Auto.train$weight,Auto.train$mpg,Auto.test$weight)
y_pred50<-kNN(50,Auto.train$weight,Auto.train$mpg,Auto.test$weight)
y_pred75<-kNN(75,Auto.train$weight,Auto.train$mpg,Auto.test$weight)
y_pred100<-kNN(100,Auto.train$weight,Auto.train$mpg,Auto.test$weight)
MSE1<-mean((y_pred1-Auto.test$mpg)^2)
MSE5<-mean((y_pred5-Auto.test$mpg)^2)
MSE10<-mean((y_pred10-Auto.test$mpg)^2)
MSE20<-mean((y_pred20-Auto.test$mpg)^2)
MSE35<-mean((y_pred35-Auto.test$mpg)^2)
MSE50<-mean((y_pred50-Auto.test$mpg)^2)
MSE75<-mean((y_pred75-Auto.test$mpg)^2)
MSE100<-mean((y_pred100-Auto.test$mpg)^2)
MSE<-c(MSE1,MSE5,MSE10,MSE20,MSE35,MSE50,MSE75,MSE100)
k<-c(1,5,10,20,35,50,75,100)
plot(k,MSE,type='o')
plot(1/k,MSE,type='o')

##question2##

model_train<-loess(mpg~weight,data = Auto.train,control=loess.control(surface="direct"))
loess_test<-predict(model_train,data.frame(weight = Auto.test$weight))
loess_MSE_dataframe<-data.frame((loess_test-Auto.test$mpg)^2)
loess_MSE<-mean(loess_MSE_dataframe[,1])

##question3##
x11(width = 12,height = 12)
plot(AUto.all$weight,AUto.all$mpg,pch=5,cex=0.6)
y_pred35_all<-kNN(35,Auto.train$weight,Auto.train$mpg,AUto.all$weight)
points(AUto.all$weight,y_pred35_all,col='red',cex=0.4)
loess_all<-predict(model_train,data.frame(weight=AUto.all$weight))
points(loess_all~AUto.all$weight,col='blue',pch=20,cex=0.6)
text.legend=c('KNN Model','Loess Model')
legend('topright',pch = c(1,20),legend=text.legend,col=c('red','blue'))
