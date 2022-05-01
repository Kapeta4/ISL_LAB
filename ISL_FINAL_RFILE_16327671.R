#-------------ISLR 8.4 Conceptual Problem 5
majority=c(0.1,0.15,0.2,0.2,0.55,0.6,0.6,0.65,0.7,0.75)
table(majority>0.5)
avg=mean(majority)
avg
#----ISLR 8.4 Applied Problem 8a
library(ISLR)
library(tree)
library(MASS)
library(randomForest)
set.seed(42)
train=sample(1:nrow(Carseats),nrow(Carseats)/2)
#----ISLR 8.4 Applied Problem 8e
mse=c()

set.seed(42)

for(i in 3:10){
  carseats.rf=randomForest(Sales~.,data=Carseats,subset=train,mtry=5,importance=T,ntree=100)
  
  tree.pred=predict(carseats.rf,Carseats[-train,])
  mse=rbind(mse,mean((tree.pred-Carseats[-train,'Sales'])^2))
}
plot(3:10,mse,type='b')


library(randomForest)

set.seed(42)
carseats.rf=randomForest(Sales~.,data=Carseats,subset=train,mtry=9,importance=T,ntree=100)

plot(carseats.rf)

varImpPlot(carseats.rf)

IMP=importance(carseats.rf)
IMP
# As you can see from the table above, 
# ShelveLoc is now the most important predictor of MSE (whose absence most increase the training MSE)
# . Furthermore, while each tree obtains a reduced training MSE when only 9 predictors are used for training,
# the test MSE is larger than the bagging strategy.

#-----------ISLR 8.4 Applied Problem 10 a
library(ISLR)
Hitters.unknownSal=is.na(Hitters[,"Salary"])
Hitters=Hitters[!Hitters.unknownSal,]
Hitters[,"Salary"]=log(Hitters[,"Salary"])

summary(Hitters)

#-----------ISLR 8.4 Applied Problem 10 g

library(randomForest)
library(gbm)
Hitters.train=Hitters[1:200,]
Hitters.test=Hitters[-c(1:200),]

train.mse=c()
test.mse=c()

for(shr in seq(0,0.08,0.002)){
  Hitters.gbm=gbm(Salary~.,data=Hitters.train,shrinkage = shr,n.trees = 1000,distribution = 'gaussian')
  
  Hitters.pred=predict(Hitters.gbm,Hitters.train,n.trees = 1000)
  train.mse=rbind(train.mse,mean((Hitters.pred-Hitters.train[,'Salary'])^2))
  
  Hitters.pred=predict(Hitters.gbm,Hitters.test,n.trees = 1000)
  test.mse=rbind(test.mse,mean((Hitters.pred-Hitters.test[,'Salary'])^2))
}

plot(seq(0,0.08,0.002),train.mse,type='l',xlab='shrinkage',xlim = c(0.003,0.07),ylab='MSE')
lines(seq(0,0.08,0.002),test.mse,col='red')
legend(x='top',legend = c('train MSE','test MSE'),col=c('black','red'),lty=1,text.width = 0.005)

tb=c()

Hitters.gbm=gbm(Salary~.,data=Hitters.train,shrinkage = 0.01,n.trees = 1000,distribution = 'gaussian')
Hitters.pred=predict(Hitters.gbm,Hitters.test,n.trees = 1000)
tb=cbind(tb,'Boost'=mean((Hitters.pred-Hitters.test[,'Salary'])^2))

# Ch3 - linear regression
Hitters.lm=lm(Salary~.,Hitters.train)
Hitters.pred=predict(Hitters.lm,Hitters.test)
tb=cbind(tb,'Linear'=mean((Hitters.pred-Hitters.test[,'Salary'])^2))

# Ch6 - ridge regression
require(glmnet)

x = model.matrix(Salary ~ ., data = Hitters.train)
x.test = model.matrix(Salary ~ ., data = Hitters.test)
y = Hitters.train$Salary

Hitters.glm=glmnet(x,y,alpha = 0)
Hitters.pred=predict(Hitters.glm,x.test)
tb=cbind(tb,'Ridge'=mean((Hitters.pred-Hitters.test[,'Salary'])^2))



Hitters.rf=randomForest(Salary~.,data = Hitters.train,mtry=ncol(Hitters.train)-1) 
Hitters.pred=predict(Hitters.rf,Hitters.test)
mean((Hitters.pred-Hitters.test[,'Salary'])^2)


#------------ISLR 9.7 Conceptual Problem 1 ((a)-(b))

x1 <- -10:10
x2 <- 1 + 3 * x1
plot(x1, x2, type = "l", col = "blue")
text(c(0), c(-20), "Greater than 0", col = "blue")
text(c(0), c(20), "Less than 0", col = "blue")
lines(x1, 1 - x1/2, col = "red")
text(c(0), c(-15), "Less than 0", col = "red")
text(c(0), c(15), "Greater than 0", col = "red")


#-------ISLR 9.7 Conceptual Problem 2 ((a)-(d))
plot(NA, NA, type = "n", xlim = c(-4, 2), ylim = c(-1, 5), asp = 1, xlab = "X1", ylab = "X2")
symbols(c(-1), c(2), circles = c(2), add = TRUE, inches = FALSE)

plot(NA, NA, type = "n", xlim = c(-4, 2), ylim = c(-1, 5), asp = 1, xlab = "X1", ylab = "X2")
symbols(c(-1), c(2), circles = c(2), add = TRUE, inches = FALSE)
text(c(-1), c(2), "< 4")
text(c(-4), c(2), "> 4")

plot(c(0, -1, 2, 3), c(0, 1, 2, 8), col = c("blue", "red", "blue", "blue"), 
     type = "p", asp = 1, xlab = "X1", ylab = "X2")
symbols(c(-1), c(2), circles = c(2), add = TRUE, inches = FALSE)


#------------ISLR 9.7 Applied Problem 5 (Complete (a) ~ (i)
set.seed(1)
x1 <- runif(500) - 0.5
x2 <- runif(500) - 0.5
y <- 1 * (x1^2 - x2^2 > 0)

plot(x1, x2, xlab = "X1", ylab = "X2", col = (4 - y), pch = (3 - y))

logit.fit <- glm(y ~ x1 + x2, family = "binomial")
summary(logit.fit)

data <- data.frame(x1 = x1, x2 = x2, y = y)
probs <- predict(logit.fit, data, type = "response")
preds <- rep(0, 500)
preds[probs > 0.47] <- 1
plot(data[preds == 1, ]$x1, data[preds == 1, ]$x2, col = (4 - 1), pch = (3 - 1), xlab = "X1", ylab = "X2")
points(data[preds == 0, ]$x1, data[preds == 0, ]$x2, col = (4 - 0), pch = (3 - 0))


logitnl.fit <- glm(y ~ poly(x1, 2) + poly(x2, 2) + I(x1 * x2), family = "binomial")

summary(logitnl.fit)

probs <- predict(logitnl.fit, data, type = "response")
preds <- rep(0, 500)
preds[probs > 0.47] <- 1
plot(data[preds == 1, ]$x1, data[preds == 1, ]$x2, col = (4 - 1), pch = (3 - 1), xlab = "X1", ylab = "X2")
points(data[preds == 0, ]$x1, data[preds == 0, ]$x2, col = (4 - 0), pch = (3 - 0))

library(e1071)
data$y <- as.factor(data$y)
svm.fit <- svm(y ~ x1 + x2, data, kernel = "linear", cost = 0.01)
preds <- predict(svm.fit, data)
plot(data[preds == 0, ]$x1, data[preds == 0, ]$x2, col = (4 - 0), pch = (3 - 0), xlab = "X1", ylab = "X2")
points(data[preds == 1, ]$x1, data[preds == 1, ]$x2, col = (4 - 1), pch = (3 - 1))

data$y <- as.factor(data$y)
svmnl.fit <- svm(y ~ x1 + x2, data, kernel = "radial", gamma = 1)
preds <- predict(svmnl.fit, data)
plot(data[preds == 0, ]$x1, data[preds == 0, ]$x2, col = (4 - 0), pch = (3 - 0), xlab = "X1", ylab = "X2")
points(data[preds == 1, ]$x1, data[preds == 1, ]$x2, col = (4 - 1), pch = (3 - 1))

#-----------ISLR 9.7 Applied Problem 6 (Complete (a) ~ (d))

set.seed(1)
x.one <- runif(500, 0, 90)
y.one <- runif(500, x.one + 10, 100)
x.one.noise <- runif(50, 20, 80)
y.one.noise <- 5/4 * (x.one.noise - 10) + 0.1

x.zero <- runif(500, 10, 100)
y.zero <- runif(500, 0, x.zero - 10)
x.zero.noise <- runif(50, 20, 80)
y.zero.noise <- 5/4 * (x.zero.noise - 10) - 0.1

class.one <- seq(1, 550)
x <- c(x.one, x.one.noise, x.zero, x.zero.noise)
y <- c(y.one, y.one.noise, y.zero, y.zero.noise)

plot(x[class.one], y[class.one], col = "blue", pch = "+", ylim = c(0, 100))
points(x[-class.one], y[-class.one], col = "red", pch = 4)


set.seed(2)
z <- rep(0, 1100)
z[class.one] <- 1
data <- data.frame(x = x, y = y, z = as.factor(z))
tune.out <- tune(svm, z ~ ., data = data, kernel = "linear", ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100, 1000, 10000)))
summary(tune.out)


data.frame(cost = tune.out$performance$cost, misclass = tune.out$performance$error * 1100)



x.test <- runif(1000, 0, 100)
class.one <- sample(1000, 500)
y.test <- rep(NA, 1000)
# Set y > x for class.one
for (i in class.one) {
  y.test[i] <- runif(1, x.test[i], 100)
}
# set y < x for class.zero
for (i in setdiff(1:1000, class.one)) {
  y.test[i] <- runif(1, 0, x.test[i])
}
plot(x.test[class.one], y.test[class.one], col = "blue", pch = "+")
points(x.test[-class.one], y.test[-class.one], col = "red", pch = 4)


set.seed(3)
z.test <- rep(0, 1000)
z.test[class.one] <- 1
data.test <- data.frame(x = x.test, y = y.test, z = as.factor(z.test))
costs <- c(0.01, 0.1, 1, 5, 10, 100, 1000, 10000)
test.err <- rep(NA, length(costs))
for (i in 1:length(costs)) {
  svm.fit <- svm(z ~ ., data = data, kernel = "linear", cost = costs[i])
  pred <- predict(svm.fit, data.test)
  test.err[i] <- sum(pred != data.test$z)
}
data.frame(cost = costs, misclass = test.err)