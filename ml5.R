#2a
#The following 3 functions is copied from the code for previous assignment
generate=function(number){
  X_combinations <- as.matrix(expand.grid(0:1,0:2))
  Y <- sample(0:2, size = number, replace = TRUE, prob = c(0.4,0.3,0.3)) # generate class labels 
  X <- matrix(0, nrow = number, ncol = 2)
  
  for(i in 1:number) {
    prob <- switch (Y[i]+1,
                    c(0.2,0.1,0.4,0.2,0.0,0.1),
                    c(0.6,0.1,0.1,0.1,0.1,0.0),
                    c(0.1,0.4,0.3,0.0,0.2,0.0))
    X[i, ] <- X_combinations[sample(1:6, size = 1, replace = TRUE, prob = prob),]
  }
  return(list(X=X,Y=Y))
}
bayes_classfier=function(Y, X, n_class = 3, n_class_x = c(2,3), m = 0) {
  n <- length(Y)
  likelihood <- vector('list', length = n_class)
  prior <- numeric(n_class)
  for(c in 0:(n_class-1)) {
    n_c <- sum(Y == c)
    prior[c+1] <- (n_c + m) / (n + n_class * m)
    for(i in seq_along(n_class_x)) {
      likelihood[[c+1]][[i]] <- numeric(n_class_x[i]) 
      for(j in 0:(n_class_x[i]-1)) {
        n_cj <- sum(Y == c & X[ ,i] == j)
        likelihood[[c+1]][[i]][j+1] <- (n_cj + m) / (n_c + n_class_x[i] * m)    
      }
    }
  }
  ret <- list(prior = prior, likelihood = likelihood, n_class = n_class, n_class_x = n_class_x)
  class(ret) <- 'nb' 
  ret
}
predict.nb=function(nbfit, X, probabilities = FALSE) {
  n <- nrow(X)
  prob <- matrix(0, nrow = n, ncol = nbfit$n_class) 
  for(i in 1:n)                 # compute joint probabilities p(x,y)
    for(c in 1:nbfit$n_class) {
      prob[i,c] <- nbfit$prior[c]      
      for(j in seq_along(nbfit$n_class_x))
        prob[i,c] <- prob[i,c] * nbfit$likelihood[[c]][[j]][X[i,j] + 1]
    }
  prob <- prob / rowSums(prob)      # normalize into conditional probabilities p(y|x) 
  pred <- max.col(prob) - 1        # predict class y as argmax p(y|x)
  if(probabilities) prob else pred             
}
testify_bayes=function(train,test,mode=TRUE){
  loss=0
  prob=predict.nb(bayes_classfier(train$Y,train$X),test$X,TRUE)
  for (i in 1:length(test$Y)){
    if(mode){
      loss=loss-log2(prob[i,test$Y[i]+1])
    }
    else{
      loss=loss+
        (order(prob[i,],decreasing = TRUE)[1]!=test$Y[i])
    }
  }
  return(loss/length(test$Y))
}
getPlotData_bayes=function(size,trainset=NULL){
  queue=rep(0,length(size))
  for (i in 1:length(size)){
    train=NULL
    if (!is.null(trainset)){train=trainset[i]}
    else{train=generate(size[i])}
    queue[i]=testify_bayes(train,test)
    print(paste("Round:",i,"Size:",size[i]))
  }
  return(queue)
}
library(nnet)
testify_logr=function(train,test,mode=TRUE){
  loss=0
  D <- data.frame(Y = train$Y, 
                  X1 = factor(train$X[ ,1]), 
                  X2 = factor(train$X[ ,2]))
  fit=multinom(Y~X1+X2,data=D)
  pred=predict(fit,type="prob",data.frame(Y = test$Y,
                              X1 = factor(test$X[ ,1]),
                              X2 = factor(test$X[ ,2])))
  for (i in 1:length(test$Y)){
    if(mode){
      loss=loss-log2(pred[i,test$Y[i]+1])
    }
    else{
      loss=loss+
        (order(prob[i,],decreasing = TRUE)[1]!=test$Y[i])
    }
  }
  return(loss/length(test$Y))
}
getPlotData_logr=function(size,trainset=NULL){
  queue=rep(0,length(size))
  for (i in 1:length(size)){
    train=NULL
    if (!is.null(trainset)){train=trainset[i]}
    else{train=generate(size[i])}
    queue[i]=testify_logr(train,test)
    print(paste("Round:",i,"Size:",size[i]))
  }
  return(queue)
}
test=generate(10000)
size=c(25,50,100,200,400,800,1600,3200,6400)
data_bayes=getPlotData_bayes(size,NULL)
data_logr=getPlotData_logr(size,NULL)
plot(x = size, log='x', data_bayes, type = 'b', 
     col = 'blue', lwd = 2, pch = 20, xlab = 'n', 
     ylab = 'test loss',ylim=c(1.4,1.6))
lines(size, data_logr, 
      type = 'b', col = 'red', lwd = 2, pch = 20)
#2b
#P{Y=c|X=x}=(P{X=x|Y=c}*P{Y=c})/sum(P{X=x|Y=c'}*P{Y=c'})
#P{X=x|Y=c}=P{X_1=x_1|Y=c}*P{X_2=x_2|Y=c}
# c(0.2,0.1,0.4,0.2,0.0,0.1)
# c(0.6,0.1,0.1,0.1,0.1,0.0)
# c(0.1,0.4,0.3,0.0,0.2,0.0)
pr=function(X){
  p_Y=c(0.4,0.3,0.3)
  X_0=matrix(c(0.2,0.1,0.4,0.2,0.0,0.1),ncol=3)
  X_1=matrix(c(0.6,0.1,0.1,0.1,0.1,0.0),ncol=3)
  X_2=matrix(c(0.1,0.4,0.3,0.0,0.2,0.0),ncol=3)
  s=sum(c(
    p_Y[1]*X_0[X[1]+1,X[2]+1],
    p_Y[2]*X_1[X[1]+1,X[2]+1],
    p_Y[3]*X_2[X[1]+1,X[2]+1]
  ))
  return(c(
    p_Y[1]*X_0[X[1]+1,X[2]+1]/s,
    p_Y[2]*X_1[X[1]+1,X[2]+1]/s,
    p_Y[3]*X_2[X[1]+1,X[2]+1]/s
    ))
}
testify_logr_true=function(test,mode=TRUE){
  loss=0
  for (i in 1:length(test$Y)){
    if (mode){
      loss=loss-log2(pr(test$X[i,])[test$Y[i]+1])
    }
    else{
      loss=loss+
        (order(pr(test$X[i,]),decreasing = TRUE)[1]!=test$Y[i]+1)
    }
  }
  return(loss/length(test$Y))
}
getPlotData_logr_true=function(size,trainset=NULL){
  queue=rep(0,length(size))
  for (i in 1:length(size)){
    queue[i]=testify_logr_true(test,FALSE)
    print(paste("Round:",i,"Size:",size[i]))
  }
  return(queue)
}
data_bayes_true=getPlotData_logr_true(size,NULL)
plot(size, data_bayes_true, lty=2,
      type = 'l', col = 'green', lwd = 2, pch = 20)
#2c
result=matrix(rep(0,6),ncol=3)
for (i in 1:2){
  for (j in 1:3){
    temp=pr(c(i-1,j-1))
    result[i,j]=order(temp,decreasing = TRUE)[1]
    print(temp)
  }
}
print(result)
#2d
testify_logr_interaction=function(train,test){
  loss=0
  D <- data.frame(Y = train$Y, 
                  X1 = factor(train$X[ ,1]), 
                  X2 = factor(train$X[ ,2]))
  fit=multinom(Y~X1*X2,data=D)
  pred=predict(fit,data.frame(Y = test$Y,
                          X1 = factor(test$X[ ,1]),
                          X2 = factor(test$X[ ,2])))
  for (i in 1:length(test$Y)){
    loss=loss+
      (pred[i]!=test$Y[i])
  }
  return(loss/length(test$Y))
}
getPlotData_logr_interaction=function(size,trainset=NULL){
  queue=rep(0,length(size))
  for (i in 1:length(size)){
    train=NULL
    if (!is.null(trainset)){train=trainset[i]}
    else{train=generate(size[i])}
    queue[i]=testify_logr_interaction(train,test)
    print(paste("Round:",i,"Size:",size[i]))
  }
  return(queue)
}
data_logr_interaction=getPlotData_logr_interaction(size,NULL)
lines(size, data_logr_interaction, 
      type = 'b', col = 'brown', lwd = 2, pch = 20)



#3a
#install.packages("e1071")
library("e1071")
set.seed (1)
generate=function(number){
  Y=rep(0,number)
  X=matrix(rep(0,2*number),ncol=2)

  for (i in 1:number){
    Y[i]=sample(c(-1,+1), size=1, replace=TRUE, prob=c(0.5,0.5))
    if (Y[i]==1){
      x=rnorm(2,mean=0,sd=4)
      X[i,1]=x[1]
      X[i,2]=x[2]
    }
    else if (Y[i]==-1){
      x=rnorm(2,mean=0,sd=1)
      X[i,1]=x[1]
      X[i,2]=x[2]
    }
  }
  list=list(X=X,Y=Y)
  return(data.frame(X1=list$X[,1],X2=list$X[,2],
                    X3=list$X[,1]^2,X4=list$X[,2]^2,
                    Y=as.factor(list$Y)))
}
data=generate(200)
#linear
tune.out=tune(svm,Y~X1+X2,data=data,kernel="linear",
              range=
                list(cost=c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out)
svmfit=svm(Y~X1+X2,
           data=data,
           kernel="linear",
           cost=tune.out$best.parameters,
           scale= FALSE)
plot(svmfit, data=data)
#polynomial
tune.out=tune(svm,Y~X1+X2,data=data,
              kernel="polynomial",
              degree=2,
              range=
                list(cost=c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out)
svmfit=svm(Y~X1+X2,
           data=data,degree=2,
           kernel="polynomial",
           cost=tune.out$best.parameters,
           scale= FALSE)
plot(svmfit, data=data)
#radial
tune.out=tune(svm,Y~X1+X2,data=data,
              kernel="radial",
              range=
                list(cost=c(0.001,0.01,0.1,1,5,10,100),
                     gamma=c(0.5,1,2,3,4)))
summary(tune.out)
svmfit=svm(Y~X1+X2,data=data,
           kernel="radial",
           cost=tune.out$best.parameters$cost,
           gamma=tune.out$best.parameters$gamma,
           scale= FALSE)
plot(svmfit, data=data)
#linear
tune.out=tune(svm,Y~X1+X2+X3+X4,data=data,kernel="linear",
              range=
                list(cost=c(0.001,0.01,0.1,1,5,10,100)))
svmfit=svm(Y~X1+X2+X3+X4,
           data=data,
           kernel="linear",
           cost=tune.out$best.parameters,
           scale= FALSE)
ypred=predict (svmfit ,data )
table(predict =ypred , truth= data$Y )
mean(ypred!=data$Y)
#polynomial
tune.out=tune(svm,Y~X1+X2+X3+X4,data=data,
              kernel="polynomial",
              degree=2,
              range=
                list(cost=c(0.001,0.01,0.1,1,5,10,100)))
svmfit=svm(Y~X1+X2+X3+X4,
           data=data,degree=2,
           kernel="polynomial",
           cost=tune.out$best.parameters,
           scale= FALSE)
ypred=predict (svmfit ,data )
table(predict =ypred , truth= data$Y )
mean(ypred!=data$Y)
#radial
tune.out=tune(svm,Y~X1+X2+X3+X4,data=data,
              kernel="radial",
              range=
                list(cost=c(0.001,0.01,0.1,1,5,10,100),
              gamma=c(0.5,1,2,3,4)))
svmfit=svm(Y~X1+X2+X3+X4,data=data,
           kernel="radial",
           cost=tune.out$best.parameters$cost,
           gamma=tune.out$best.parameters$gamma,
           scale= FALSE)
ypred=predict (svmfit ,data )
table(predict =ypred , truth= data$Y )
mean(ypred!=data$Y)

#3b
library(ISLR)
data(OJ)
sample=sample(NROW(OJ),800,FALSE)
train=OJ[sample,]
test=OJ[setdiff(c(1:NROW(OJ)),sample),]
#linear
svmfit=svm(Purchase~.,
           data=train,
           kernel="linear",
           cost=0.01,
           scale= FALSE)
summary(svmfit)
mean(predict (svmfit,train)!=train$Purchase)
mean(predict (svmfit,test)!=test$Purchase)
tune.out=tune(svm,Purchase~.,data=train,kernel="linear",
              range=
                list(cost=c(0.01,0.1,1,5,10)))
svmfit=svm(Purchase~.,
           data=train,
           kernel="linear",
           cost=tune.out$best.parameters,
           scale= FALSE)
mean(predict (svmfit,train)!=train$Purchase)
mean(predict (svmfit,test)!=test$Purchase)
#radial
svmfit=svm(Purchase~.,
           data=train,
           kernel="radial",
           cost=0.01,
           scale= FALSE)
summary(svmfit)
mean(predict (svmfit,train)!=train$Purchase)
mean(predict (svmfit,test)!=test$Purchase)
tune.out=tune(svm,Purchase~.,data=train,kernel="radial",
              range=
                list(cost=c(0.01,0.1,1,5,10)))
svmfit=svm(Purchase~.,
           data=train,
           kernel="radial",
           cost=tune.out$best.parameters,
           scale= FALSE)
mean(predict (svmfit,train)!=train$Purchase)
mean(predict (svmfit,test)!=test$Purchase)
#polynomial
svmfit=svm(Purchase~.,
           data=train,
           kernel="polynomial",degree=2,
           cost=0.01,
           scale= FALSE)
summary(svmfit)
mean(predict (svmfit,train)!=train$Purchase)
mean(predict (svmfit,test)!=test$Purchase)
tune.out=tune(svm,Purchase~.,data=train,kernel="polynomial",
              degree=2,
              range=
                list(cost=c(0.01,0.1,1,5,10)))
svmfit=svm(Purchase~.,
           data=train,
           kernel="linear",degree=2,
           cost=tune.out$best.parameters,
           scale= FALSE)
mean(predict (svmfit,train)!=train$Purchase)
mean(predict (svmfit,test)!=test$Purchase)

