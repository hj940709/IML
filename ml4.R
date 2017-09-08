#1a
p_hat=0.5
X=c(1,2)
#P(X_1=1,X_2=2|Y=+1)
# p=1/sqrt(2*pi*16)*exp(-(X[1]-0)^2/(2*16))*
# 1/sqrt(2*pi*16)*exp(-(X[2]-0)^2/(2*16))
p=dnorm(X[1],mean=0,sd=4)*dnorm(X[2],mean=0,sd=4)
#P(X_1=1,X_2=2|Y=-1)
# n=1/sqrt(2*pi*1)*exp(-(X[1]-0)^2/(2*1))*
# 1/sqrt(2*pi*1)*exp(-(X[2]-0)^2/(2*1))
n=dnorm(X[1],mean=0,sd=1)*dnorm(X[2],mean=0,sd=1)
#P(Y=+1|X_1=1,X_2=2)
p*p_hat/(p*p_hat+n*(1-p_hat))

#1b
visualize=function(p_hat){
  grid=expand.grid(x1=.25*(-20:20),x2=.25*(-20:20))
  N=rep(0,41*41)
  for (i in 1:1681){
    #P(X=x|Y=+1)
    # p=1/sqrt(2*pi*16)*exp(-(grid$x1[i]-0)^2/(2*16))*
    #   1/sqrt(2*pi*16)*exp(-(grid$x2[i]-0)^2/(2*16))
    p=dnorm(grid$x1[i],mean=0,sd=4)*
      dnorm(grid$x2[i],mean=0,sd=4)
    #P(X=x|Y=-1)
    # n=1/sqrt(2*pi*1)*exp(-(grid$x1[i]-0)^2/(2*1))*
    #   1/sqrt(2*pi*1)*exp(-(grid$x2[i]-0)^2/(2*1))
    n=dnorm(grid$x1[i],mean=0,sd=1)*
      dnorm(grid$x2[i],mean=0,sd=1)
    #P(Y=+1|X=x)
    N[i]=p*p_hat/(p*p_hat+n*(1-p_hat))
    # N[i]=n*(1-p_hat)/(p*p_hat+n*(1-p_hat))
  }
  return(N)
}
P_p=visualize(p_hat)
contour(seq(-5,5,length.out = 41),
        seq(-5,5,length.out = 41),matrix(P_p,41,41))
contour(seq(-5,5,length.out = 41),
        seq(-5,5,length.out = 41),matrix(1-P_p,41,41),
        add = TRUE, col = "blue")
image(seq(-5,5,length.out = 41),
        seq(-5,5,length.out = 41),matrix(P_p,41,41))

#1c
sigma_p=matrix(c(16,0,0,16),2)
sigma_n=matrix(c(1,0,0,1),2)
mu_p=c(0,0)
mu_n=c(0,0)
p=2
N_p=1/((2*pi)^(p/2)*sqrt(det(sigma_p)))*
  exp(-0.5*t(as.matrix(X-mu_p))%*%
        solve(sigma_p)%*%
        as.matrix(X-mu_p))
N_n=1/((2*pi)^(p/2)*sqrt(det(sigma_n)))*
  exp(-0.5*t(as.matrix(X-mu_n))%*%
        solve(sigma_n)%*%
        as.matrix(X-mu_n))
#P(Y=+1|X_1=1,X_2=2)
N_p*p_hat/(N_p*p_hat+N_n*(1-p_hat))

#2a
generate=function(number){
  Y=rep(0,number)
  X=matrix(rep(0,2*number),ncol=2)
  # y_0=0
  # x_00=0
  for (i in 1:number){
    Y[i]=sample(0:2, size=1, replace=TRUE, prob=c(0.4,0.3,0.3))
    if (Y[i]==0){
      x=
        expand.grid(x1=0:1,x2=0:2)[sample(1:6, 1, replace=TRUE, prob=c(0.2,0.1,0.4,0.2,0.0,0.1)),]
      X[i,1]=x$x1
      X[i,2]=x$x2
      # y_0=y_0+1
    }
    else if (Y[i]==1){
      x=
        expand.grid(x1=0:1,x2=0:2)[sample(1:6, 1, replace=TRUE, prob=c(0.6,0.1,0.1,0.1,0.1,0.0)),]
      X[i,1]=x$x1
      X[i,2]=x$x2
    }
    else if (Y[i]==2){
      x=
        expand.grid(x1=0:1,x2=0:2)[sample(1:6, 1, replace=TRUE, prob=c(0.1,0.4,0.3,0.0,0.2,0.0)),]
      X[i,1]=x$x1
      X[i,2]=x$x2
    }
    # if (X[i,1]==0&X[i,2]==0){
    #   x_00=x_00+1
    # }
  }
  # print(paste("Number of X=(0,0):",x_00,
  #             ";Number of Y=0:",y_0))
  return(list(X=X,Y=Y))
}
train=generate(100)

#2b
#P{X_j=x|Y=c}
prob_X_j=function(data,j,x,c,smooth){
  #maximum likelihood:smooth=0
  #Laplace:smooth=1
  #Krichesky-Trofimov:smooth=1/2
  y_count=0.000000000001 #In case 0/0
  x_count=0.000000000001
  for (i in 1:length(data$Y)){
    if (data$Y[i]==c){
      y_count=y_count+1
      if(data$X[i,j]==x){
        x_count=x_count+1
      }
    }
  }
  q=2
  if(j==2){q=3}
  return((x_count+smooth)/(y_count+q*smooth))
}
#P{Y=c}
prob_Y=function(data,c,smooth){
  #maximum likelihood:smooth=0
  #Laplace:smooth=1
  #Krichesky-Trofimov:smooth=1/2
  y_count=0.000000000001 #In case 0/0
  for (i in 1:length(data$Y)){
    if (data$Y[i]==c){
      y_count=y_count+1
    }
  }
  return((y_count+smooth)/(length(data$Y)+3*smooth))
}

#2c
#P{Y=c|X=x}=(P{X=x|Y=c}*P{Y=c})/sum(P{X=x|Y=c'}*P{Y=c'})
#P{X=x|Y=c}=P{X_1=x_1|Y=c}*P{X_2=x_2|Y=c}
#prob_X_j(data,1,x[1],c,smooth)*
#prob_X_j(data,2,x[2],c,smooth)*prob_Y(data,c,smooth)
prob=function(data,x,c,smooth){
  #Optimization by merging the loop
  x1_count=0.000000000001
  x2_count=0.000000000001
  y_count=0.000000000001
  for (i in 1:length(data$Y)){
    if (data$Y[i]==c){
      y_count=y_count+1
      if(data$X[i,1]==x[1]){
        x1_count=x1_count+1
      }
      if(data$X[i,2]==x[2]){
        x2_count=x2_count+1
      }
    }
  }
  return((x1_count+smooth)/(y_count+2*smooth)*
           (x2_count+smooth)/(y_count+3*smooth)*
           (y_count+smooth)/(length(data$Y)+3*smooth)
           )
}
bayes_classfier=function(data,x,smooth){
  class=0
  max=0
  curr=0
  s=sum(c(prob(data,x,0,smooth),prob(data,x,1,smooth),
          prob(data,x,2,smooth)))
  for (c in 0:2){
    curr=prob(data,x,c,smooth)/s
    if (curr>max){
      class=c
      max=curr
    }
  }
  return(class)
}
test=generate(10000)
testify_bayes=function(test,train,smooth){
  error=0
  result=matrix(rep(0,6),ncol=3)
  #Map result with static value to reduce computation
  result[1,1]=bayes_classfier(train,c(0,0),smooth)
  result[2,1]=bayes_classfier(train,c(1,0),smooth)
  result[1,2]=bayes_classfier(train,c(0,1),smooth)
  result[2,2]=bayes_classfier(train,c(1,1),smooth)
  result[1,3]=bayes_classfier(train,c(0,2),smooth)
  result[2,3]=bayes_classfier(train,c(1,2),smooth)
  for (i in 1:length(test$Y)){
    if(result[test$X[i,1]+1,test$X[i,2]+1]!=test$Y[i]){
      error=error+1
    }
  }
  return(error/length(test$Y))
}
#testify_bayes(test,train,0.5)

getPlotData_bayes=function(size,trainset=NULL,smooth){
  queue=rep(0,length(size))
  test=generate(10000)
  for (i in 1:length(size)){
    train=NULL
    if (!is.null(trainset)){train=trainset[i]}
    else{train=generate(size[i])}
    queue[i]=testify_bayes(test,train,smooth)
    print(paste("Round:",i,"Size:",size[i]))
  }
  return(queue)
}
size=c(25,50,100,200,400,800,1600,3200,6400)
trainset=c(generate(size[1]),
           generate(size[2]),
           generate(size[3]),
           generate(size[4]),
           generate(size[5]),
           generate(size[6]),
           generate(size[7]),
           generate(size[8]),
           generate(size[9]))
data_bayes=getPlotData_bayes(size,trainset,1)
plot(data_bayes,xlab="Size",ylab = "Error Rate",
     xaxt="n",type="b")
axis(1,at=1:length(size),label = size)
#2d
library(nnet)
testify_logr=function(train,test){
  error=0
  D <- data.frame(Y = train$Y, 
                  X1 = factor(train$X[ ,1]), 
                  X2 = factor(train$X[ ,2]))
  fit=multinom(Y~X1+X2,data=D)
  pred=predict(fit,data.frame(Y = test$Y,
    X1 = factor(test$X[ ,1]), X2 = factor(test$X[ ,2])))
  for (i in 1:length(test$Y)){
    if(pred[i]!=test$Y[i]){
      error=error+1
    }
  }
  return(error/length(test$Y))
}
getPlotData_logr=function(size,trainset=NULL){
  queue=rep(0,length(size))
  test=generate(10000)
  for (i in 1:length(size)){
    train=NULL
    if (!is.null(trainset)){train=trainset[i]}
    else{train=generate(size[i])}
    queue[i]=testify_logr(test,train)
    print(paste("Round:",i,"Size:",size[i]))
  }
  return(queue)
}
data_logr=getPlotData_logr(size,trainset)
#Need to change to a better plot function
plot(data_logr,xlab="Size",ylab = "Error Rate",
     xaxt="n",type="b")
axis(1,at=1:length(size),label = size)
points(data_bayes,type="b")
