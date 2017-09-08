library("ISLR")
data("College")
college =College [,-1]
fix(college)
summary(college)
pairs(college[,1:10])
boxplot(College$Outstate,College$Private)
Elite =rep ("No",nrow(college ))
Elite [college$Top10perc >50]=" Yes"
Elite =as.factor (Elite)
college = data.frame(college ,Elite)
summary(college$Elite)
boxplot(college$Outstate,college$Elite)
hist(college$Apps)
hist(college$Accept)
hist(college$Enroll)
hist(college$Top10perc)
par(mfrow=c(3,3))




x=runif(30,-3,3)
x=sort(x)
y=2+x-0.5*x^2+rnorm(30,0,0.4)
l1<-lm(y ~ poly(x, 1))
l2<-lm(y ~ poly(x, 2))
l3<-lm(y ~ poly(x, 3))
l4<-lm(y ~ poly(x, 4))
l5<-lm(y ~ poly(x, 5))
l6<-lm(y ~ poly(x, 6))
l7<-lm(y ~ poly(x, 7))
l8<-lm(y ~ poly(x, 8))
l9<-lm(y ~ poly(x, 9))
l10<-lm(y ~ poly(x, 10))
plot(x,y)
#a
lines(x,fitted(l1), col='blue')
lines(x,fitted(l2), col='green')
lines(x,fitted(l3), col='red')
lines(x,fitted(l4), col='purple')
lines(x,fitted(l5), col='orange')
lines(x,fitted(l6), col='grey')
lines(x,fitted(l7), col='black')
lines(x,fitted(l8), col='brown')
lines(x,fitted(l9), col='pink')
lines(x,fitted(l10), col='yellow')

trMSE<-c(sum((y-predict(l1))^2)/length(y),
         sum((y-predict(l2))^2)/length(y),
         sum((y-predict(l3))^2)/length(y),
         sum((y-predict(l4))^2)/length(y),
         sum((y-predict(l5))^2)/length(y),
         sum((y-predict(l6))^2)/length(y),
         sum((y-predict(l7))^2)/length(y),
         sum((y-predict(l8))^2)/length(y),
         sum((y-predict(l9))^2)/length(y),
         sum((y-predict(l10))^2)/length(y))
trMSE
#b
xx <- runif(1000,-3,3)
xx=sort(xx)
yy=2+xx-0.5*xx^2+rnorm(1000,0,0.4)
teMSE<-c(
  sum((yy-predict(l1,data.frame(x=xx)))^2)/length(yy),
  sum((yy-predict(l2,data.frame(x=xx)))^2)/length(yy),
  sum((yy-predict(l3,data.frame(x=xx)))^2)/length(yy),
  sum((yy-predict(l4,data.frame(x=xx)))^2)/length(yy),
  sum((yy-predict(l5,data.frame(x=xx)))^2)/length(yy),
  sum((yy-predict(l6,data.frame(x=xx)))^2)/length(yy),
  sum((yy-predict(l7,data.frame(x=xx)))^2)/length(yy),
  sum((yy-predict(l8,data.frame(x=xx)))^2)/length(yy),
  sum((yy-predict(l9,data.frame(x=xx)))^2)/length(yy),
  sum((yy-predict(l10,data.frame(x=xx)))^2)/length(yy)
)


names(teMSE)<-c(1:10)
names(trMSE)<-c(1:10)
barplot(t(cbind(trMSE,teMSE)), 
        beside = TRUE,legend = c("Train", "Test"))

lines(xx, predict(l1, data.frame(x=xx)), col='blue')
lines(xx, predict(l2, data.frame(x=xx)), col='green')
lines(xx, predict(l3, data.frame(x=xx)), col='red')
lines(xx, predict(l4, data.frame(x=xx)), col='purple')
lines(xx, predict(l5, data.frame(x=xx)), col='orange')
lines(xx, predict(l6, data.frame(x=xx)), col='grey')
lines(xx, predict(l7, data.frame(x=xx)), col='black')
lines(xx, predict(l8, data.frame(x=xx)), col='brown')
lines(xx, predict(l9, data.frame(x=xx)), col='pink')
lines(xx, predict(l10, data.frame(x=xx)), col='yellow')


print(paste("K=1: training MSE=",sum((y-predict(l1))^2)/length(y)))
print(paste("K=2: training MSE=",sum((y-predict(l2))^2)/length(y)))
print(paste("K=3: training MSE=",sum((y-predict(l3))^2)/length(y)))
print(paste("K=4: training MSE=",sum((y-predict(l4))^2)/length(y)))
print(paste("K=5: training MSE=",sum((y-predict(l5))^2)/length(y)))
print(paste("K=6: training MSE=",sum((y-predict(l6))^2)/length(y)))
print(paste("K=7: training MSE=",sum((y-predict(l7))^2)/length(y)))
print(paste("K=8: training MSE=",sum((y-predict(l8))^2)/length(y)))
print(paste("K=8: training MSE=",sum((y-predict(l9))^2)/length(y)))
print(paste("K=10: training MSE=",sum((y-predict(l10))^2)/length(y)))

print(paste("K=1: testing MSE=",sum((yy-predict(l1,data.frame(x=xx)))^2)/length(yy)))
print(paste("K=2: testing MSE=",sum((yy-predict(l2,data.frame(x=xx)))^2)/length(yy)))
print(paste("K=3: testing MSE=",sum((yy-predict(l3,data.frame(x=xx)))^2)/length(yy)))
print(paste("K=4: testing MSE=",sum((yy-predict(l4,data.frame(x=xx)))^2)/length(yy)))
print(paste("K=5: testing MSE=",sum((yy-predict(l5,data.frame(x=xx)))^2)/length(yy)))
print(paste("K=6: testing MSE=",sum((yy-predict(l6,data.frame(x=xx)))^2)/length(yy)))
print(paste("K=7: testing MSE=",sum((yy-predict(l7,data.frame(x=xx)))^2)/length(yy)))
print(paste("K=8: testing MSE=",sum((yy-predict(l8,data.frame(x=xx)))^2)/length(yy)))
print(paste("K=8: testing MSE=",sum((yy-predict(l9,data.frame(x=xx)))^2)/length(yy)))
print(paste("K=10: testing MSE=",sum((yy-predict(l10,data.frame(x=xx)))^2)/length(yy)))
#c
r<-sample(30,30,FALSE)
j<-matrix(r,10,3)
mse<-c(1:10)
for (k in 1:10){
  mse[k]=0
  for(i in 1:10){
    yt = subset(y,y!=y[j[i,1]]&y!=y[j[i,2]]&y!=y[j[i,3]]) 
    xt = subset(x,x!=x[j[i,1]]&x!=x[j[i,2]]&x!=x[j[i,3]])
    ls=lm(yt~poly(xt, k))
    mse[k] = mse[k]+ sum((y[j[i,1:3]]-predict(ls,data.frame(xt=x[j[i,1:3]])))^2)/length(yt)
  }
  print(paste("When K=",k,", sum of MSE=",mse[k]))
} 
plot(c(1:10),mse,type="b",xlab="K",ylab="MSE")
for(k in 1:10){
  if (mse[k] == min(mse)){
    print(paste("When K=",k,", MSE gets minimum error, which is",mse[k]))
  }
}
