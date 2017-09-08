#1a
load_mnist()
show_digit(train$x[5,])
train$y[5]

#1b
install.packages(proxy)
library(proxy)
# euclidean_distances<-function(i=1,j=1){
#   sqrt(sum((train$x[i,]-test$x[j,])^2))
# }
# dis<-matrix(0,5000,1000)
# for(i in 1:5000){
#   for(j in 1:1000){
#     dis[i,j]<-euclidean_distances(i,j)
#   }
# }
# euclidean_distances(1,1)

dis<-dist(train$x[1:5000,],test$x[1:1000,])

save(dis, file = "D:/document/ml/ml2.Rdata")
#1c
load("D:/document/ml/ml2.Rdata")

# nearest_point<-function(test_number){
#   a<-c(1:50)
#   sorted<-sort(dis[,test_number])[1:50]
#   for( i in 1:50){
#     for( j in 1:5000){
#       if(dis[j,test_number]==sorted[i]){
#         a[i]<-j
#         break
#       }
#     }
#   }
#   return(a)
# }

getMode<-function(array){
  count<-rep(0,10)
  for(k in 1:length(array)){
    if(array[k]<0||array[k]>9){
      print("Error")
      return(0)
    }
    count[array[k]+1] = count[array[k]+1]+1
  }
  return(order(count,decreasing = TRUE)[1]-1)
}
# testarray=sample(c(rep(0,15),rep(1,16),rep(2,12),rep(4,13)),
#                  length(c(rep(0,15),rep(1,16),rep(2,12),rep(4,13))))

accuracy<-rep(0,50)
for(test_number in 1:1000){
  #near<-nearest_point(test_number)
  near<-order(dis[,test_number])[1:50]
  for(k in 1:50){
    if (test$y[test_number]== getMode(train$y[near[1:k]])){
      accuracy[k]=accuracy[k]+1
    }
  }
}

save(accuracy, file = "D:/document/ml/ml21.Rdata")

load("D:/document/ml/ml21.Rdata")
plot(c(1:50),accuracy/1000,type = "b",xlab="Classfier",ylab = "Accuracy")
for(k in 1:50){
  if (accuracy[k] == max(accuracy)){
    print(paste0("The number ",k,
                 " classifier has the best accuracy rate:",accuracy[k]/1000))
  }
}

#2a
library("ISLR")
data("Weekly")
summary(Weekly)
cor(Weekly[,-9])
attach (Weekly)
plot(Volume)
lr=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Weekly,family ="binomial" )
summary(lr)$coef[,4]
coef(lr)

#2b
prob<-predict(lr,type ="response")
contrasts(Direction)
pred<-rep("Down",1089)
pred[prob>0.5]<-"Up"
#confusion matrix
table(pred ,Direction)
#overall fraction of correct predictions
(54+557)/1089
mean(pred==Direction)#0.5610562

train=(Year<2008)
Weeklyaf2008<-Weekly[!train,]
dim(Weekly[!train,])
lr=glm(Direction~Lag2,data=Weekly,family ="binomial",subset = train)
prob<-predict(lr,Weeklyaf2008,type ="response")
pred<-rep ("Down ",156)
pred[prob>0.5]<-"Up"
#confusion matrix
table(pred ,Weeklyaf2008$Direction)
#overall fraction of correct predictions
mean(pred==Weeklyaf2008$Direction)#0.5064103

#2c
#LDA
library (MASS)
lda=lda(Direction~Lag2,data=Weekly,subset=train)
pred=predict(lda ,Weeklyaf2008)
names(pred)
#confusion matrix
table(pred$class,Weeklyaf2008$Direction)
#overall fraction of correct predictions
mean(pred$class==Weeklyaf2008$Direction)#0.5448718
#QDA
qda=qda(Direction~Lag2,data=Weekly,subset=train)
pred=predict(qda ,Weeklyaf2008)
names(pred)
#confusion matrix
table(pred$class,Weeklyaf2008$Direction)
#overall fraction of correct predictions
mean(pred$class==Weeklyaf2008$Direction)#0.5384615
#KNN
library(class)
trainKNN=cbind(Lag2[train])
testKNN=cbind(Lag2[!train])
directionKNN=Direction[Year<2008]
set.seed(1)
pred=knn (trainKNN,testKNN, directionKNN,k=1)
#confusion matrix
table(pred,Weeklyaf2008$Direction)
#overall fraction of correct predictions
mean(pred==Weeklyaf2008$Direction)#0.5


