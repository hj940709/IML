#a
library("MASS")
p=2
mu1=c(0,0)
sigma=matrix(c(2,(-0.75*sqrt(6)),(-0.75*sqrt(6)),3),2,2)
data=mvrnorm(200,mu1,sigma)
cov(data)
cor(data)

#b
plot(data)
contour(kde2d(data[,1],data[,2]))
image(kde2d(data[,1],data[,2]))
persp(kde2d(data[,1],data[,2]))

#c
grid=expand.grid(x1=.25*(-20:20),x2=.25*(-20:20))

N1=rep(0,41*41)
for (i in 1:1681){
  N1[i]=1/((2*pi)^(p/2)*sqrt(det(sigma)))*
    exp(-0.5*as.matrix(grid[i,]-mu1)%*%
             solve(sigma)%*%
             t(as.matrix(grid[i,]-mu1)))
    
}

contour(seq(-5,5,length.out = 41),
        seq(-5,5,length.out = 41),matrix(N1,41,41))
image(seq(-5,5,length.out = 41),
      seq(-5,5,length.out = 41),matrix(N1,41,41))
persp(seq(-5,5,length.out = 41),
        seq(-5,5,length.out = 41),matrix(N1,41,41))

#d
mu2=c(2,1)
N2=rep(0,41*41)
for (i in 1:1681){
  N2[i]=exp(-0.5*as.matrix(grid[i,]-mu2)%*%
             solve(sigma)%*%
             t(as.matrix(grid[i,]-mu2)))/
    ((2*pi)^(p/2)*sqrt(det(sigma)))
}
contour(seq(-5,5,length.out = 41),
        seq(-5,5,length.out = 41),matrix(N2,41,41),
        add = TRUE, col = "red")
N=rep(0,41*41)
for (i in 1:1681){
  n1=exp(-0.5*as.matrix(grid[i,]-mu1)%*%
              solve(sigma)%*%
              t(as.matrix(grid[i,]-mu1)))/
    ((2*pi)^(p/2)*sqrt(det(sigma)))
  n2=exp(-0.5*as.matrix(grid[i,]-mu2)%*%
              solve(sigma)%*%
              t(as.matrix(grid[i,]-mu2)))/
    ((2*pi)^(p/2)*sqrt(det(sigma)))
  N[i]=0.5*n1/(0.5*(n1+n2))
}
contour(seq(-5,5,length.out = 41),
        seq(-5,5,length.out = 41),matrix(N,41,41),
        add = TRUE, col = "blue")
image(seq(-5,5,length.out = 41),
        seq(-5,5,length.out = 41),matrix(N,41,41))
