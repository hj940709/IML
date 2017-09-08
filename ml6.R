#3a
# randomly generate K centroids
# for each of input vectors, find their nearest centroid
# assign the vector to K cluster
# calculate for the new centroids
lloyd_kmean = function(data_matrix,n_cluster,initial_centroids=NULL){
  closest=function(vector,centroids){
    K=nrow(centroids)
    dis=rep(-1,K)
    for(i in 1:K){
      dis[i]=sum((vector-centroids[i,])^2)
    }
    return(order(dis)[1])
  }
  centroids = initial_centroids
  n_col = ncol(data_matrix)#P demension
  n_row = nrow(data_matrix)#n set of data
  if(is.null(centroids))# randomly generated centroids matrix
    centroids = 
    matrix(runif(n_col*n_cluster,min=min(data_matrix),
                 max = max(data_matrix)),ncol = n_col)
  previous_centroids = centroids
  assign_vector = NULL
  repeat{
    assign_vector=rep(-1,n_row)
    for (i in 1:n_row){
      assign_vector[i]=closest(data_matrix[i,],centroids)
    }
    for (i in 1:n_cluster){
      if (sum(assign_vector==i)==0)
        # centroids[i,] = rep(min(data_matrix),n_col)
        centroids[i,] = runif(n_col,
                      min = min(data_matrix),max = max(data_matrix))
      else if (sum(assign_vector==i)==1){
        centroids[i,] = data_matrix[assign_vector==i,]
      }
      else
        centroids[i,] =
          colMeans(data_matrix[assign_vector==i,])
    }
    if(sum(previous_centroids!=centroids)==0){
      break
    }
    else{
      previous_centroids=centroids
    }
  }
  return(list(centroids=centroids,
              assign_vector=assign_vector))
}
scatter_plot=function(result,data_matrix){
  color=c('red','orange','blue','green','cyan',
          'purple','violet','brown','gray','yellow')
  centroids=result$centroids
  assign_vector=result$assign_vector
  plot(centroids,xlim=c(-3.0,3.0),ylim = c(-3.0,3.0))
  for(i in 1:nrow(data_matrix)){
    points(x=data_matrix[i,1],y=data_matrix[i,2],col=color[assign_vector[i]])
  }
}
library("MASS")
data_matrix=mvrnorm(100,c(0,0),matrix(c(1,0,0,1),2))
scatter_plot(lloyd_kmean(data_matrix,10),data_matrix)

#3b
visualize_digit=function(centroids){
  par(mfrow=c(5,4))
  for (i in 1:nrow(centroids)){
    show_digit(centroids[i,])
  }
  par(mfrow=c(1,1))
}
load_mnist()
amount=500
# amount=5000
dataset=list(x=train$x[1:amount,],y=train$y[1:amount])
visualize_digit(
  lloyd_kmean(dataset$x,10,dataset$x[1:10,])$centroids)
# visualize_digit(
#   lloyd_kmean(dataset$x,10)$centroids)
#3c
init_centroid=function(dataset,n_cluster){
  n_col = ncol(dataset$x)#P demension
  centroids=matrix(rep(-1,n_col*n_cluster),ncol=n_col)
  for(i in 0:9){
    for(j in 1:nrow(dataset$x)){
      if (dataset$y[j]==i){
        centroids[i+1,]=dataset$x[j,]
        break
      }
    }
  }
  return(centroids)
}
visualize_digit(
  lloyd_kmean(dataset$x,10,init_centroid(dataset,10))$centroids)

