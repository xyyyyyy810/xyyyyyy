##knn function
knn<-function(x,X,Y,k){
  n=length(Y-1)
  id=rep(0,n)
  for (i in 1:n){
    id[i]=sum((X[i,]-x)^2)}
  cv<-sort(id)[k]
  return(mean(Y[id<=cv]))}
