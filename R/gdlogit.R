##逻辑回归
gdlogit<-function(X,Y,beta0=NULL,gam=0.1,epsilon=1e-6)
{
  n=nrow(X);
  p=ncol(X);
  if (is.null(beta0)){beta0=rep(0,p)}
  beta=beta0
  beta1=beta0
  ep=1;
  while (ep>epsilon) {
    beta=beta1
    pr<-as.vector(exp(X%*%beta)/(1+exp(X%*%beta))-Y)
    beta1<-beta-gam*apply(diag(pr)%*%X, 2,mean)
    ep<-sqrt(sum((beta1-beta)^2))
  }
  return(beta1)
}
