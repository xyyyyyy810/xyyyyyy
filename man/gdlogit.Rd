\name{gdlogit}
\alias{gdlogit}
\title{logistic regression}
\usage{
gdlogit()
}
\description{
Gradient descent.
}
\examples{
data(iris)
X1=as.matrix(subset(iris, Species=="setosa")[1:4])
X2=as.matrix(subset(iris, Species=="versicolor")[1:4])
X3=as.matrix(subset(iris, Species=="virginica")[1:4])
X=rbind(X2,X3)
y1=c(rep(0,50),rep(1,50))
Y=as.vector(y1)
Z=as.vector(c(rep(1,100)))
newX<-cbind(Z,X)
gdlogit(newX,Y)
}
