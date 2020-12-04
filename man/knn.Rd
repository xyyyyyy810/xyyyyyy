\name{knn}
\alias{knn}
\title{knn(x,X,Y,k)}
\usage{
knn()
}
\description{
The first variable means the test sample.
The second variable means independent variable of train set.
The second variable means dependent varibale of train set.
k means the chosen k.
}
\examples{
##use iris set as an  example
#Get the data of each subset
data(iris)
X1=as.matrix(subset(iris, Species == "setosa")[1:4])
X2=as.matrix(subset(iris, Species == "versicolor")[1:4])
X3=as.matrix(subset(iris, Species == "virginica")[1:4])
X=rbind(X1,X2)
y1=c(rep(0,50),rep(1,50))
Y=as.vector(y1)
Z=c(1:100)
Y1<-c(rep(0,49),rep(1,50))
Y2<-c(rep(0,50),rep(1,49))
for (i in 1:50)
{Z[i]=knn(X[i,],X[-i,],Y1,3)
Z[i+50]=knn(X[i+50,],X[-(i+50),],Y2,3)}
Z
}
