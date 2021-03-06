\name{box.outliers}
\alias{box.outliers}

\title{
Box Outliers
}
\description{
Squeeze all outliers onto standard.dev-limits and/or normalize to [0;1] scale
}
\usage{
box.outliers(x, limit = 1.5, normalize = TRUE)
}
\arguments{
  \item{x}{
numeric vector, matrix, array, data.frame
}
  \item{limit}{
limit(SD,standard deviation) any number deviating more than limit from mean is an outlier 
}

\item{normalize}{
TRUE/FALSE
should output range be normalized to [0;1]?
}


}
\details{
Can be used to squeeze high dimensional data into a box, hence the name box.outliers. Box.outliers is used internally in forestFloor-package to compute colour gradients without assigning unique colours to few outliers. It's a box because the borders uni-variate/non-interacting.
}
\value{
matrix(n x p) of normalized values
}

\author{
Soren Havelund Welling, 2014
}

\seealso{
scale()
}

\examples{

box.outliers = function (x, limit = 1.5)  {
  x = scale(x)
  x[ x > limit] =  limit
  x[-x > limit] = -limit
  x = x - min(x)
  x = x/(limit * 2)
  return(x)
}
n=1000 #some observations
p = 5  #some dimensions
X = data.frame(replicate(p,rnorm(n))) # a dataset
Xboxed =box.outliers(X,limit=1.5) #applying normalization
plot(Xboxed[,1],Xboxed[,2],col="#00000088") #plot output for first two dimensions
}

\keyword{outlier.filtration}
