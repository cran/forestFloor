\name{convolute_grid}
\alias{convolute_grid}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
  Model structure grid estimated by feature contributions 
}

\description{
Low-level n-dimensional grid wrapper of \code{\link{kknn}} (not \code{\link{train.kknn}}). Predicts a grid structure on the basis of estimated feature contributions. Is used to draw  one 2D surface in a 3D plot (\code{\link{show3d}}) on basis of feature contributions.
}

\usage{
convolute_grid           (ff,
                          Xi,
                          FCi = NULL,
                          grid = 30,
                          limit = 3,
                          zoom = 3,
                          k.fun=function() round(sqrt(n.obs)/2),
                          userArgs.kknn = alist(kernel="gaussian") )
}
\arguments{
  
  \item{ff}{
    the forestFloor object of class "forestFloor_regression" or "forestFloor_multiClass" at least containing ff$X and ff$FCmatrix with two matrices of equal size
  }
  \item{Xi}{
    the integer vector, of col indices of ff$X to estimate by, often of length 2 or 3. Note total number of predictions is a equal grid^"length of this vector".
  }
  \item{FCi}{
    the integer vector, of col indices of ff$FCmatrix. Those feature contributions to combine(sum) and estimate. If FCi=NULL, will copy Xi vector, which is the trivial choice.
  }
  
  \item{grid}{
  Either, an integer describing the number of grid.lines in each dimension(trivial choice) or, a full defined matrix of any grid position as defined by this function.
  }
  
  \item{limit}{
  a numeric scalar, number of standard deviations away from mean by any dimension to disregard outliers when spanning observations with grid. Set to limit=Inf outliers never should be disregarded.
  }
  
  \item{zoom}{
  numeric scalar, the size of the grid compared to the uni-variate range of data.  If zoom=2 the grid will by any dimension span the double range of the observations. Outliers are disregarded with limit argument.
  
  }
  
\item{k.fun}{
  function to define k-neighbors to consider. n.obs is a constant as number of observations in ff$X. Hereby k neighbors is defined as a function k.fun of n.obs. To set k to a constant use e.g. k.fun = function() 10. k can also be overridden with   userArgs.kknn = alist(kernel="Gaussian",kmax=10).
}
  
  \item{userArgs.kknn}{
    argument list to pass to train.kknn function for each convolution, see \code{\link{kknn}} for possible args. Arguments in this list will have priority of any passed by default by this wrapper function, see argument merger \code{\link{append.overwrite.alists}}
}
  
}
\details{
  This low-level function predicts feature contributions in a grid with \code{\link{train.kknn}} which is k-nearest neighbor + Gaussian weighting. This wrapper is used to construct the transparent grey surface in \code{\link{show3d}}.
}

\value{
  a data frame, 1 + X variable columns.
  First column is the predicted summed feature contributions as a function of the following columns feature   coordinates.
}

\author{
  Soren Havelund Welling
}

\examples{
\dontrun{
## avoid testing of rgl 3D plot on headless non-windows OS
## users can disregard this sentence.
if(!interactive() && Sys.info()["sysname"]!="Windows") skip=TRUE
 
library(rgl)
library(randomForest)
library(forestFloor)

#simulate data
obs=1500
vars = 6 
X = data.frame(replicate(vars,runif(obs)))*2-1
Y = with(X, X1*2 + 2*sin(X2*pi) + 3* (X3+X2)^2 )
Yerror = 1 * rnorm(obs)
var(Y)/var(Y+Yerror)
Y= Y+Yerror

#grow a forest, remember to include inbag
rfo=randomForest::randomForest(X,Y,
                               keep.inbag=TRUE,
                               ntree=1000,
                               replace=TRUE,
                               sampsize=500,
                               importance=TRUE)

#compute ff
ff = forestFloor(rfo,X)

#print forestFloor
print(ff) 

#plot partial functions of most important variables first
Col=fcol(ff,1)
plot(ff,col=Col,orderByImportance=TRUE) 


#the pure feature contributions
rgl::plot3d(ff$X[,2],ff$X[,3],apply(ff$FCmatrix[,2:3],1,sum),
            #add some colour gradients to ease visualization
            #box.outliers squese all observations in a 2 std.dev box
            #univariately for a vector or matrix and normalize to [0;1]
            col=fcol(ff,2,orderByImportance=FALSE))

#add grid convolution/interpolation
#make grid with current function
grid23 = convolute_grid(ff,Xi=2:3,userArgs.kknn= alist(k=25,kernel="gaus"),grid=50,zoom=1.2)
#apply grid on 3d-plot
rgl::persp3d(unique(grid23[,2]),unique(grid23[,3]),grid23[,1],alpha=0.3,
col=c("black","grey"),add=TRUE)
#anchor points of grid could be plotted also
rgl::plot3d(grid23[,2],grid23[,3],grid23[,1],alpha=0.3,col=c("black"),add=TRUE)

## and we se that their is almost no variance out of the surface, thus is FC2 and FC3
## well explained by the feature context of both X3 and X4

### next example show how to plot a 3D grid + feature contribution
## this 4D application is very experimental 

#Make grid of three effects, 25^3 = 15625 anchor points
grid123 = convolute_grid(ff,
                         Xi=c(1:3),
                         FCi=c(1:3),
                         userArgs.kknn = alist(
                           k= 100,
                           kernel = "gaussian",
                           distance = 1),
                         grid=25,
                         zoom=1.2)

#Select a dimension to place in layers
uni2 = unique(grid123[,2])  #2 points to X1 and FC1
uni2=uni2[c(7,9,11,13,14,16,18)] #select some layers to visualize

## plotting any combination of X2 X3 in each layer(from red to green) having different value of X1
count = 0
add=FALSE
for(i in uni2) {
  count = count +1 
  this34.plane = grid123[grid123[,2]==i,]
  if (count==2) add=TRUE 
  
  #  plot3d(ff$X[,1],ff$X[,2]
  persp3d(unique(this34.plane[,3]),
          unique(this34.plane[,4]),
          this34.plane[,1], add=add,
          col=rgb(count/length(uni2),1-count/length(uni2),0),alpha=0.1)
}



## plotting any combination of X1 X3 in each layer(from red to green) having different value of X2
uni3 = unique(grid123[,4])  #2 points to X1 and FC1
uni3=uni3[c(7,9,11,13,14,16,18)] #select some layers to visualize
count = 0
add=FALSE
for(i in uni3) {
  count = count +1 
  this34.plane = grid123[grid123[,4]==i,]
  if (count==2) add=TRUE
  
  
  #plot3d(ff$X[,1],ff$X[,2])
  persp3d(unique(this34.plane[,2]),
          unique(this34.plane[,3]),
          this34.plane[,1], add=add,
          col=rgb(count/length(uni3),1-count/length(uni3),0),alpha=0.1)
  
}
}

}
