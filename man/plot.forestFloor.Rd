\name{plot.forestFloor}
\alias{plot.forestFloor}
\alias{plot.forestFloor_regression}
\alias{plot.forestFloor_multiClass}
\title{
plot.forestFloor_regression
}
\description{
A method to plot an object of forestFloor-class. Plot partial feature contributions of the most important variables. Colour gradients can be applied to show possible interactions. Fitted function(plot_GOF) describe FC only as a main effect and quantifies 'Goodness Of Fit'.
}
\usage{
\method{plot}{forestFloor_regression} (
  x,
  plot_seq=NULL, 
  plotTest = NULL,
  limitY=TRUE,
  orderByImportance=TRUE, 
  cropXaxes=NULL, 
  crop_limit=4,
  plot_GOF = TRUE,
  GOF_args = list(col="#33333399"),
  speedup_GOF = TRUE,
  ...)
                          
\method{plot}{forestFloor_multiClass} (
  x,
  plot_seq = NULL,
  label.seq = NULL,
  plotTest  = NULL,
  limitY = TRUE,
  col = NULL,
  colLists = NULL,
  orderByImportance = TRUE,
  fig.columns = NULL,
  plot_GOF = TRUE,
  GOF_args = list(),
  speedup_GOF = TRUE,
  jitter_these_cols = NULL,
  jitter.factor = NULL,
  ...)                             
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
forestFloor-object, also abbreviated ff. Basically a list of class="forestFloor" containing feature contributions, features, targets and variable importance.
}

\item{plot_seq}{
a numeric vector describing which variables and in what sequence to plot. Ordered by importance as default. If orderByImportance = F, then by feature/column order of training data.
}

\item{label.seq}{
[only classification] a numeric vector describing which classes and in what sequence to plot. NULL is all classes ordered is in levels in x$Y of forestFloor_mulitClass object x. 
}

\item{plotTest}{
  NULL(plot by test set if available), TRUE(plot by test set), FALSE(plot by train), "andTrain"(plot by both test and train) 
}

\item{fig.columns}{
[only for multiple plotting], how many columns per page.
default(NULL) is 1 for one plot, 2 for 2, 3 for 3, 2 for 4 and 3 for more. 
}
\item{limitY}{
TRUE/FLASE, constrain all Yaxis to same limits to ensure relevance of low importance features is not over interpreted
}
\item{col}{
Either a colur vector with one colour per plotted class label or a list of colour vectors. Each element is a colour vector one class. Colour vectors in list are normally either of length 1 with or of length equal to number of training observations. NULL will choose standard one colour per class.
}
\item{colLists}{
Deprecetated, will be replaced by col input
}
\item{jitter_these_cols}{
vector to apply jitter to x-axis in plots. Will refer to variables. Useful to for categorical variables. Default=NULL is no jitter.
}
\item{jitter.factor}{
value to decide how much jitter to apply. often between .5 and 3
}
   
\item{orderByImportance}{
TRUE / FALSE
should plotting and plot_seq be ordered after importance. Most important feature plot first(TRUE)
}

\item{cropXaxes}{
a vector of indices of which zooming of x.axis should look away from outliers
}

\item{crop_limit}{
a number often between 1.5 and 5, referring limit in sigmas from the mean defining outliers
if limit = 2, above selected plots will zoom to +/- 2 std.dev of the respective features.
}

\item{plot_GOF}{
Boolean TRUE/FALSE. Should the goodness of fit be plotted as a line?
}

\item{GOF_args}{
Graphical arguments fitted lines, see \code{\link{points}} for parameter names.
}

\item{speedup_GOF}{
Should GOF only computed on reasonable sub sample of data set to speedup computation.
GOF estimation leave-one-out-kNN becomes increasingly slow for +1500 samples.
}

\item{...}{
... other arguments passed to \code{\link{par}} or \code{\link{plot}}\cr.
e.g. mar=, mfrow=, is passed to par, and cex= is passed to plot. par() arguments are reset immediately as plot function returns.
}

}
\details{
The method plot.forestFloor visualizes partial plots of the most important variables first. Partial dependence plots are available in the randomForest package. But such plots are single lines(1d-slices) and do not answer the question:
Is this partial function(PF) a fair generalization or subject to global or local interactions. \cr

}

\author{
Soren Havelund Welling
}


\examples{
\dontrun{
  ## avoid testing of rgl 3D plot on headless non-windows OS
  ## users can disregard this sentence.
  if(!interactive() && Sys.info()["sysname"]!="Windows") skipRGL=TRUE
  
  ###
  #1 - Regression example:
  set.seed(1234)
  library(forestFloor)
  library(randomForest)
  
  #simulate data y = x1^2+sin(x2*pi)+x3*x4 + noise
  obs = 5000 #how many observations/samples
  vars = 6   #how many variables/features
  #create 6 normal distr. uncorr. variables
  X = data.frame(replicate(vars,rnorm(obs)))
  #create target by hidden function
  Y = with(X, X1^2 + sin(X2*pi) + 2 * X3 * X4 + 0.5 * rnorm(obs)) 
  
  #grow a forest
  rfo = randomForest(
    X, #features, data.frame or matrix. Recommended to name columns.
    Y, #targets, vector of integers or floats
    keep.inbag = TRUE,  # mandatory,
    importance = TRUE,  # recommended, else ordering by giniImpurity (unstable)
    sampsize = 1500 ,   # optional, reduce tree sizes to compute faster
    ntree = if(interactive()) 1000 else 25 #speedup CRAN testing
  )
  
  #compute forestFloor object, often only 5-10\% time of growing forest
  ff = forestFloor(
    rf.fit = rfo,       # mandatory
    X = X,              # mandatory
    calc_np = FALSE,    # TRUE or FALSE both works, makes no difference
    binary_reg = FALSE  # takes no effect here when rfo$type="regression"
  )
  
  #print forestFloor
  print(ff) #prints a text of what an 'forestFloor_regression' object is
  plot(ff)
  
  #plot partial functions of most important variables first
  plot(ff,                       # forestFloor object
       plot_seq = 1:6,           # optional sequence of features to plot
       orderByImportance=TRUE    # if TRUE index sequence by importance, else by X column  
  )
       
  #Non interacting features are well displayed, whereas X3 and X4 are not
  #by applying color gradient, interactions reveal themself 
  #also a k-nearest neighbor fit is applied to evaluate goodness-of-fit
  Col=fcol(ff,3,orderByImportance=FALSE) #create color gradient see help(fcol)
  plot(ff,col=Col,plot_GOF=TRUE) 
  
  #feature contributions of X3 and X4 are well explained in the context of X3 and X4
  # as GOF R^2>.8
  
  show3d(ff,3:4,col=Col,plot_GOF=TRUE,orderByImportance=FALSE)
  
  #if needed, k-nearest neighbor parameters for goodness-of-fit can be accessed through convolute_ff
  #a new fit will be calculated and saved to forstFloor object as ff$FCfit
  ff = convolute_ff(ff,userArgs.kknn=alist(kernel="epanechnikov",kmax=5))
  plot(ff,col=Col,plot_GOF=TRUE) #this computed fit is now used in any 2D plotting.
  
  
  ###
  #2 - Multi classification example:   (multi is more than two classes)
  set.seed(1234)
  library(forestFloor)
  library(randomForest)
  
  data(iris)
  X = iris[,!names(iris) \%in\% "Species"]
  Y = iris[,"Species"]
  
  rf = randomForest(
    X,Y,               
    keep.forest=TRUE,  # mandatory
    keep.inbag=TRUE,   # mandatory
    samp=20,           # reduce complexity of mapping structure, with same OOB\%-explained
    importance  = TRUE, # recommended, else ordering by giniImpurity (unstable)
    ntree = if(interactive()) 1000 else 25 #speedup CRAN testing
  )
  
  ff = forestFloor(rf,X)
  
  plot(ff,plot_GOF=TRUE,cex=.7,
    col=c("#FF0000A5","#00FF0050","#0000FF35") #one col per plotted class
  )
  
  #...and 3D plot, see show3d
  show3d(ff,1:2,1:2,plot_GOF=TRUE)
  
  #...and simplex plot (only for three class problems)
  plot_simplex3(ff)
  plot_simplex3(ff,zoom.fit = TRUE)
  
  #...and 3d simplex plots (rough look, Z-axis is feature)
  plot_simplex3(ff,fig3d = TRUE)
  
  ###
  #3 - binary regression example
  #classification of two classes can be seen as regression in 0 to 1 scale
  set.seed(1234)
  library(forestFloor)
  library(randomForest)
  data(iris)
  X = iris[-1:-50,!names(iris) \%in\% "Species"] #drop third class virginica
  Y = iris[-1:-50,"Species"]
  Y = droplevels((Y)) #drop unused level virginica
  
  rf = randomForest(
    X,Y,               
    keep.forest=TRUE,  # mandatory
    keep.inbag=TRUE,   # mandatory
    samp=20,           # reduce complexity of mapping structure, with same OOB\%-explained
    importance  = TRUE, # recommended, else giniImpurity
     ntree = if(interactive()) 1000 else 25 #speedup CRAN testing
  )
  
  ff = forestFloor(rf,X,
                   calc_np=TRUE,    #mandatory to recalculate
                   binary_reg=TRUE) #binary regression, scale direction is printed
  Col = fcol(ff,1) #color by most important feature
  plot(ff,col=Col)   #plot features 
  
  #interfacing with rgl::plot3d
  show3d(ff,1:2,col=Col,plot.rgl.args = list(size=2,type="s",alpha=.5)) 
}
}



\keyword{ multivariate }
\keyword{ models }
\keyword{ nonlinear }
\keyword{ robust }
