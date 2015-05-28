\name{plot.forestFloor}
\alias{plot.forestFloor}
\title{
Metod: plot.forestFloor
}
\description{
Method to plot an object of forestFloor-class. Plot partial feature contributions of the most important variables. Colour gradients can be applied two show possible interactions.
}
\usage{
\method{plot}{forestFloor} (x,
                            plot_seq=NULL, 
                            limitY=TRUE,
                            order_by_importance=TRUE, 
                            cropXaxes=NULL, 
                            crop_limit=4,
                            compute_GOF = FALSE,
                            plot_GOF = NULL,
                            GOF_col = "#33333399",
                            ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
foretFloor-object, also abbrivated ff..
Computed topology of randomForest-model, the output from the forestFloor function \cr
includes also X and Y and importance data
}

\item{plot_seq}{
a numeric vector describing which variables and in what sequence to plot,
ordered by importance as default, order_by_importance = F then by feature/coloumn order of training data.
}

\item{limitY}{
TRUE/FLASE, constrain all Yaxis to same limits to ensure relevance of low importance features is not overinterpreted
}

\item{order_by_importance}{
TRUE / FALSE
should plotting and plot_seq be ordered after importance. Most important feature plot first(TRUE)
}

\item{cropXaxes}{
a vector of indice numbers of which zooming of x.axis should look away from outliers
}

\item{crop_limit}{
a number often between 1.5 and 5, referring limit in std.devs from the mean defining outliers
if limit = 2, above selected plots will zoom to +/- 2 std.dev of the respective features.
}

\item{compute_GOF}{
Booleen TRUE/FALSE. Should the goodness of fit be computed for all plots and written in main titles?
}

\item{plot_GOF}{
Booleen TRUE/FALSE. Should the goodness of fit be plotted as a line?
}

\item{GOF_col}{
Color of plotted GOF line
}


\item{...}{
... other arguments passed to generic plot functions
}

}
\details{
The method plot.forestFloor visualizes partial plots of the most important variables first. Partial plotting is also available in randomForest package. But such plots are single thin lines and do not answer the questions;
Is this partial function(PF) a fair generalization or subject to global or local interactions, which cannot be captured in one_way partial plots? \cr
Do sufficently datapoints describe all areas of the PF, or could some areas of the partial plot have a small observation density? \cr
Each partial plot should be viewed as perspectives of the multidimensional toplogy. With multiple perspectives with matching colour gradients and occasional 3D-plots, a fairly complex high dimensional topology can be understood.\cr
Start inspecting the most important but diffuse partial function and recolour observations by this variables physical value. If systematic colourings appear in other partial plots, these two variables/feature contributions are related. If the colour-gradiant most appear horisontal, the variables are correlated. If the colour pattarns are stacking vertical, these feature contributions is interacting in this region. This mean the random forest topology applies an interaction between these two variables. Taking such 2Way interaction further to 3D can comfirm such interaction(see show3D). Looking back to 2D coloured plots, the user can now recognize the shadow of 3D structure. Infact the colouring reminds of that structure. There is no easy way to plot even higher level interactions. But maybe the user can use 3D and colours to percieve 4D and 5D aswell, as we here can use 2D+colours to depict some higher interactions.

Why not just plot the raw data as is, in various ways? Because even when noise component is low, the plotted 2D shadow of multiple non-linear interactions can look merely random. randomForest provides interpretation of what there is the signal and of individual contributions/components related to each variable. What would otherwise become a messy indistinguishable cloud of interactions and additive effects and noise.\cr
system = signal + noise = additive component + interacting components + noise \cr
This pseudo-equation explains how a high dimensional system can be segrated into smaller parts which in turn can be visulized. The random forest provides an interpretation of what is signal and feature contributions of the individual feature related components. Adittive components can be plotted as is by this function. Interacting components must be plotted in the context of two or more features, see \code{\link{show3d_new}}

The root of computation of the partial interpretations, is that treesbased models, recursively process the data univariately - one dicision by one variable at the time. This leads to a multitude of interpretations where randomForest is the average of many trees and feature contributions is the sampled topology of this fit to the data. Secondly random forest is robust and thereby removing a good deal of the noise component. Random trends will across many trees average eachother out, wheres reproducible trends will stay. Furthermore default paremeter-set of randomForest-algorithm is stable and do not need much optimising. Other methods may later supercede in predictions performance. If an underlying system is near linear, GLM will always perform better. But before blindly choosing between e.g. RF, SVM and GLM solely by prediction performance, an idea of a expected system topology should be highly appreciated. For an archtypical 'conservative classical statistician', this package would be funpark trip, only inspirring to what models should tested out to be significant. Could any of the variables be transformed to become linaer? What interactions-terms should be included?

Interpreting partial feature/variable contributions can be an inspirring or alleuring tool. Plausible causality links comes to mind when interactions are mapped. Remember to also concider reverse and external causality or that sampling was not truely indpendent, e.g. Simpson Paradox. Furthermore oberserved interactions can be due to general statistical issues, such as high colinearity, low sample size and non-independant sampling and more. Often some usaul supects can be ruled out as implausible, while others could be similar plausible and only further testing could possibly tell. Remember these topology maps are created by just another oppurtunistic algorithms, only trying to please its loss function. When data is sparse, randomForest tends to yield topologies with few interactions and with steap sigmoid partial functions. This does not mean the underlying system is exactly as such, just that no more complex interpretations was stable and reproducible across the many treees. Furthermore tree models do not extrapolate outside featurespace of trainingset. Any datapoint outside will be predicted as the most resembling datapoints inside the feature space. The amplitude of suggested partialfunctions is lowered in the near boarder of feature space until et becomes a flatline. This make linear effects look more or less sigmoid. This effect is very well displayed with simulated sinfunctions which have large amplitudes in the center of feature space and small gradually dimishing to no amplitude close to the border of feature space. Also normal distribted variables increase this effect of soft border in the feature space. In high dimensions featurespace of uncorrealted variales will have a low density, and if the variables also are normal distributed, the density of datapoints at the borders of features space will be even more sparse. Personally I think that any data-driven model should return to conservative estimates in low density areas of feature space, which is what randomForest in generally do. Nonetheless, as randomForest is not completely robust, sometimes some complex topologies can emerge by chance in a low density areas of feature space. Here it is important to look at the amount data point driving this specifc topology. With a given prior belief is this a plausible and/or probable topology? On th otherhand when firm topologies are found and in areas of many data points, the rule of large numbers must apply, and this should emphasised more closely.
}

\author{
Soren Havelund Welling
}



\examples{
\dontrun{
#simulate data
obs=1000
vars = 6 
X = data.frame(replicate(vars,rnorm(obs)))
Y = with(X, X1^2 + sin(X2*pi) + 2 * X3 * X4 + 0.5 * rnorm(obs))

#grow a forest, remeber to include inbag
rfo=randomForest::randomForest(X,Y,keep.inbag=TRUE)

#compute topology
ff = forestFloor(rfo,X)

#print forestFloor
print(ff) 

#plot partial functions of most important variables first
plot(ff,order_by_importance=TRUE) 

#Non interacting functions are well displayed, whereas X3 and X4 are not
#by applying different colourgradient, interactions reveal themself 
Col=fcol(ff,3,orderByImportance=FALSE)
plot(ff,col=Col) 

#in 3D the interaction between X3 and X reveals itself completely
show3d_new(ff,3:4,col=Col) 

#although no interaction, a joined additive effect of X1 and X2
#can also be informative to display in 3D
Col = fcol(ff,2,X.matrix=FALSE,orderByImportance=FALSE)
plot(ff,col=Col) #use plot first to define colours 
show3d_new(ff,1:2,
           col=Col,
           plot.rgl=list(size=4),
           surf.rgl=list(col=c("red","green")))
 
}
}
\keyword{ multivariate }
\keyword{ models }
\keyword{ nonlinear }
\keyword{ robust }
