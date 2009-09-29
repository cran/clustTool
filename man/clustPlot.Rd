\name{clustPlot}
\alias{clustPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Plotting function for package clustTool }
\description{
  The first plot visualises the clusters in a map. The second plot provides the visualisation of the cluster centres centres.
}
\usage{
clustPlot(coord, clust, k, val = "silwidths", which.plot = c(1, 2), Map = "kola.background", texth = 0.75)
}
\arguments{
  \item{coord}{ (x,y)-coordinates of the data }
  \item{clust}{ Object from function \dQuote{clust} }
  \item{k}{ Number of clusters }
  \item{val}{ local validity measure }
  \item{which.plot}{ if a subset of the plots is required, specify a subset of the
          numbers \sQuote{1:2} }
  \item{Map}{A map may be selected}
  \item{texth}{cex of variable names in the cluster centres plot.}
}
\details{

The resulting clusters of function \dQuote{clust} will be visualised in maps. 

In general, not only the location of the single clustes in the map is of interest but also the composition of the single cluster.
For this purpose a plot of the cluster centres is supported which is helpful for the interpretation of the clusters. The cluster centre is the element-wise mean of all observations of a cluster. Therefore, for each cluster all elements used for clustering are presented. The resulting means for all clusters are horizontally arranged. If the variables used for clustering were standardised they have the same contribution for the cluster analysis. If single elements show very high or low means for a cluster they are highly influential for this cluster.
}
\author{ Matthias Templ }
\seealso{ \code{\link{clust}}, \code{\link{GUIspatClust}} }
\examples{
library(mvoutlier)
data(kola.background)
data(humus)
x <- prepare(humus[,c("As", "Ca", "Co", "Mo", "Ni")])
cl1 <- clust(x, k=9, method="clara", distMethod="manhattan")
names(cl1)
clustPlot(coord=humus[,2:3], clust=cl1, k=cl1$k, val="median.distance", 
          Map="kola.background")
}
\keyword{ dplot }
\keyword{ hplot }
