\name{clustTool-package}
\alias{clustTool-package}
\alias{clustTool}
\docType{package}
\title{
Clustering with spatial information.
}
\description{
Cluster results can change dramatically depending on the choice of the clustering method, the distance measure, and the number of clusters. Moreover, depending on the selected validity measure, there may be different results for the optimal number of clusters. Despite of the changing cluster results, each partition could still be informative and valuable. The results can give an interesting insight into the multivariate data structure even if the validity measure does not suggest the optimum for the chosen cluster number. It is thus desirable to perform cluster analysis in an exploratory context, by changing the cluster parameters and inspecting the results visually.

For this purpose, this statistical tool has been developed. 
Data, subsets of the data, coordinates and maps can be selected. Furhtermore, different parameters like the distance measure, the clustering method, the number of clusters and the validity measure can be selected. Depending on the selection the clusters can be presented on maps. Additionally, plots of the cluster centres are provided.

}
\details{
\tabular{ll}{
Package: \tab clustTool\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2006-08-09\cr
License: \tab GPL 2.0 or newer\cr
}
Try 

GUIspatStat
}
\author{
Matthias Templ

Maintainer: Matthias Templ <templ@statistik.tuwien.ac.at>
}
\keyword{ package }
\seealso{
}
\examples{
library(mvoutlier)
data(kola.background)
data(humus)
x <- prepare(humus[,c("As", "Ca", "Co", "Mo", "Ni")])
cl1 <- clust(x, k=9, method="clara", distMethod="manhattan")
names(cl1)
clustPlot(coord=humus[,2:3], clust=cl1, k=cl1$k, val="median.distance", Map="kola.background")
GUIspatClust()
}
