\name{GUIspatClust}
\alias{GUIspatClust}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ GUI for package clustTool }
\description{
  Graphical user interface (GUI) for package \pkg{clustTool}.
}
\usage{
GUIspatClust()
}
\details{
Cluster results can change dramatically with the choice of the clustering method, the distance measure, and the number of clusters. Moreover, depending on the selected validity measure, a different answers result for the optimal number of clusters. Despite of the changing cluster results, each partition could still be informative and valuable. The results can give an interesting insight into the multivariate data structure even if the validity measure does not suggest the optimum for the chosen cluster number. It is thus desirable to perform cluster analysis in an exploratory context, by changing the cluster parameters and visually inspecting the results. 

For this purpose, this statistical tool has been developed. Data, subsets of the data, coordinates and maps can be selected. Different parameters like the distance measure, the clustering method, the number of clusters, and the validity measure can be chosen. Depending on the selection, the clusters can be presented in maps and plots of the cluster centres are provided. 
}
\value{
  The plots of interest.

  Additionally, informations about the cluster result are printed in the R console.
}
\author{ Matthias Templ }
\seealso{ \code{\link{clust}}, \code{\link{clustPlot}},\code{\link{prepare}} }
\examples{
 GUIspatClust()
}
\keyword{utilities}
