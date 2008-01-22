\name{clust}
\alias{clust}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Wrapper function for a variety of clustering algorithms }
\description{
  Performs cluster analysis on data.
}
\usage{
clust(x = Cassini$x, k = 3, method = "kmeansHartigan", seed = set.seed(123), distMethod = "euclidean", qtclustsize = 0.7, iter.max = 100, eps = 0.1, vals = TRUE, alt = NULL, coord = NULL, bic = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ data frame or matrix }
  \item{k}{ Number of clusters }
  \item{method}{ Cluster algorithm }
  \item{seed}{ Seed (can be useful if results from clustering should be reproduced exactly) }
  \item{distMethod}{ Distance Measure }
  \item{qtclustsize}{ Only important if method qtclust is chosen (see \sQuote{qtclust} in package \pkg{flexclust}) }
  \item{iter.max}{ Only important if method kmeans is chosen (see \sQuote{kmeans} in package \pkg{stats}  }
  \item{eps}{ Only important if method \sQuote{dbscan} is chosen }
  \item{vals}{ Validity measures for the resulting clusters would be calculated if this parameter is set to TRUE }
  \item{alt}{ an integer vector for each observation indicating the cluster number for an alternative clustering.
 If provided, the corrected rand
          index for 'clustering' vs. 'alt.clustering' will be computed (see also in package \pkg{fpc}). }
  \item{coord}{ Cluster validity measures will be calculated based on coordinates. }
  \item{bic}{ Alternative way to specify bic values for each cluster. }
}
\details{
  This function acts like a wrapper function for applying a variety of clustering algorithms. The function would be carried out from the \pkg{clustTool}-GUI. To specify additional parameters for special algorithms one should use the algorithm itself and structure the output as the output from this function (as class \sQuote{clust} suggests). 

  Number of Clusters: Since there will be no necessarity for a large number of clusters, the maximum number of clusters should not exeed 12.

  Cluster algorithms: Possible values are: \dQuote{kmeansHartigan}, \dQuote{kmeansLloyd}, \dQuote{kmeansForgy},
 \dQuote{kmeansMacQueen}, \dQuote{cmeans},  \dQuote{cmeansUfcl}, \dQuote{pam}, \dQuote{clara}, \dQuote{fanny}, \dQuote{bclust}, \dQuote{cshell}, \dQuote{Mclust}, \dQuote{kccaKmeans}, \dQuote{kccaKmedians}, \dQuote{kccaAngle}, \dQuote{kccaJaccard}, \dQuote{kccaEjaccard}, \dQuote{cclustKmeans}, \dQuote{cclustHardcl}, \dQuote{cclustNeuralgas}, \dQuote{qtclustKmeans}, \dQuote{qtclustKmedian}, \dQuote{qtclustAngle}, \dQuote{qtclustJaccard}, \dQuote{qtclustEjaccard}, \dQuote{dbscan}, \dQuote{speccPolydot}, \dQuote{fixmahal}, \dQuote{hclustSingle}, \dQuote{hclustComplete}, \dQuote{hclustAverage}, \dQuote{hclustWard}, \dQuote{hclustMcquitty}, \dQuote{hclustMedian}, \dQuote{hclustcentroid}.

  Cluster algorithms which are supported by \pkg{clustTool}-GUI: \dQuote{kmeansHartigan}, \dQuote{clara}, \dQuote{bclust}, \dQuote{Mclust}, \dQuote{kccaKmeans}, \dQuote{speccPolydot}, \dQuote{cclustNeuralgas}, \dQuote{cmeans}, \dQuote{kccaKmedians}. 
 
  For details see the help files listed below.

  distMethod: Possible values are: \dQuote{euclidean}, \dQuote{manhattan}, \dQuote{maximum}, \dQuote{canberra}, \dQuote{cosa}, \dQuote{rf} (dissimilarity measure based on random Forest proximity measure), \dQuote{gower}, \dQuote{bray}, \dQuote{kulczynski}, \dQuote{chord}, \dQuote{morisita}, \dQuote{horn}, \dQuote{mountford}, \dQuote{correlation} (dissimilarity measure based on correlations).

  Distance measures which are supported by \pkg{spatClust}-GUI: \dQuote{euclidean}, \dQuote{manhattan},\dQuote{rf},\dQuote{bray},\dQuote{gower},\dQuote{kulczynski},\dQuote{morisita},\dQuote{correlation}.

  For details see the help files listed below.
}
\value{
  \item{cluster }{A vector of integers indicating the cluster to which each
          point is allocated.}
  \item{centers}{A matrix of cluster centres.}
  \item{size}{The number of points in each cluster.}
  \item{xdata}{The input data.}
  \item{method}{Clustering method}
  \item{distMethod}{Distance measure}
  \item{k}{Number of clusters}
  \item{valTF}{logical, if global validity measures provided}
  \item{valMeasures}{global validity measures}
  \item{silwidths}{local validity measure}
  \item{separation}{local validity measure}
  \item{diameter}{local validity measure}
  \item{average.distance}{local validity measure}
  \item{median.distance}{local validity measure}
  \item{average.toother}{local validity measure}
  \item{vp}{logical, if colnames provided}
}
\author{ Matthias Templ }
\seealso{ 
 Clustering methods:

\code{\link[stats]{kmeans}}, \code{\link[e1071]{cmeans}}, \code{\link[cluster]{pam}}, \code{\link[cluster]{clara}}, \code{\link[cluster]{fanny}}, \code{\link[e1071]{bclust}}, \code{\link[mclust]{Mclust}}, \code{\link[flexclust]{kcca}}, \code{\link[flexclust]{cclust}}, \code{\link[kernlab]{specc}}, \code{\link[stats]{hclust}}

 Distance measures:

\code{\link[stats]{dist}}, \code{\link[vegan]{vegdist}},  \code{\link[mvpart]{g.dist}}, \code{\link[randomForest]{randomForest}}, \dQuote{cosa}, \code{\link[stats]{cor}}

 Cluster validity measures:

\code{\link[fpc]{cluster.stats}}

}
\examples{
 require(mvoutlier)
 data(humus)
 x <- prepare(humus[,c("As", "Ca", "Co", "Mo", "Ni")])
 cl1 <- clust(x, k=9, method="clara", distMethod="manhattan")
 cl1
 names(cl1)
}
\keyword{ cluster }
