\name{prepare}
\alias{prepare}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function for tranformation and standardisation }
\description{
  This function can used for transformation and standardisation of the data.
}
\usage{
prepare(x, scaling = "classical", transformation = "logarithm", powers = "none")
}
\arguments{
  \item{x}{ data frame or matrix }
  \item{scaling}{ Scaling of the data. 

                  Possible values are: \dQuote{classical}, \dQuote{robust}, \dQuote{none} }
  \item{transformation}{ Transformation of the data. 

                  Possible values are: \dQuote{logarithm}, \dQuote{boxcox}, \dQuote{bcOpt}, \dQuote{none} }
  \item{powers}{ Powers for Box-Cox transformation for each variable (if \dQuote{boxcox} is chosen) }
}
\details{

  \bold{Transformation}:

  \dQuote{logarithm} replaces the values of x with the natural logarithm by using function \sQuote{log}.

  \dQuote{boxcox} apply a Box-Cox transformation on each variable. Powers must be specified.      
  
  \dQuote{bcOpt} apply a Box-Cox transformation on each variable. Powers are calculated with function \sQuote{box.cox.powers}.

  \dQuote{none} is also possible.

  Transformation before clustering: Cluster analysis in general does not need normally distributed data. However, it is advisable that heavily skewed data are first transformed to a more symmetric distribution. If a good cluster structure exists for a variable we can expect a distribution which has two or more modes. A transformation to more symmetry will preserve the modes but remove large skewness. 

  \bold{Standardisation}:

  \dQuote{classical} apply a \emph{z}-Transformation on each variable by using function \sQuote{scale}.

  \dQuote{robust} apply a robustified \emph{z}-Transformation by using median and MAD. 

  \dQuote{none} is also possible.

  Standardisation before clustering: Standardisation is needed if the variables show a striking difference in the amount of variablity. 
}
\value{
  Transformed and standardised data.
}
\author{ Matthias Templ }
\seealso{ \code{\link{scale}}, \code{\link[car]{box.cox.powers}} }
\examples{
require(mvoutlier)
data(humus)
x <- humus[,4:40]
xNew <- prepare(x, scaling="classical", transformation="logarithm")
}
\keyword{ manip }
