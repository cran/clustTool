\name{kola.background}
\alias{kola.background}
\docType{data}
\title{ kola background }
\description{
 Coordinates of the kolamap background
}
\usage{data(kola.background)}
\format{
  The format is:
List of 4
 $ boundary:`data.frame':	50 obs. of  2 variables:
  ..$ V1: num [1:50] 388650 388160 386587 384035 383029 ...
  ..$ V2: num [1:50] 7892400 7881248 7847303 7790797 7769214 ...
 $ coast   :`data.frame':	6259 obs. of  2 variables:
  ..$ V1: num [1:6259] 438431 439102 439102 439643 439643 ...
  ..$ V2: num [1:6259] 7895619 7896495 7896495 7895800 7895542 ...
 $ borders :`data.frame':	504 obs. of  2 variables:
  ..$ V1: num [1:504] 417575 417704 418890 420308 422731 ...
  ..$ V2: num [1:504] 7612984 7612984 7613293 7614530 7615972 ...
 $ lakes   :`data.frame':	6003 obs. of  2 variables:
  ..$ V1: num [1:6003] 547972 546915     NA 547972 547172 ...
  ..$ V2: num [1:6003] 7815109 7815599      NA 7815109 7813873 ...
}
\details{
 Is used by map.plot() in package \pkg{mvoutlier} and used by function \sQuote{clustPlot}.
}
\source{
 Kola Project (1993-1998)
}
\references{
 see reference in \pkg{mvoutlier}.
}
\examples{
library(mvoutlier)
data(kola.background)
   example(map.plot)
}
\keyword{datasets}
