\name{plotKola.background}
\alias{plotKola.background}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ plot kola.background }
\description{
  Function for visualising \sQuote{kola.background}.
}
\usage{
plotKola.background(map = "kola.background", which.map = c(1, 2, 3, 4), 
           map.col = c(5, 1, 3, 4), map.lwd = c(2, 1, 2, 1), add.plot = FALSE)
}
\arguments{
  \item{map}{ default: \sQuote{kola.background} }
  \item{which.map}{ 1: plot project boundary, 
                    2: plot coast line,
                    3: plot country borders,
                    4: plot lakes and rivers}
  \item{map.col}{ Line colors for project boundary, coast line, country borders, lakes and rivers }
  \item{map.lwd}{ Line widths for project boundary, coast line, country borders, lakes and rivers }
  \item{add.plot}{ if FALSE (default) create new plot }
}
\author{ Peter Filzmoser }
\seealso{ \code{\link{kola.background}} }
\examples{
### coming soon...
}
\keyword{ hplot }
