\encoding{latin1}
\name{nem}
\alias{nem}
\alias{nem.default}
\alias{nem.arrayCGH}
\title{Spatial Classification by EM algorithm}
\description{
The function \code{nem} computes spatial classification by EM algorithm.}
\usage{
\method{nem}{default}(LogRatio, Col, Row, nk=nk, beta=1, iters=2000, \ldots)
\method{nem}{arrayCGH}(arrayCGH, variable, nk=5, beta=1, iters=2000, \ldots)
}

\arguments{
  \item{LogRatio}{Vector that corresponds to the values to be classified.}
  \item{Col}{Vector of columns coordinates.}
  \item{Row}{Vector of rows coordinates.}
  \item{nk}{Integer value corresponding to the number classes.}
  \item{beta}{Scale parameter for importance of spatial information. }
  \item{iters}{Maximum number of iterations allowed.}
  \item{arrayCGH}{Object of class \code{\link[GLAD]{arrayCGH}}.}
  \item{variable}{Variable that corresponds to the values to be classified.}
  \item{\ldots}{\ldots}
}

\value{
 Either a data frame with the following added elements:
  \item{ZoneNem}{Vector of label zones.}

  or an object of class \code{\link[GLAD]{arrayCGH}} with the following
  elements added to the data.frame attribute \code{arrayValues}:
  \item{ZoneNem}{Vector of label zones.}

}

\references{
\item{ }{C. Ambroise, \emph{Approche probabiliste en classification automatique et contraintes de voisinage}, Ph.D. thesis, Université de Technologie de Compiègne, 1996.}
\item{ }{C. Ambroise, M. Dang, and G. Govaert, \emph{Clustering of
    spatial data by the em algorithm} in Geostatistics for Environmental
  Applications, A. Soares, J. Gomez-Hernandez, and R. Froidevaux, Eds.,
  pp. 493-504. Kluwer Academic Publisher, 1997.}

\item{ }{P. Neuvial, P. Hupé, I. Brito, S. Liva, E. Manié, C. Brennetot,
  A. Aurias, F. Radvanyi, and E. Barillot. \emph{Spatial normalization
    of array-CGH data}. In preparation.
  }
}


\keyword{spatial,models}


\author{Philippe Hupé, \email{manor@curie.fr}.}

\note{People interested in tools for array-CGH analysis can
  visit our web-page: \url{http://bioinfo.curie.fr}.}

\examples{
data(spatial)  ## arrays with local spatial effects

## Plot of LogRatio measured on the array CGH
\dontrun{
arrayPlot(edge,"LogRatio", main="Log2-Ratio measured on the array
CGH", zlim=c(-1,1), bar="v", mediancenter=TRUE) 
}

## Spatial trend of the scaled log-ratios (the variable "ScaledLogRatio"
## equals to the log-ratio minus the median value of the corresponding chromosome arm) 
edgeTrend <- arrayTrend(edge, variable="ScaledLogRatio",
span=0.03, degree=1, iterations=3, family="symmetric")

\dontrun{
arrayPlot(edgeTrend, variable="Trend", main="Spatial trend of the array CGH", bar="v") 
}

## Classification with spatial constraint of the spatial trend
edgeNem <- nem(edgeTrend, variable="Trend")
\dontrun{
arrayPlot(edgeNem, variable="ZoneNem", main="Spatial zones identified by nem", bar="v")
}
}

