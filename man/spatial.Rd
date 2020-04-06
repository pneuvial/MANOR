\name{spatial}
\docType{data}
\alias{spatial}
\alias{edge}
\alias{edge.norm}
\alias{edge.txt}
\alias{gradient}
\alias{gradient.norm}
\alias{gradient.gpr}
\title{Examples of  array-CGH data with spatial artifacts}
\description{
  This data set provides an example of array-CGH data with spatial
  artifacts, consisting of including \code{\link[GLAD]{arrayCGH}}
  objects before and after normalization}

\details{
  'edge' presents local spatial bias in the top-right edge corner, and
  'gradient' presents global spatial trend. 'edge' and 'gradient' are
  \code{\link[GLAD]{arrayCGH}} objects before normalization. They have
  been created respectively from spot and gpr files using
  \code{\link{import}}. 'edge.norm' and 'gradient.norm' are the
  corresponding \code{\link[GLAD]{arrayCGH}} objects after normalization
  using \code{\link{norm.arrayCGH}}.

  \code{\link{flag}} objects used for data normalization come from
  \code{\link{flags}} dataset.
}

\usage{data(spatial)}

\format{
  \itemize{
    \item{edge, gradient}{\code{\link[GLAD]{arrayCGH}}
      objects before normalization:
      \tabular{lll}{
	\tab \code{arrayValues} \tab spot-level information \cr
   	\tab \code{arrayDesign} \tab block design of the array \cr
	\tab \code{cloneValues} \tab additionnal clone-level data (chromosome, position) \cr
      }
    }
    \item{edge.norm, gradient.norm}{\code{\link[GLAD]{arrayCGH}} objects after normalization}
  }
}

\source{Institut Curie, \email{manor@curie.fr}.}

\author{Pierre Neuvial, \email{manor@curie.fr}.}

\note{People interested in tools for array-CGH analysis can
  visit our web-page: \url{http://bioinfo.curie.fr}.}

\keyword{datasets}
\seealso{\code{\link{flags}}}

\examples{
data(spatial)

## edge: example of array with local spatial effects

layout(matrix(1:4, 2, 2), height=c(9,1))
GLAD::arrayPlot(edge, "LogRatio", main="Log-ratios before normalization",
zlim=c(-1,1), bar="h", layout=FALSE, mediancenter=TRUE)
GLAD::arrayPlot(edge.norm, "LogRatioNorm", main="Log-ratios after spatial
normalization", zlim=c(-1,1), bar="h", layout=FALSE, mediancenter=TRUE) 

## gradient: example of array with spatial gradient

layout(matrix(1:4, 2, 2), height=c(9,1))
GLAD::arrayPlot(gradient, "LogRatio", main="Log-ratios before normalization",
zlim=c(-2,2), bar="h", layout=FALSE)
GLAD::arrayPlot(gradient.norm, "LogRatioNorm", main="Log-ratios after spatial
normalization", zlim=c(-2,2), bar="h", layout=FALSE)   
} 
