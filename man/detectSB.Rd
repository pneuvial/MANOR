\encoding{latin1}
\name{detectSB}
\alias{detectSB}
\alias{detectSB.default}
\alias{detectSB.arrayCGH}
\title{Spatial bias detection}
\description{
This function detects spatial bias on array CGH.
}
\usage{
\method{detectSB}{arrayCGH}(arrayCGH, variable, proportionup=0.25,
proportiondown,type="up", thresholdup=0.2, thresholddown=0.2, \ldots ) 
}

\arguments{
  \item{arrayCGH}{Object of \code{\link[GLAD]{arrayCGH}}.}
  \item{variable}{Variable used to compare the mean of zones detected by
   \code{\link{nem}}}
  \item{proportionup}{Maximal proportion of the array which may be
    affected by spatial bias with high values.}
  \item{proportiondown}{Maximal proportion of the array which may be
    affected by spatial bias with low values.}
  \item{type}{Type of spatial bias detected. Specify either "up" (to
    detect spatial bias with high values), or "down" (to detect spatial
    bias with low values) or "upanddown" (to detect both type of spatial bias).}
  \item{thresholdup}{Threshold used to detect spatial bias with high values.}
  \item{thresholddown}{Threshold used to detect spatial bias with low values.}
  \item{...}{}
}


\details{You must run the \code{\link{arrayTrend}} and \code{\link{nem}} function before detecting
  spatial bias: the \code{\link{arrayTrend}} computes a spatial trend
  and the \code{\link{nem}} function performs a
  classification with spatial constraints defining different zones on
  the array. Based on those results, spatial bias is detected.}

\value{
  An object of class \code{arrayCGH} with the following added
  information in the data.frame attribute \code{arrayValues}:
  \item{SB}{Spots located in zone of spatial bias are coded either by 1
    (if they correspond to a spatial bias with high values) or by -1 (if
    they correspond to a spatial bias with low values). Otherwise they
    are coded by 0.}  
}

\author{Philippe Hupé, \email{Philippe.Hupe@.curie.fr}.}

\note{People interested in tools for array-CGH analysis can
  visit our web-page: \url{http://bioinfo.curie.fr}.}

\seealso{\code{\link{arrayTrend}}, \code{\link{nem}}}

\keyword{spatial,bias,detection}

\examples{
data(spatial)  ## arrays with local spatial effects

## Plot of LogRatio measured on the array CGH
arrayPlot(edge,"LogRatio", main="Log2-Ratio measured on the array
CGH", zlim=c(-1,1), bar="v", mediancenter=TRUE) 

## Spatial trend of the scaled log-ratios (the variable "ScaledLogRatio"
## equals to the log-ratio minus the median value of the corresponding
## chromosome arm)

edgeTrend <- arrayTrend(edge, variable="ScaledLogRatio",
span=0.03, degree=1, iterations=3, family="symmetric")   
arrayPlot(edgeTrend, variable="Trend", main="Spatial trend of the
array CGH", bar="v")  

\dontrun{
## Classification with spatial constraint of the spatial trend
edgeNem <- nem(edgeTrend, variable="Trend")
arrayPlot(edgeNem, variable="ZoneNem", main="Spatial zones identified
by nem", bar="v") 

# Detection of spatial bias
edgeDet <- detectSB(edgeNem, variable="LogRatio", proportionup=0.25,type="up", thresholdup=0.15) 
arrayPlot(edgeDet, variable="SB", main="Zone of spatial bias in red", bar="v")

# CGH profile 
plot(LogRatio ~ PosOrder, data=edgeDet$arrayValues,
col=c("black","red")[as.factor(SB)], pch=20, main="CGH profile: spots
located in spatial bias are in red")  
}
}

\references{
P. Neuvial, P. Hupé, I. Brito, S. Liva, E. Manié, C. Brennetot,
A. Aurias, F. Radvanyi, and E. Barillot. \emph{Spatial normalization of
  array-CGH data}. In preparation.
}

