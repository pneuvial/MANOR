\name{report.plot}
\alias{report.plot}
\alias{report.plot.arrayCGH}
\alias{report.plot.default}

\title{Display an array image and a genomic representation of a normalized arrayCGH}
\description{
}

\details{This function successively calls \code{\link[GLAD]{arrayPlot}} and \code{\link{genome.plot}}
}


\usage{
\method{report.plot}{arrayCGH}(arrayCGH, x="PosOrder", y=c("LogRatioNorm",
"LogRatio"), chrLim=NULL, layout=TRUE, main=NULL, zlim=NULL, \ldots)
\method{report.plot}{default}(spot.data, clone.data, design, x="PosOrder",
y=c("LogRatioNorm", "LogRatio"), chrLim=NULL, layout=TRUE, main=NULL,
zlim=NULL, \ldots) 
}

\arguments{
  \item{arrayCGH}{an object of type \code{arrayCGH}}
  \item{spot.data}{data.frame with spot-level information to be passed
    to arrayPlot}
  \item{clone.data}{data.frame with clone-level information to be passed
  to genome.plot}
  \item{design}{vector of length 4 with array design: number of blocks
    per column and per row, number of columns and rows per block}
  \item{x}{a variable name from arrayCGH\$cloneValues giving the order position
    of the clones along the genome}
  \item{y}{a vector of one or two variable names to be plotted on the
    array and along the genome. The first one is taken from
    \code{arrayCGH\$arrayValues} and is plotted on the array; the second
    one (or the first one if only one name was provided) is taken from
    \code{arrayCGH\$cloneValues} and is plotted along the genome}
  \item{chrLim}{an optional variable name from arrayCGH\$cloneValues
    giving the limits of each chromosome}
  \item{layout}{if \code{TRUE}, plot layout is set to a 1*2 matrix with
    relative column widths 1 and 4}
  \item{main}{title for the genomic profile}
  \item{zlim}{numeric vector of length 2 to be passed to
    \code{\link[GLAD]{arrayPlot}}: minimum and maximum signal values for
    array image display}
  \item{...}{further arguments to be passed to \code{\link{genome.plot}}}
}

\seealso{\code{\link[plot]{genome.plot}}, \code{\link[GLAD]{arrayPlot}},
  \code{\link{html.report}}}

\author{Pierre Neuvial, \email{manor@curie.fr}.}

\note{People interested in tools for array-CGH analysis can
  visit our web-page: \url{http://bioinfo.curie.fr}.}

\examples{
data(spatial)

### edge: local spatial bias
## aggregate arrayCGH without normalization for comparison with
## normalized array
edge.nonorm <- norm.arrayCGH(edge, flag.list=NULL, FUN=median, na.rm=TRUE) 
edge.nonorm <- sort.arrayCGH(edge.nonorm, position.var="PosOrder")

layout(matrix(c(1,2,4,5,3,3,6,6), 4,2),width=c(1, 4), height=c(6,1,6,1))
report.plot(edge.nonorm, chrLim="LimitChr", layout=FALSE,
main="Pangenomic representation (before normalization)", zlim=c(-1,1),
ylim=c(-3,1))  
report.plot(edge.norm, chrLim="LimitChr", layout=FALSE,
main="Pangenomic representation (after normalization)", zlim=c(-1,1),
ylim=c(-3,1)) 

### gradient: global array Trend
## aggregate arrayCGH without normalization for comparison with
## normalized array
gradient.nonorm <- norm.arrayCGH(gradient, flag.list=NULL, FUN=median, na.rm=TRUE) 
gradient.nonorm <- sort.arrayCGH(gradient.nonorm)

layout(matrix(c(1,2,4,5,3,3,6,6), 4,2),width=c(1, 4), height=c(6,1,6,1))
report.plot(gradient.nonorm, chrLim="LimitChr", layout=FALSE,
main="Pangenomic representation (before normalization)", zlim=c(-2,2),
ylim=c(-3,2)) 
report.plot(gradient.norm, chrLim="LimitChr", layout=FALSE,
main="Pangenomic representation (after normalization)", zlim=c(-2,2),
ylim=c(-3,2)) 
}

\keyword{}
