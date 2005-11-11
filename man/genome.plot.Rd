\name{genome.plot}
\alias{genome.plot}
\alias{genome.plot.default}
\alias{genome.plot.arrayCGH}

\title{Display a pan-genomic representation of a normalized arrayCGH}
\description{
}

\usage{
  genome.plot.arrayCGH(arrayCGH, x="PosOrder", y="LogRatio",
chrLim=NULL, col.var=NULL, clim=NULL, cex=NULL, pch=NULL, ...)  
  genome.plot.default(data, pch=NULL, cex=NULL, xlab="", ylab="", ...)} 

\arguments{
  \item{arrayCGH}{an object of type \code{arrayCGH}}
  \item{data}{a data frame with two columns: 'x' and 'y', and optionally
    a column data\$chrLim giving the limits of each chromosome}
  \item{x}{a variable name from \code{arrayCGH\$cloneValues} giving the order position
    of the clones along the genome (defaults to 'PosOrder')}
  \item{y}{a variable name from \code{arrayCGH\$cloneValues} to be plotted along the
    genome (defaults to 'LogRatio')}
  \item{chrLim}{an optional variable name from \code{arrayCGH\$cloneValues}
    giving the limits of each chromosome}
  \item{col.var}{a variable name from \code{arrayCGH\$cloneValues}
    defining the color legend}
  \item{clim}{a numeric vector of length 2: color range limits (used if \code{col.var} is numeric)}
  \item{cex}{a numerical value giving the amount by which plotting text
    and symbols should be scaled relative to the default: see \code{\link{par}}}
  \item{xlab}{a title for the x axis: see \code{\link{title}}}
  \item{ylab}{a title for the y axis: see \code{\link{title}}}
  \item{pch}{either an integer specifying a symbol or a single character
    to be used as the default in plotting points: see \code{\link{par}} }
  \item{...}{further arguments to be passed to \code{plot}}
}

\details{if \code{col.var} is a numeric variable, \code{y} colors are
  proportionnal to \code{col.var} values; if it is a character variable
  or a factor, one color is assigned to each different value of
  \code{col.var}. If \code{col.var} is NULL, colors are proportionnal to
  \code{y} values.}

\author{Pierre Neuvial, \email{manor@curie.fr}.}

\note{People interested in tools for array-CGH analysis can
  visit our web-page: \url{http://bioinfo.curie.fr}.}

\examples{
data(spatial)

## default color code: log-ratios
\dontrun{
genome.plot(edge.norm, chrLim="LimitChr")
}

## color code determined by a qualitative variable: ZoneGNL (DNA copy number code)
edge.norm$cloneValues$ZoneGNL <- as.factor(edge.norm$cloneValues$ZoneGNL)
\dontrun{
genome.plot(edge.norm, col.var="ZoneGNL")
}
## comparing profiles with and without normalization
## aggregate data without normalization (flags)

gradient.nonorm <- norm.arrayCGH(gradient, flag.list=NULL, var="LogRatio",
FUN=median, na.rm=TRUE)
gradient.nonorm <- sort.arrayCGH(gradient.nonorm)

\dontrun{
genome.plot(gradient.nonorm, pch=20, main="Genomic profile without
normalization", chrLim="LimitChr")   
x11()
genome.plot(gradient.norm, pch=20, main="Genomic profile with
normalization", chrLim="LimitChr")   
}
}

\keyword{}
\seealso{\code{\link{flag}}, \code{\link[plot]{report.plot}}}
