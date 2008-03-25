\name{sort.arrayCGH}
\alias{sort.arrayCGH}
\title{Sort for normalized arrayCGH objects}
\description{Sorts clone-level information of a normalized arrayCGH object.}
\usage{
  \method{sort}{arrayCGH}(x, decreasing = FALSE, position.var="Position",
    chromosome.var="Chromosome", \ldots)
}
\arguments{
  \item{x}{an object of type \code{arrayCGH}.}
  \item{decreasing}{(for compatibility with \code{sort} class) currently unused.}
  \item{position.var}{name of position variable.}
  \item{chromosome.var}{name of chromosome variable.}
  \item{\ldots}{further arguments to be passed to \code{sort}.}
}
\author{Pierre Neuvial, \email{manor@curie.fr}.}
\note{People interested in tools for array-CGH analysis can
  visit our web-page: \url{http://bioinfo.curie.fr}.}
\examples{
data(spatial)

## sort a normalized array by clone position
gradient.norm <- sort.arrayCGH(gradient.norm)

report.plot(gradient.norm, main="Genomic profile after normalization")
}
\seealso{\code{\link{norm.arrayCGH}}}
\keyword{}
