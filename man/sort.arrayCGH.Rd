\name{sort.arrayCGH}
\alias{sort.arrayCGH}

\title{sort clone-level information of a normalized arrayCGH object}
\description{
}

\usage{
  sort.arrayCGH(x, decreasing = FALSE, position.var="Position",
chromosome.var="Chromosome", ...)
}

\arguments{
  \item{x}{an object of type \code{arrayCGH}}
  \item{decreasing}{(for compatibility with \code{sort} class) currently unused}
  \item{position.var}{}
  \item{chromosome.var}{}
  \item{...}{further arguments to be passed to \code{sort}}
}

\details{
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

\keyword{}
\seealso{\code{\link{norm.arrayCGH}}}
