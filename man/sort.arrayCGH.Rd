\name{sort.arrayCGH}
\alias{sort.arrayCGH}

\title{sort clone-level information of a normalizaed arrayCGH object}
\description{
}

\usage{
  sort.arrayCGH(arrayCGH, position.var="Position", chromosome.var="Chromosome")
}

\arguments{
  \item{arrayCGH}{an object of type \code{arrayCGH}}
  \item{position.var}{}
  \item{chromosome.var}{}
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
