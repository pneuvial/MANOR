\name{qscore.arrayCGH}
\alias{qscore.arrayCGH}
\title{Compute a quality score for a given arrayCGH}
\description{
}
\details{
}

\value{A numeric value}

\usage{qscore.arrayCGH(qscore, arrayCGH)}

\arguments{
  \item{qscore}{an object of type \code{\link{qscore}}}
  \item{arrayCGH}{an object of type \code{arrayCGH}}
}

\author{Pierre Neuvial, \email{manor@curie.fr}.}

\note{People interested in tools for array-CGH analysis can
  visit our web-page: \url{http://bioinfo.curie.fr}.}

\examples{
data(qscores)
data(spatial)

## compute a quality score for a couple of arrays: signal smoothness
qscore.arrayCGH(smoothness.qscore, edge.norm)
qscore.arrayCGH(smoothness.qscore, gradient.norm)
}

\keyword{}
\seealso{\code{\link{qscore}}, \code{\link{qscore.summary}}}
