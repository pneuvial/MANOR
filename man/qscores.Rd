\name{qscores}
\docType{data}
\alias{clone.qscore}
\alias{pct.clone.qscore}
\alias{pct.spot.qscore}
\alias{pct.replicate.qscore}
\alias{smoothness.qscore}
\alias{dynamics.qscore}
\alias{var.replicate.qscore}
\alias{dyn.x.qscore}
\alias{dyn.y.qscore}

\title{Examples of qscore objects (quality scores) to apply to CGH arrays}
\description{
  This data set provides \code{\link{qscore}} objects that can be
  applied to \emph{normalized} \code{arrayCGH} objects in order to
  evaluate data quality after normalization.
}
\usage{data(qscores)}
\format{The following \code{qscore} objects are provided:
  \tabular{lll}{
    \tab \code{clone.qscore} \tab number of clones \cr
    \tab \code{pct.clone.qscore} \tab percentage of clones \cr
    \tab \code{pct.spot.qscore} \tab percentage of spots \cr
    \tab \code{pct.replicate.qscore} \tab average percentage of replicates \cr
    \tab \code{smoothness.qscore} \tab signal smoothness\cr
    \tab \code{var.replicate.qscore} \tab \cr
    \tab \code{dyn.x.qscore} \tab signal dynamics on X chromosome\cr
    \tab \code{dyn.y.qscore} \tab signal dynamics on Y chromosome\cr
  }
}

\source{Institut Curie, \email{manor@curie.fr}.}

\author{Pierre Neuvial, \email{manor@curie.fr}.}

\note{People interested in tools for array-CGH analysis can
  visit our web-page: \url{http://bioinfo.curie.fr}.}

\keyword{datasets}

\examples{
data(qscores)
data(spatial)

## define a list of qscores
qscore.list <- list(clone=clone.qscore, pct.clone=pct.clone.qscore,
pct.spot=pct.spot.qscore, pct.replicate=pct.replicate.qscore,
smoothness=smoothness.qscore, dyn.x=dyn.x.qscore, dyn.y=dyn.y.qscore,
var.replicate=var.replicate.qscore)

## compute quality scores for a couple of normalized arrays
gradient.norm$quality <- qscore.summary.arrayCGH(gradient.norm,
qscore.list)
print(gradient.norm$quality[, 2:3])

qscore.list$dyn.x$args$test <- 23
qscore.list$dyn.y$args$test <- 24
edge.norm$quality <- qscore.summary.arrayCGH(edge.norm, qscore.list)
print(edge.norm$quality[, 2:3])
}

\seealso{\code{\link{spatial}}, \code{\link{qscore.summary.arrayCGH}},
  \code{\link{qscore}}}
