\name{qscore.summary}
\alias{qscore.summary.arrayCGH}
\alias{qscore.summary}
\title{Compute quality scores for a given arrayCGH object}
\description{
  Compute useful quality scores for the \code{arrayCGH} and display them
  in a convenient way
}
\details{
  This function is used by the function \code{html.report} for
  the generation of an HMTL report of the normalization step.
  It can also be used by itself. 
}

\value{
  A data.frame with 3 columns:
  \item{name}{qscore name}
  \item{label}{qscore label}
  \item{qscore}{quality qscore}
}

\usage{qscore.summary.arrayCGH(arrayCGH, qscore.list)}

\arguments{
  \item{arrayCGH}{an object of type \code{arrayCGH}}
  \item{qscore.list}{a list of objects of type \code{\link{qscore}}}
}

\author{Pierre Neuvial, \email{manor@curie.fr}.}

\note{People interested in tools for array-CGH analysis can
  visit our web-page: \url{http://bioinfo.curie.fr}.}

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

\keyword{misc}
\seealso{\code{\link{qscore}}, \code{\link{qscore.summary}}, \code{\link{html.report}}}
