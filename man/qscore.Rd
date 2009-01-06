\name{qscore}
\alias{to.qscore}
\alias{qscore}
\title{Create an object of type qscore}
\description{
  \code{qscore} object is a list which contains a function, a name, and
  optionnally a label and arguments to be passed to the function.
}

\value{
  An object of class \code{qscore}.
}

\usage{
 to.qscore(FUN, name=NULL, args=NULL, label=NULL, dec=3)
}
\arguments{
  \item{FUN}{a R function returning a numeric value, with first argument
    of type \code{\link[GLAD]{arrayCGH}}, and optionally other
    arguments.}
  \item{name}{a short character value for \code{qscore} object identification}
  \item{args}{a list of arguments to be passed to \code{FUN}; defaults
    to NULL (ie \code{\link[GLAD]{arrayCGH}} is the only argument to \code{FUN})}
  \item{label}{a character value for \code{qscore} object labelling}
  \item{dec}{an integer value giving the number of significant digits to
    keep (defaults to 3)}
}

\author{Pierre Neuvial, \email{manor@curie.fr}.}

\note{People interested in tools for array-CGH analysis can
  visit our web-page: \url{http://bioinfo.curie.fr}.}

\keyword{misc}
\seealso{\code{\link{qscore.arrayCGH}}, \code{\link{qscore.summary.arrayCGH}}}
