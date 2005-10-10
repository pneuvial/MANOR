\name{flag.arrayCGH}
\alias{flag.arrayCGH}
\title{Apply a flag to an arrayCGH}
\description{
  Function \code{flag$FUN} is applied to a \code{flag} object for
  normalization
}
\details{
  Optional arguments in \code{flag$args} are passed to
  \code{flag$FUN}
}

\value{
  An object of class \code{arrayCGH}, which corresponds to the return
  value of \code{flag$FUN} if \code{flag$char} is null, and to the
  input \code{arrayCGH} object with spots given by \code{flag$FUN}
  flagged with \code{flag$char}
}


\usage{flag.arrayCGH(flag, arrayCGH)
}

\arguments{
  \item{flag}{an object of type 'flag'}
  \item{arrayCGH}{an object of type \code{\link[GLAD]{arrayCGH}}}
}

\author{Pierre Neuvial, \email{manor@curie.fr}.}

\note{People interested in tools for array-CGH analysis can
  visit our web-page: \url{http://bioinfo.curie.fr}.}

\examples{
data(spatial)
data(flags)

gradient$arrayValues$LogRatioNorm <- gradient$arrayValues$LogRatio
## flag spots with no available position on the genome
gradient <- flag.arrayCGH(position.flag, gradient)

## flag spots corresponding to low poor quality clones
gradient <- flag.arrayCGH(val.mark.flag, gradient)

## flag spots excluded by Genepix pro
gradient <- flag.arrayCGH(spot.flag, gradient)

## flag local spatial bias zones
\dontrun{gradient <- flag.arrayCGH(local.spatial.flag, gradient)}

## correct global spatial bias
gradient <- flag.arrayCGH(global.spatial.flag, gradient)

## flag spots with low signal to noise
gradient <- flag.arrayCGH(SNR.flag, gradient)

## flag spots with extremely high log-ratios
gradient <- flag.arrayCGH(amplicon.flag, gradient)

## flag spots with poor within replicate consistency
gradient <- flag.arrayCGH(replicate.flag, gradient)

## flag spots corresponding to clones for which all other spot
## replicates have already been flagged  
gradient <- flag.arrayCGH(unique.flag, gradient)

summary.factor(gradient$arrayValues$Flag)
}

\keyword{}
\seealso{\code{\link{flag}}, \code{\link{norm.arrayCGH}}}
