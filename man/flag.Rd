\name{flag}
\alias{to.flag}
\alias{flag}
\title{Create an object of type flag}
\description{
  A \code{flag} object is a list which contains essentially a function
  (flag action) and a character, optionally arguments to be passed to the
  function.
  We make the distinction between two different flag types,
  corresponding to two different purposes:
  - \emph{permanent flags} identify poor quality spots or clones and remove them from further
  analysis (eg spots with low signal to noise ratio)
  - \emph{temporary flags} identify spots or clones that have not to be taken into account for
  the computation of a (scaling) normalization coefficient (eg X chromosome in case of sex mismatch)
}
\details{
  If \code{flag$char} is null, \code{flag$FUN} is supposed to return a
  \code{arrayCGH} object; if it is not null, \code{flag$FUN} is supposed
  to return a list of spots to be flagged with \code{flag$char}.
}

\value{
  An object of class \code{flag}.
}

\usage{
 to.flag(FUN, char=NULL, args=NULL, type="perm.flag", label=NULL)
}
\arguments{
  \item{FUN}{a R function to be applied to an \code{\link[GLAD]{arrayCGH}}, and optionally
    other arguments. If char is not NULL, must return a list of spots (lines of
    \code{arrayCGH$arrayValues}) to be flagged out; if \code{char==NULL}, must return
  an object of type \code{\link[GLAD]{arrayCGH}}}
  \item{char}{a character value to identify flagged spots; defaults to NULL}
  \item{args}{a list of further arguments to be passed to \code{FUN}; defaults
    to NULL (ie \code{\link[GLAD]{arrayCGH}} is the only argument to \code{FUN})}
  \item{type}{a character value defaulting to "perm.flag" which makes
    the distinction between permanent flags (type="perm.flag") and
    temporary flags (type="temp.flag")} 
  \item{label}{a character value for flag labelling}
}

\author{Pierre Neuvial, \email{manor@curie.fr}.}

\note{People interested in tools for array-CGH analysis can
  visit our web-page: \url{http://bioinfo.curie.fr}.}

\examples{
### creation of a permanent flag:
## flag spots with low signal to noise ratios
SNR.FUN <- function(arrayCGH, snr.thr)
    which(arrayCGH$arrayValues$F2 < arrayCGH$arrayValues$B2+log(snr.thr, 2))
SNR.char <- "B"
SNR.flag <- to.flag(SNR.FUN, SNR.char, args=alist(snr.thr=3))

### creation of a permanent flag returning an arrayCGH object:
## correct log-ratios for spatial trend

global.spatial.FUN <- function(arrayCGH, var)
  {
    Trend <- arrayTrend(arrayCGH, var, span=0.03, degree=1,
iterations=3, family="symmetric")
    arrayCGH$arrayValues[[var]] <- Trend$arrayValues[[var]]-Trend$arrayValues$Trend
    arrayCGH
  }
global.spatial.flag <- to.flag(global.spatial.FUN, args=alist(var="LogRatio"))

### creation of a temporary flag:
## exclude sexual chromosomes from signal scaling
chromosome.FUN <- function(arrayCGH, var)
  which(!is.na(match(as.character(arrayCGH$arrayValues[[var]]), c("X", "Y"))))
chromosome.char <- "X"
chromosome.flag <- to.flag(chromosome.FUN, chromosome.char, type="temp.flag",
args=alist(var="Chromosome")) 

data(spatial)

SNR.flag$args$snr.thr <- 3                   ## set SNR threshold
gradient <- flag.arrayCGH(SNR.flag, gradient)    ## apply SNR.flag to array CGH

gradient <- flag.arrayCGH(global.spatial.flag, gradient)

gradient <- flag.arrayCGH(chromosome.flag, gradient)

summary.factor(gradient$arrayValues$Flag)   ## permanent flags
summary.factor(gradient$arrayValues$FlagT)  ## temporary flags
}

\keyword{}
\seealso{\code{\link{flag.arrayCGH}}, \code{\link{norm.arrayCGH}}}
