\name{flags}
\docType{data}
\alias{amplicon.flag}
\alias{chromosome.flag}
\alias{control.flag}
\alias{flags}
\alias{global.spatial.flag}
\alias{local.spatial.flag}
\alias{spatial.flag}
\alias{position.flag}
\alias{rep.flag}
\alias{replicate.flag}
\alias{SNR.flag}
\alias{ref.snr.flag}
\alias{dapi.snr.flag}
\alias{spot.flag}
\alias{unique.flag}
\alias{val.mark.flag}
\alias{intensity.flag}

\title{Examples of flag objects to apply to CGH arrays}
\description{
  This data set provides \code{\link{flag}} objects that can be applied to \code{arrayCGH}
  objects in order to normalize them.
}
\usage{data(flags)}
\format{These \code{flag} objects typically take part to a normalization process:
  \tabular{lll}{
    \tab \code{amplicon.flag} \tab flags spots with high log-ratios (temp flag) \cr
    \tab \code{chromosome.flag} \tab flags spots located on sexual
    chromosomes (named "X" and "Y") \cr
    \tab \code{control.flag} \tab flag control spots \cr
    \tab \code{global.spatial.flag} \tab corrects arrayCGH from global
    spatial trend on the array\cr
    \tab \code{local.spatial.flag} \tab flags spots belonging to local spatial bias zones on the array \cr
    \tab \code{num.chromosome.flag} \tab flags spots located on sexual
    chromosomes (named 23 and 24)\cr
    \tab \code{position.flag} \tab flag spots with no available genome
    position \cr
    \tab \code{replicate.flag} \tab flag spots with poor within-clone-replicate consitency \cr
    \tab \code{ref.snr.flag} \tab flags spots with low signal to noise
    ratio for reference \cr
    \tab \code{dapi.snr.flag} \tab flags spots with low signal to noise
    ratio for DAPI \cr
    \tab \code{SNR.flag} \tab flags spots with low signal to noise ratio \cr
    \tab \code{spot.corr.flag} \tab flags spots with low correlation
    coefficient after image analysis \cr
    \tab \code{spot.flag} \tab flags spots excluded by the image
    analysis software \cr
    \tab \code{unique.flag} \tab exclude last non-flagged spot of a clone \cr 
    \tab \code{val.mark.flag} \tab flags spots corresponding to bad quality clones \cr
    \tab \code{intensity.flag} \tab corrects for an intensity effect
    (using loess regression) \cr
  }
}

\source{Institut Curie, \email{manor@curie.fr}.}

\author{Pierre Neuvial, \email{manor@curie.fr}.}

\note{People interested in tools for array-CGH analysis can
  visit our web-page: \url{http://bioinfo.curie.fr}.}

\keyword{datasets}

\examples{
data(flags)

### complete normalization of an arrayCGH object (with spatial gradient):
## Initialize flag$args

flag.list1 <- list(local.spatial=local.spatial.flag,
  global.spatial=global.spatial.flag, spot=spot.flag, SNR=SNR.flag,
  val.mark=val.mark.flag, unique=unique.flag,
  amplicon=amplicon.flag, chromosome=chromosome.flag,
  replicate=replicate.flag)

data(spatial)
\dontrun{gradient.norm <- norm(gradient, flag.list=flag.list1,
var="LogRatio", FUN=median, na.rm=TRUE)}
print(gradient.norm$flags) ## spot-level flag summary (computed by flag.summary)

### complete normalization of an arrayCGH object (with local spatial bias):
## Initialize flag$args

flag.list2 <- list(spatial=local.spatial.flag, spot=spot.corr.flag,
ref.snr=ref.snr.flag, dapi.snr=dapi.snr.flag, rep=rep.flag,
unique=unique.flag) 
flag.list2$spatial$args <- alist(var="ScaledLogRatio", by.var=NULL,
nk=5, prop=0.25, thr=0.15, beta=1, family="symmetric") 
flag.list2$spot$args <- alist(var="SpotFlag")
flag.list2$spot$char <- "O"
flag.list2$spot$label <- "Image analysis"

\dontrun{edge.norm <- norm(edge, flag.list=flag.list2,
var="LogRatio", FUN=median, na.rm=TRUE)} 
print(edge.norm$flags) ## spot-level flag summary (computed by flag.summary)
}

\seealso{\code{\link{spatial}}, \code{\link{norm.arrayCGH}},
  \code{\link{flag}}, \code{\link{flag.summary}}}
