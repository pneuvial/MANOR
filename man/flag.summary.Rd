\name{flag.summary}
\alias{flag.summary}
\alias{flag.summary.arrayCGH}
\alias{flag.summary.default}
\title{Summarize information about flags after array normalization}
\description{
  Compute spot-level information (number of flagged spots, normalization
  parameters), and display it in a convenient way 
}
\details{This function is used by the function \code{html.report} for
  the generation of an HMTL report of the normalization step.
  It can also be used by itself. 
}

\value{
  A data.frame data.frame with 4 columns:
  \item{name}{flag character}
  \item{label}{flag label}
  \item{arg}{first numeric argument of \code{flag\$FUN}}
  \item{count}{number of flagged spots}
}

\usage{
\method{flag.summary}{arrayCGH}(arrayCGH, flag.list, flag.var="Flag", nflab="not flagged", \ldots)
\method{flag.summary}{default}(spot.flags, flag.list, nflab="not flagged", \ldots)
}

\arguments{
  \item{arrayCGH}{an object of type arrayCGH, \emph{after normalization}
    by MANOR}
  \item{flag.list}{a list of flags with \code{flag\$char} corresponding
    to the values of \code{spot.flags}}
  \item{flag.var}{the name of a variable of arrayCGH\$arrayValues
    containing information about flags (defaults to \code{Flag})}
  \item{var}{the name of a variable of arrayCGH\$cloneValues containing
    signal values (defaults to \code{LogRatio})}
  \item{spot.flags}{a character vector containing information about
    flags}
  \item{nflab}{a character vector providing a legend for "not flagged" spots}
  \item{...}{}
}

\author{Pierre Neuvial, \email{manor@curie.fr}.}

\note{People interested in tools for array-CGH analysis can
  visit our web-page: \url{http://bioinfo.curie.fr}.}

\examples{
data(spatial)
data(flags)
flag.list <- list(spatial=local.spatial.flag, spot=spot.corr.flag,
ref.snr=ref.snr.flag, dapi.snr=dapi.snr.flag, rep=rep.flag,
unique=unique.flag) 
flag.list$spatial$args <- alist(var="ScaledLogRatio", by.var=NULL,
nk=5, prop=0.25, thr=0.15, beta=1, family="symmetric") 
flag.list$spot$args <- alist(var="SpotFlag")
flag.list$spot$char <- "O"
flag.list$spot$label <- "Image analysis"

## normalize arrayCGH
\dontrun{edge.norm <- norm(edge, flag.list=flag.list,
var="LogRatio", FUN=median, na.rm=TRUE)} 
fs <- flag.summary(edge.norm, flag.list=flag.list, flag.var="Flag")

print("Flag and normalization parameters summary")
print(fs)
}

\keyword{misc}
\seealso{\code{\link{html.report}}, \code{\link{flag}}}
