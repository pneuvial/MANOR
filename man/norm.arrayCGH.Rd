\encoding{latin1}
\name{norm.arrayCGH}
\alias{norm.arrayCGH}
\alias{MANOR}
\alias{manor}

\title{Normalize an object of type arrayCGH}
\description{
  Normalize an object of type \code{arrayCGH} using a list of criteria
  specified as (temporary or permanent) flags. If a replicate identifier
  (clone name) is provided, a single target value is computed for all the replicates.
  
  The normalization coefficient is computed as a function, and is applied
  to all good quality spots of the array.
}
\details{
  The two flag types are treated differently :  
   - permanent flags detect poor quality spots, which are removed from further analysis
   - temporary flags detect good quality spots that would bias the normalization
   coefficient if they were not excluded from its computation,
   e.g. amplicons or sexual chromosomes. Thus they are not taken into
   account for the computation of the coefficient, but at the end of the
   analysis they are normalized as any good quality spots of the array. 

  The normalization coefficient is computed as a function (e.g. mean or
  median) of the target value (e.g. log-ratios); it is then applied
  to all good quality spots (including temporary flags),
  i.e. substracted from each target value.

  If clone level information is available (i.e. if
  \code{arrayCGH$cloneValues} is not null), a normalized mean target
  value and basic statistics (such as the number of replicates per
  clone) are calculated for each clone.
}

\usage{
  norm.arrayCGH(arrayCGH, flag.list=NULL, var="LogRatio", printTime=FALSE, FUN=median, ...)
}
\arguments{
  \item{arrayCGH}{an object of type \code{arrayCGH}}
  \item{flag.list}{a list of objects of type flag}
  \item{var}{a variable name (from \code{arrayCGH$arrayValues}) from which
    normalization coefficient has to be computed; default is "LogRatio"}
  \item{printTime}{boolean value; if \code{TRUE}, the time
      taken by each step of the normalization process is displayed}
  \item{FUN}{an aggregation function (e.g. mean, median) to compute a normalization
    coefficient; default is median}
  \item{...}{further arguments to be passed to FUN}
}

\value{an object of type \code{arrayCGH}}

\author{Pierre Neuvial, \email{manor@curie.fr}.}

\note{People interested in tools for array-CGH analysis can
  visit our web-page: \url{http://bioinfo.curie.fr}.}

\examples{
data(spatial)
data(flags)

### 'edge': local spatial bias
## define a list of flags to be applied
flag.list1 <- list(spatial=local.spatial.flag, spot=spot.corr.flag,
ref.snr=ref.snr.flag, dapi.snr=dapi.snr.flag, rep=rep.flag,
unique=unique.flag) 
flag.list1$spatial$args <- alist(var="ScaledLogRatio", by.var=NULL,
nk=5, prop=0.25, thr=0.15, beta=1, family="symmetric") 
flag.list1$spot$args <- alist(var="SpotFlag")
flag.list1$spot$char <- "O"
flag.list1$spot$label <- "Image analysis"

## normalize arrayCGH
\dontrun{edge.norm <- norm.arrayCGH(edge, flag.list=flag.list1,
var="LogRatio", FUN=median, na.rm=TRUE)} 
print(edge.norm$flags) ## spot-level flag summary (computed by flag.summary)

## aggregate arrayCGH without normalization
edge.nonorm <- norm.arrayCGH(edge, flag.list=NULL, var="LogRatio",
FUN=median, na.rm=TRUE)  

## sort genomic informations 
edge.norm <- sort.arrayCGH(edge.norm, position.var="PosOrder")
edge.nonorm <- sort.arrayCGH(edge.nonorm, position.var="PosOrder")

## plot genomic profiles
layout(matrix(c(1,2,4,5,3,3,6,6), 4,2),width=c(1, 4), height=c(6,1,6,1))
report.plot(edge.nonorm, chrLim="LimitChr", layout=FALSE,
main="Pangenomic representation (before normalization)", zlim=c(-1,1),
ylim=c(-3,1))  
report.plot(edge.norm, chrLim="LimitChr", layout=FALSE,
main="Pangenomic representation (after normalization)", zlim=c(-1,1),
ylim=c(-3,1)) 

### 'gradient': global array Trend
## define a list of flags to be applied
flag.list2 <- list(
  spot=spot.flag, global.spatial=global.spatial.flag, SNR=SNR.flag,
  val.mark=val.mark.flag, position=position.flag, unique=unique.flag,
  amplicon=amplicon.flag, replicate=replicate.flag,
  chromosome=chromosome.flag)

## normalize arrayCGH
\dontrun{gradient.norm <- norm.arrayCGH(gradient, flag.list=flag.list2, var="LogRatio", FUN=median, na.rm=TRUE) }
## aggregate arrayCGH without normalization
gradient.nonorm <- norm.arrayCGH(gradient, flag.list=NULL, var="LogRatio", FUN=median, na.rm=TRUE) 

## sort genomic informations 
gradient.norm <- sort.arrayCGH(gradient.norm)
gradient.nonorm <- sort.arrayCGH(gradient.nonorm)

## plot genomic profiles
layout(matrix(c(1,2,4,5,3,3,6,6), 4,2),width=c(1, 4), height=c(6,1,6,1))
report.plot(gradient.nonorm, chrLim="LimitChr", layout=FALSE,
main="Pangenomic representation (before normalization)", zlim=c(-2,2),
ylim=c(-3,2)) 
report.plot(gradient.norm, chrLim="LimitChr", layout=FALSE,
main="Pangenomic representation (after normalization)", zlim=c(-2,2),
ylim=c(-3,2)) 
}

\keyword{}
\seealso{\code{\link{flag}}}

\references{
  P. Neuvial, P. Hupé, I. Brito, S. Liva, E. Manié, C. Brennetot,
    A. Aurias, F. Radvanyi, and E. Barillot. \emph{Spatial normalization
      of array-CGH data}. BMC Bioinformatics, 7(1):264. May 2006.
}
