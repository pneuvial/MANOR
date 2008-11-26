\name{import}
\alias{import}
\alias{import.default.aux}
\alias{import.gpr.aux}
\alias{import.spot.aux}
\title{Import raw file to an arrayCGH object}
\description{
  Load raw data from a text file coming from image analysis and convert it to an \code{\link[GLAD]{arrayCGH}}
  object, using additional information about the array design.

  Supported file types are Genepix Results file (.gpr), outputs from
  SPOT, or any text file with appropriate fields "Row" and "Column" and
  specified array design
}
\details{
  Mandatory elements of \code{\link[GLAD]{arrayCGH}} objects are the array design and the x and y
  \emph{absolute coordinates} of each spot on the array. Output files
  from SPOT contain x and y relative coordinates of each spot within a
  block, as well as block coordinates on the array; one can therefore
  easily construct te corresponding \code{\link[GLAD]{arrayCGH}}
  object.

  .gpr files currently only contain x and y relative coordinates of each
  spot within a block, and block index with no specification of the 
  spatial block design: if block design is not specified by user, we
  compute it using the real pixel locations of each spot (\code{X} and \code{Y} variables in
  usual .gpr files)  

  If clone.names is provided, an additional data frame is created with
  clone-level information (e.g. clone names, positions, 
  chromosomes, quality marks), aggregated from array-level information
  using the identifier specified by id.rep. This identifier is also
  added to the \code{arrayCGH} object created, with name 'id.rep'.

  Due to space limitations, only the first 100 lines of sample 'gpr' and
  'spot' files are given in the standard distribution of
  \code{MANOR}. Complete files are available at \url{http://bioinfo.curie.fr/projects/manor/index.html}
}

\usage{
  import(file, var.names=NULL, spot.names=NULL, clone.names=NULL, type=c("default", "gpr", "spot"), id.rep=1, design=NULL, add.lines=FALSE, ...)
}
\arguments{
  \item{file}{a connection or character string giving the name of the
    file to import.}
  \item{var.names}{a vector of variables names used to compute the array
    design. If default is not
    overwritten, it is set to c("Block", "Column", "Row", "X", "Y") for
    gpr files, c("Arr.colx", "Arr.rowy", "Spot.colx", "Spot.rowy") for
    SPOT files, and c("Col", "Row") for other text files
  }
  \item{spot.names}{a list with spot-level variable names to be added to
    \code{arrayCGH$arrayValues}}
  \item{clone.names}{a list with clone-level variable names to be added
    to \code{arrayCGH$cloneValues} (only used in case of within-slide replicates)} 
  \item{type}{a character value specifying the type of input file:
    currently .gpr files ("gpr"), spot files ("spot") and other text
    files with fields 'Col' and 'Row' ("default") are supported}
  \item{id.rep}{index of the replicate identifier (e.g. the name of the clone) in the
  vector(clone.names)}
  \item{design}{a numeric vector of length 4 specifying array design as
    number of blocks per column, number of blocks per row, number of columns by block, number of
    rows per block. This field is mandatory
    for "default" text files, optional for "gpr" files, and not used for
    "SPOT" files}
  \item{add.lines}{boolean value to handle the case when array design
    does not match number of lines. If TRUE, empty lines are added; if
    FALSE, execution is stopped}
  \item{...}{additional import parameters (e.g. '\code{sep=}', or '\code{comment.char=}', to be passed to read.delim
    function. Note that argument \code{as.is=TRUE} is always passed to
    read.delim, in order to avoid unapropriate conversion of character
    vectors to factors}
}

\value{an object of class \code{\link[GLAD]{arrayCGH}}}

\author{Pierre Neuvial, \email{manor@curie.fr}.}

\note{People interested in tools for array-CGH analysis can
  visit our web-page: \url{http://bioinfo.curie.fr}.}

\examples{
dir.in <- system.file("data", package="MANOR")

## import from 'spot' files
spot.names <- c("LogRatio", "RefFore", "RefBack", "DapiFore", "DapiBack", "SpotFlag", "ScaledLogRatio")
clone.names <- c("PosOrder", "Chromosome")
edge <- import(paste(dir.in, "/edge.txt", sep=""), type="spot",
spot.names=spot.names, clone.names=clone.names, add.lines=TRUE) 

## import from 'gpr' files
spot.names <- c("Clone", "FLAG", "TEST_B_MEAN", "REF_B_MEAN",
"TEST_F_MEAN", "REF_F_MEAN", "ChromosomeArm")  
clone.names <- c("Clone", "Chromosome", "Position", "Validation")
  
ac <- import(paste(dir.in, "/gradient.gpr", sep=""), type="gpr",
spot.names=spot.names, clone.names=clone.names, sep="\t", 
comment.char="@", add.lines=TRUE) 
}

\keyword{import,read}
\seealso{\code{\link[GLAD]{arrayCGH}}}
