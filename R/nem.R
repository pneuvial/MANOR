## Spatial classification with EM algorithm

## Copyright (C) 2003 Institut Curie
## Author(s): Philippe Hupé (Institut Curie) 2003
## Contact: manor@curie.fr

nem <- function(...)
    UseMethod("nem")

nem.default <- function(LogRatio, Col, Row, nk=nk, beta=1, iters=2000, ...) {
    ## grid dimension
    nc <- max(Col)
    nr <- max(Row)

    data <- data.frame(LogRatio=LogRatio, Col=Col, Row=Row)

    NbVars <- 1

    if(length(LogRatio)!=length(na.omit(LogRatio)))
        stop("NA's are not allowed")

    if(nr*nc!=length(data$LogRatio))
        stop("invalid grid dimension : rows and colums coordonates does not match with vector LogRatio")

    if(beta < 0)
        stop("beta must be >= 0")

    if(nk <= 0)
        stop("nk must be > 0")

    if(iters < 100)
        stop("iters must be >= 100")

    row.names(data) <- 1:dim(data)[1]
    data <- data[order(data$Col,data$Row),]

    res <- .C("nem",
              as.single(data$LogRatio),
              as.integer(nr),
              as.integer(nc),
              as.integer(NbVars),
              as.integer(nk),
              as.single(beta),
              as.integer(iters),
              classification=integer(nr*nc),
              PACKAGE = "MANOR")$classification

    data <- data.frame(data, ZoneNem=res)
    data <- data[order(as.numeric(row.names(data))),]

    return(data)
}

nem.arrayCGH <- function(arrayCGH, variable, nk=5, beta=1, iters=2000, ...) {
    ## grid dimension
    nc <- arrayCGH$arrayDesign[1]*arrayCGH$arrayDesign[3]
    nr <- arrayCGH$arrayDesign[2]*arrayCGH$arrayDesign[4]

    NbVars <- 1

    if(length(which(names(arrayCGH$arrayValues)==variable))<1)
        stop(paste("variable",variable,"not found",sep=" "))

    if(length(arrayCGH$arrayValues[[variable]])!=length(na.omit(arrayCGH$arrayValues[[variable]])))
        stop("NA's are not allowed")

    if(nr*nc!=length(arrayCGH$arrayValues[[variable]]))
        stop(paste("invalid grid dimension : rows and colums coordonates does not match with vector",variable, sep=" "))

    if(beta < 0)
        stop("beta must be >= 0")

    if(nk <= 0)
        stop("nk must be > 0")

    if(iters < 100)
        stop("iters must be >= 100")

    row.names(arrayCGH$arrayValues) <- 1:length(arrayCGH$arrayValues[[variable]])
    arrayCGH$arrayValues <- arrayCGH$arrayValues[order(arrayCGH$arrayValues$Col,arrayCGH$arrayValues$Row), ]

    res <- .C("nem",
              as.single(arrayCGH$arrayValues[[variable]]),
              as.integer(nr),
              as.integer(nc),
              as.integer(NbVars),
              as.integer(nk),
              as.single(beta),
              as.integer(iters),
              classification=integer(nr*nc),
              PACKAGE = "MANOR")$classification

    arrayCGH$arrayValues$ZoneNem <- res
    arrayCGH$arrayValues <- arrayCGH$arrayValues[order(as.numeric(row.names(arrayCGH$arrayValues))),]
    return(arrayCGH)
}

