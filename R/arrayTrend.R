## Spatial trend of an arrayCGH object

## Copyright (C) 2003 Institut Curie
## Author(s): Philippe Hupé (Institut Curie) 2003
## Contact: manor@curie.fr


arrayTrend <- function(...)
{
    UseMethod("arrayTrend")
}

arrayTrend.default <- function(Statistic, Col, Row, ...)
{
                                        #if(!require("modreg"))stop("Could not load required package modreg")
                                        #if(!require("stats"))stop("Could not load required package stats")
                                        #nb modreg is automatically loaded in R 1.7.0
                                        # loess in in package stats (automatically loaded)


    loess <- loess(Statistic ~ Col*Row, ...)
    predict.trend <- predict(loess, data.frame(Col=Col, Row=Row))
    data <- data.frame(Trend=predict.trend, Col=Col, Row=Row)

    return(data)
}

arrayTrend.arrayCGH <- function(arrayCGH,variable, ...)
{
                                        #if(!require("modreg"))stop("Could not load required package modreg")
                                        #if(!require("stats"))stop("Could not load required package stats")
    if(length(which(names(arrayCGH$arrayValues)==variable))<1)stop(paste("variable",variable,"not found",sep=" "))
                                        #nb modreg is automatically loaded in R 1.7.0
    Col <- arrayCGH$arrayValues$Col
    Row <- arrayCGH$arrayValues$Row
    Statistic <- arrayCGH$arrayValues[[variable]]
    loess <- loess(Statistic ~ Col*Row, weights=arrayCGH$arrayValues$Weight, ...)
    predict.trend <- predict(loess, data.frame(Col=arrayCGH$arrayValues$Col, Row=arrayCGH$arrayValues$Row))
    arrayCGH$arrayValues$Trend <- predict.trend

    return(arrayCGH)
}
