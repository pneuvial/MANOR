## Copyright (C) 2004 Institut Curie
## Author(s): Pierre Neuvial (Institut Curie) 2004
## Contact: manor@curie.fr


intensity.plot <- function(...)
    UseMethod("intensity.plot")

intensity.plot <- function(arrayCGH, M.var="LogRatio", A.var="LogInt", pred.var="iPred", center.var=NULL) {
    data <- arrayCGH$arrayValues
    if (!is.null(center.var))
        data[[M.var]] <- center(data, M.var, center.var)
    plot(data[[A.var]], data[[M.var]], xlab="A", ylab="M", pch='.')
    points(data[[A.var]], data[[pred.var]], col="red", pch=".")
}

