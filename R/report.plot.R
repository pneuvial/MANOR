## Copyright (C) 2004 Institut Curie
## Author(s): Pierre Neuvial (Institut Curie) 2004
## Contact: manor@curie.fr


report.plot <- function(...)
    UseMethod("report.plot")

report.plot.arrayCGH <- function(arrayCGH, x="PosOrder", y=c("LogRatioNorm", "LogRatio"), chrLim=NULL, layout=TRUE, main=NULL, zlim=NULL, ...) {
    if (layout)
        layout(matrix(c(1,2,3,3), 2,2), height=c(7,1), width=c(1,3))
    ##        compute.layout.genome(arrayCGH$arrayDesign[1], arrayCGH$arrayDesign[2], bar=bar, surf=surf)
    if(length(y)==1)
        y <- c(y,y)  ## if only one signal variable name is provided, the same name is taken for clone and spot-level analysis
    y.spot <- y[1]
    y.clone <- y[2]

    arrayPlot(arrayCGH, y.spot, main="Array image", bar="h", mediancenter=TRUE, layout=FALSE, zlim=zlim)
    if (is.null(main))
        main <- "Pan-genomic representation"
    genome.plot(arrayCGH, x=x, y=y.clone, main=main, chrLim=chrLim, ...)
}

report.plot.default <- function(spot.data, clone.data, design, x="PosOrder", y=c("LogRatioNorm", "LogRatio"), chrLim=NULL, layout=TRUE, main=NULL, zlim=NULL, ...) {
    arrayCGH <- list(arrayValues=spot.data, cloneValues=clone.data, arrayDesign=design)
    class(arrayCGH) <- "arrayCGH"
    if (layout)
        layout(matrix(1:2,1,2), width=c(1,4))
    if(length(y)==1)
        y <- c(y,y)  ## if only one signal variable name is provided, the same name is taken for clone and spot-level analysis
    y.spot <- y[1]
    y.clone <- y[2]

    arrayPlot.arrayCGH(arrayCGH, y.spot, main="Array image", bar="horizontal", mediancenter=TRUE, zlim=zlim)
    if (is.null(main))
        main <- "Pan-genomic representation"
    genome.plot(arrayCGH, x=x, y=y.clone, main=main, chrLim=chrLim, ...)
}
