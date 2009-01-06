## Copyright (C) 2004 Institut Curie
## Author(s): Pierre Neuvial (Institut Curie) 2004
## Contact: manor@curie.fr


genome.plot <- function(...)
    UseMethod("genome.plot")

genome.plot.arrayCGH <- function(arrayCGH, x="PosOrder", y="LogRatio", chrLim=NULL, col.var=NULL, clim=NULL, cex=NULL, pch=NULL, ...)
{
    xx <- arrayCGH$cloneValues[[x]]
    yy <- as.numeric(arrayCGH$cloneValues[[y]])

    data <- data.frame(x=xx, y=yy)

    if (!is.null(chrLim))
        data <- cbind(data, chrLim=arrayCGH$cloneValues[[chrLim]])

    if (is.null(col.var)) {
        col.var <- y
    }
    z <- arrayCGH$cloneValues[[col.var]]

    if (class(z)=="numeric") {
        if (length(clim)!=2)
            clim <- c(quantile(z, 0.05, na.rm=TRUE), quantile(z, 0.95, na.rm=TRUE))
        max <- clim[2]
        min <- clim[1]
        col <- myPalette("green", "red", "yellow")
        c <- (z-min)/(max-min)*((z>=min))*((z<=max))
        c[z>=max] <- 1
        mycol <- col[1+floor((length(col)-1)*c)]
    }

    else {
        mycol <- as.factor(z)
        l <- length(levels(mycol))
        levels(mycol) <- rainbow(l)
    }

    data <- cbind(data, col=mycol)
    genome.plot.default(data, cex=cex, pch=pch, ...)
}

genome.plot.default <- function(data, pch=NULL, cex=NULL, xlab="", ylab="", ...)
{
    ## default parameters
    pch <- if(is.null(pch))
        20
    else pch

    cex <- if(is.null(cex))
        0.5
    else cex

    xlab <- if(!nchar(xlab))
        "Genome position"
    else
        xlab

    ylab <- if(!nchar(ylab))
        "DNA Copy Number Variation"
    else ylab

    if(!is.null(data$col))
        plot(data$y~data$x, col=as.character(data$col), pch=pch, cex=cex, xlab=xlab, ylab=ylab, ...)
    else
        plot(data$y~data$x, pch=pch, cex=cex, xlab=xlab, ...)

    if (!is.null(data$chrLim))
    {
        w <- which(data$chrLim==1)
        for (j in 1:length(w)) abline(v=data$x[w[j]], lty=2, col="black")
    }
}
