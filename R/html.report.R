## Generation of an html page with a summary of the normalization process

## Copyright (C) 2004 Institut Curie
## Author(s): Pierre Neuvial (Institut Curie) 2004
## Contact: manor@curie.fr


w <- function(x1="", x2="", x3="", append=TRUE, file="rpt")
    write(paste(x1, x2, x3, sep=""), append=append, file=file)

html.report <- function(...)
    UseMethod("html.report")

html.report.arrayCGH <- function(array.norm, array.nonorm=NULL, dir.out=".", array.name=NULL, x="PosOrder", y=c("LogRatioNorm", "LogRatio"), chrLim=NULL, ylim=NULL, zlim=NULL, clim=NULL, intensity=NULL, light=FALSE, file.name="report", width=10, height=5, ...){
### outuput file names
    if (is.null(array.name))
        array.name <- "arrayCGH"
    if (!file.exists(dir.out))
        dir.create(dir.out)
    ##  img.nonorm <- paste("nonorm_", array.name, ".png", sep="")
    ##  img.norm <- paste("norm_", array.name, ".png", sep="")
    if (!light)
        fh <- paste(dir.out, "/", file.name, ".html", sep="")
    else
        fh <- paste(dir.out, "/", file.name, sep="")
    an <- gsub(" ", "_", array.name)
    img.maplot <- paste(an, "_maplot.png", sep="")
    img.nonorm <- paste(an, "_nonorm.png", sep="")
    img.norm <- paste(an, "_norm.png", sep="")

    if(length(y)==1)
        y <- c(y,y)  ## if only one signal variable name is provided, the same name is taken for clone and spot-level analysis

### spot- and clone-level flag summary
    clone.var <- array.norm$cloneValues[[y[2]]]
    after <- sum(!is.na(clone.var)) ## number of clones after normalization
    before <- length(clone.var)     ## number of clones before normalization
    clones <- list(before=before, after=after)

    if (!light) {
###header of html output file
        w("<html>", append=FALSE, file=fh)
        w("<head> <title>Normalization report</title> </head>", file=fh)
        w("<body bgcolor=\"lightsteelblue\" text=\"black\" link=\"blue\">", file=fh)
    }

    ## beginning of a big table
    w("<center>", append=(!light), file=fh)
    ## report title
    tit <- paste("MANOR: analysis of ", array.name, sep="")
    w("<h2><font color=\"purple\">", tit, "</font></h2>", file=fh)
    w("<h5>", format(Sys.time(), "%Y-%m-%d, %X"), "</h5>", file=fh)
    w("<table bgcolor=\"lightsteelblue\">", file=fh)

### MA-plot (if intensity is not NULL)
    if (!is.null(intensity)) {
        bitmap(paste(dir.out, "/", img.maplot, sep=""), width=width, height=height, pointsize=10, bg="white")
        intensity.plot(array.norm, M.var=intensity[["M.var"]], A.var=intensity[["A.var"]], pred.var=intensity[["pred.var"]], center.var=intensity[["center.var"]])
        dev.off()
        w("<tr align=center><td style=\"font-size:20;color:black;\">M-A plot (span=", intensity[["span"]], ")</font></td></tr>", file=fh)
        w("<tr><td><img src=\"", img.maplot, "\" alt=\"<M-A plot>\"></td></tr>", file=fh)
    }

### genomic- and array-profile without normalization (if array.nonorm is provided)
    if (!is.null(array.nonorm)) {
        clone.var.nonorm <- array.nonorm$cloneValues[[y[2]]]
        if (is.null(ylim))
            ylim <- c(min(clone.var.nonorm, na.rm=TRUE), max(clone.var.nonorm, na.rm=TRUE))
        if (is.null(clim))
            clim <- c(quantile(clone.var.nonorm, 0.05, na.rm=TRUE), quantile(clone.var.nonorm, 0.95, na.rm=TRUE))
        if (is.null(zlim))
            zlim <- clim

        bitmap(paste(dir.out, "/", img.nonorm, sep=""), width=width, height=height, pointsize=10, bg="white")
        ##    bitmap(paste(dir.out, "/", img.nonorm, sep=""), pointsize=10, bg="white")
        ##    png(filename = paste(dir.out, "/", img.nonorm, sep=""), width = 800, height = 300, pointsize = 10, bg = "white")
        report.plot(array.nonorm, x=x, y=y, chrLim=chrLim, ylim=ylim, zlim=zlim, clim=clim, ...)
        dev.off()
        w("<tr align=center><td style=\"font-size:20;color:black;\">Profile before normalization: ", clones$before, " clones</font></td></tr>", file=fh)
        w("<tr><td><img src=\"", img.nonorm, "\" alt=\"<image without normalization>\"></td></tr>", file=fh)
    }

### genomic- and array-profile with normalization
    bitmap(paste(dir.out, "/", img.norm, sep=""), width=width, height=height, pointsize=10, bg="white")
    ##  bitmap(paste(dir.out, "/", img.norm, sep=""), pointsize=10, bg="white")
    ##  png(filename = paste(dir.out, "/", img.norm, sep=""), width = 800, height = 300, pointsize = 10, bg = "white")
    report.plot(array.norm, x=x, y=y, chrLim=chrLim, ylim=ylim, zlim=zlim, clim=clim, ...)
    dev.off()

    c2 <- paste(clones$after, " clones (", round(clones$after*100/clones$before), "%) ", sep="")
    w("<tr align=center><td style=\"font-size:20;color:black;\">Profile after normalization: ", c2, "</font></td></tr>", file=fh)
    w("<tr><td><img src=\"", img.norm, "\" alt=\"<image with normalization>\"></td></tr>", file=fh)

### table with spot-level flag summary
    df <- array.norm$flags
    if (!is.null(df)) {
        df$arg[which(is.na(df$arg))] <- "-"
        w("<table border bgcolor=\"lightyellow\">", file=fh)
        w("<caption align=top><h2><font color=\"black\"> Spot-level flag report </font></h2></caption>", file=fh)
        if (!is.null(df$label))
            w("<tr><th>Flag label</th> <th>Flag code</th> <th>Parameter</th> <th>Flagged spots</th> <th>Flagged spots(%)</th> </tr>", file=fh)
        else
            w("<tr><th>Flag code</th> <th>Parameter</th> <th>Flagged spots</th> <th>Flagged spots(%)</th> </tr>", file=fh)
        for (i in 1:length(df$char)) {
            if (!is.null(df$label))
                line <- paste(df$label[i], df$char[i], df$arg[i], df$count[i], round(100*df$count[i]/sum(df$count)), sep="</th> <th>")
            else
                line <- paste(df$char[i], df$arg[i], df$count[i], round(100*df$count[i]/sum(df$count)), sep="</th> <th>")
            w("<tr><th>", line, "</th></tr>", file=fh)
        }
        w("</table>", file=fh)
    }

### table with array-level quality criteria
    qdf <- array.norm$quality
    if (!is.null(qdf)) {
        w("<table border bgcolor=\"lightyellow\">", file=fh)
        w("<caption align=top> <h2><font color=\"black\"> Quality criteria </font></h2></caption>", file=fh)
        w("<tr><th>Label</th><th>Value</th> </tr>", file=fh)
        for (i in 1:(dim(qdf)[1])) {
            line <- paste(qdf$label[i], qdf$score[i], sep="</th> <th>")
            w("<tr><th>", line, "</th></tr>", file=fh)
        }
        w("</table>", file=fh)
    }

    ## end of big table
    w("</table></center>", file=fh)

    if (!light) {
        w("</body>", file=fh)
        w("</html>", file=fh)
    }
}

html.report.default <- function(spot.data, clone.data=NULL, flag.data=NULL, quality.data=NULL, ...) {
### create an object of type arrayCGH
    array.norm <- list(arrayValues=spot.data, cloneValues=clone.data, quality=quality.data, flags=flag.data)
    class(array.norm) <- "arrayCGH"

### apply html.report.arrayCGH
    html.report.arrayCGH(array.norm, array.nonorm=NULL, ...)
}
