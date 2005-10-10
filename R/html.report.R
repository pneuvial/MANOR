## Generation of an html page with a summary of the normalization process

## Copyright (C) 2004 Institut Curie
## Author(s): Pierre Neuvial (Institut Curie) 2004
## Contact: manor@curie.fr


w <- function(x1="", x2="", x3="", append=TRUE, file=fh)
  write(paste(x1, x2, x3, sep=""), append=append, file=file)

html.report <- function(...)
  UseMethod("html.report")

html.report.arrayCGH <- function(array.norm, array.nonorm=NULL, dir.out=".", array.name=NULL, x="PosOrder", y=c("LogRatioNorm", "LogRatio"), chrLim=NULL, ylim=NULL, zlim=NULL, clim=NULL, light=FALSE, file.name="report", width=10, height=5, ...){
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
    w("<HTML>", append=FALSE, file=fh)
    w("<HEAD> <TITLE>Normalization report</TITLE> </HEAD>", file=fh)
    w("<BODY bgcolor=\"lightsteelblue\" text=\"black\" link=\"blue\">", file=fh)
  }

  ## beginning of a big table
  w("<CENTER>", append=(!light), file=fh)
  ## report title
  tit <- paste("MANOR: analysis of ", array.name, sep="")
  w("<h2><font COLOR=\"purple\">", tit, "</font></h2>", file=fh)
  w("<h5>", format(Sys.time(), "%Y-%m-%d, %X"), "</h5>", file=fh)
  w("<TABLE BGCOLOR=\"lightsteelblue\">", file=fh)

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
  w("<TR align=center><TD style=\"font-size:20;color:black;\">Profile before normalization: ", clones$before, " clones</font></TD></TR>", file=fh)
  w("<TR><TD><IMG SRC=\"", img.nonorm, "\" ALT=\"<image without normalization>\"></TD></TR>", file=fh)
  }

### genomic- and array-profile with normalization
  bitmap(paste(dir.out, "/", img.norm, sep=""), width=width, height=height, pointsize=10, bg="white")
##  bitmap(paste(dir.out, "/", img.norm, sep=""), pointsize=10, bg="white")
##  png(filename = paste(dir.out, "/", img.norm, sep=""), width = 800, height = 300, pointsize = 10, bg = "white")
  report.plot(array.norm, x=x, y=y, chrLim=chrLim, ylim=ylim, zlim=zlim, clim=clim, ...)
  dev.off()

  c2 <- paste(clones$after, " clones (", round(clones$after*100/clones$before), "%) ", sep="")
  w("<TR align=center><TD style=\"font-size:20;color:black;\">Profile after normalization: ", c2, "</font></TD></TR>", file=fh)
  w("<TR><TD><IMG SRC=\"", img.norm, "\" ALT=\"<image with normalization>\"></TD></TR>", file=fh)

### table with spot-level flag summary
  df <- array.norm$flags
  df$arg[which(is.na(df$arg))] <- "-"
  w("<TABLE BORDER BGCOLOR=\"lightyellow\">", file=fh)
  w("<CAPTION align=top><h2><font COLOR=\"black\"> Spot-level flag report </font></h2></CAPTION>", file=fh)
  if (!is.null(df$label))
    w("<TR><TH>Flag label</TH> <TH>Flag code</TH> <TH>Parameter</TH> <TH>Flagged spots</TH> <TH>Flagged spots(%)</TH> </TR>", file=fh)
  else
    w("<TR><TH>Flag code</TH> <TH>Parameter</TH> <TH>Flagged spots</TH> <TH>Flagged spots(%)</TH> </TR>", file=fh)
  for (i in 1:length(df$char)) {
    if (!is.null(df$label))
      line <- paste(df$label[i], df$char[i], df$arg[i], df$count[i], round(100*df$count[i]/sum(df$count)), sep="</TH> <TH>")
    else
      line <- paste(df$char[i], df$arg[i], df$count[i], round(100*df$count[i]/sum(df$count)), sep="</TH> <TH>")
    w("<TR><TH>", line, "</TH></TR>", file=fh)
  }
  w("</TABLE>", file=fh)

### table with array-level quality criteria
  qdf <- array.norm$quality
  if (!is.null(qdf)) {
    w("<TABLE BORDER BGCOLOR=\"lightyellow\">", file=fh)
    w("<CAPTION align=top> <h2><font COLOR=\"black\"> Quality criteria </font></h2></CAPTION>", file=fh)
    w("<TR><TH>Label</TH><TH>Value</TH> </TR>", file=fh)
    for (i in 1:(dim(qdf)[1])) {
      line <- paste(qdf$label[i], qdf$score[i], sep="</TH> <TH>")
      w("<TR><TH>", line, "</TH></TR>", file=fh)
    }
    w("</TABLE>", file=fh)
  }

  ## end of big table
  w("</TABLE></CENTER>", file=fh)
  
  if (!light) {
    w("</BODY>", file=fh)
    w("</HTML>", file=fh)
  }
}

html.report.default <- function(spot.data, clone.data=NULL, flag.data=NULL, quality.data=NULL, ...) {
### create an object of type arrayCGH
  array.norm <- list(arrayValues=spot.data, cloneValues=clone.data, quality=quality.data, flags=flag.data)
  class(array.norm) <- "arrayCGH"
  
### apply html.report.arrayCGH
  html.report.arrayCGH(array.norm, array.nonorm=NULL, ...)
}
