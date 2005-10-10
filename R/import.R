## Data importation froml text files to arrayCGH objects

## Copyright (C) 2004 Institut Curie
## Author(s): Pierre Neuvial (Institut Curie) 2004
## Contact: manor@curie.fr


import <- function(file, var.names=NULL, spot.names=NULL, clone.names=NULL, type=c("default", "gpr", "spot"), id.rep=1, design=NULL, add.lines=FALSE, ...)  {
  l <- length(var.names)
  var.names <- switch(match.arg(type),
                      gpr = if (l!=5) c("Block", "Column", "Row", "X", "Y") else var.names,
                      spot= if (l!=4) c("Arr.colx", "Arr.rowy", "Spot.colx", "Spot.rowy") else var.names,
                      default= if (l!=2) c("Col", "Row") else var.names)
### read raw data (skipping header lines if necessary)
  data <- read.delim(file, skip=grep(var.names[1], readLines(file, n = 100)) - 1, as.is=TRUE, ...)
  
### exclude unmatched variable names from import
  data.names <- names(data)
  if (!is.null(var.names))
    var.names <- check.names(var.names, data.names, optional=FALSE)
  if (!is.null(clone.names))
    {
      spot.names <- union(spot.names, clone.names[id.rep])  ## force id.rep to be present in spot.data
      clone.names <- check.names(clone.names, data.names)
    }
  if (!is.null(spot.names))
    spot.names <- check.names(spot.names, data.names)
  
  ret <- switch(match.arg(type),
                gpr = import.gpr.aux(data, var.names=var.names, spot.names=spot.names, add.lines=add.lines, design=design),
                spot = import.spot.aux(data, var.names=var.names, spot.names=spot.names, add.lines=add.lines),
                default = import.default.aux(data, var.names=var.names, spot.names=spot.names, design=design))
  spot.data <- ret$spot.data
  design <- ret$design
  ## sort lines of spot.data
  o <- order(spot.data$Row, spot.data$Col)
  spot.data <- spot.data[o, ]
    
### optional clone-level information to be kept in arrayCGH
  if (!is.null(clone.names)) {
    clone.data <- data[, clone.names]
    ## this information has to be aggregated from spot-level information to clone-level information
    if (length(clone.names) > 1)
      clone.data <- my.aggregate.data.frame(clone.data, clone.names[id.rep], function(x) {x[1]})
    else {
      clone.data <- data.frame(unique(clone.data))
      names(clone.data) <- clone.names
    }
    arrayCGH <- list(arrayValues=spot.data, arrayDesign=design, cloneValues=clone.data, id.rep=clone.names[id.rep])
  }
  else
    arrayCGH <- list(arrayValues=spot.data, arrayDesign=design)
  class(arrayCGH) <- "arrayCGH"
  
  arrayCGH
}

import.gpr.aux <- function(data, var.names=c("Block", "Column", "Row", "X", "Y"), spot.names=NULL, add.lines=FALSE, design=NULL) {
  data[[var.names[1]]] <- data[[var.names[1]]] - min(data[[var.names[1]]])+1 ## "Block" now starts from 1
  Block <- data[[var.names[1]]]                     # "Block"
  Column <- data[[var.names[2]]]                    # "Column"
  Row <- data[[var.names[3]]]                       # "Row"
  
  max.col <- max(Column, na.rm=TRUE)                 # total number of columns per block of the array
  max.row <- max(Row, na.rm=TRUE)                    # total number of rows    per block of the array
  max.block <- max(Block, na.rm=TRUE)

### add lines for empty spots if necessary ;)
  if (max.block*max.col*max.row!=dim(data)[1]) {
    if (!add.lines)
      stop("Incomplete .gpr file: number of lines does not match array design")
    else {
      print("number of lines does not match array design: adding empty lines...")
      test <- merge(data.frame(1:max.block), merge(data.frame(1:max.col), data.frame(1:max.row)))
      names(test) <- var.names[1:3]
      data <- merge(data, test, all=TRUE)
      
      ## new variables
      Block <- data[[var.names[1]]]                     # "Block"
      Column <- data[[var.names[2]]]                    # "Column"
      Row <- data[[var.names[3]]]                       # "Row"
    }
  }
  X <- data[[var.names[4]]]                         # "IMAGE_X"
  Y <- data[[var.names[5]]]                         # "IMAGE_Y"
  
### compute array design
  if (is.null(design)) {
    print("calculating array design...")
    ## block design was not specified by user: it has to be computed
    tmp <- aggregate(X, list(Block=Block), median, na.rm=TRUE)
    ## blocks are designed from left to right, top to bottom
    x <- tmp$x
    l <- length(x)
    y <- x[2:l]-x[1:l-1]
    ## so the first decrease in X occurs after the last block of the first row
    br <- which(y<0)[1] ## number of blocks per row
    design <- c(br, max.block/br, max.col, max.row)
  }
  row.blocks <- design[1]           # number of blocks per row of the array
  i <- (Block+row.blocks-1)%/%row.blocks
  j <- (Block-1)%%row.blocks+1
    
### build "Col" and "Row" variables of the arrayCGH
  col <- (j-1)*max.col + Column
  row <- (i-1)*max.row + Row
  
  ## optional spot-level information to be kept in arrayCGH
  if (!is.null(spot.names)) {
    spot.data <- data.frame(col, row, data[, spot.names])
    names(spot.data) <- c("Col", "Row", spot.names) ## useful if clone.names is of length 1
  }
  else
    spot.data <- data.frame(Col=col, Row=row)
  list(spot.data=spot.data, design=design)
}

import.spot.aux <- function(data, var.names=c("Arr.colx", "Arr.rowy", "Spot.colx", "Spot.rowy"), spot.names=NULL, add.lines=FALSE) {
  d <- data[, var.names]
  names(d) <- c("ac", "ar", "sc", "sr")

### compute array design (a little easier than for .gpr files...)
  design <- as.numeric(apply(d, 2, max))
    
### add lines for empty spots if necessary ;)
  if (prod(design)!=dim(data)[1]) {
    if (!add.lines)
      stop("Incomplete .gpr file: number of lines does not match array design. Use 'add.lines=TRUE' to add empty lines")
    else {
      print("number of lines does not match array design: adding empty lines...")
      test <- merge(merge(data.frame(1:design[1]), data.frame(1:design[2])), merge(data.frame(1:design[3]), data.frame(1:design[4])))
      names(test) <- var.names
      data <- merge(data, test, all=TRUE)

      ## one more time...
      d <- data[, var.names]
      names(d) <- c("ac", "ar", "sc", "sr")
    }
  }

### build "Col" and "Row" variables of the arrayCGH
  col <- (d$ac-1)*design[3]+d$sc
  row <- (d$ar-1)*design[4]+d$sr
  
  ## optional spot-level information to be kept in arrayCGH
  if (!is.null(spot.names)) {
    spot.data <- data.frame(col, row, data[, spot.names])
    names(spot.data) <- c("Col", "Row", spot.names) ## useful if clone.names is of length 1
  }
  else
    spot.data <- data.frame(Col=col, Row=row)
  list(spot.data=spot.data, design=design)
}

import.default.aux <- function(data, var.names=c("Col", "Row"), spot.names=NULL, design=NULL) {
  if (is.null(design))
    stop("You must specify array design in order to import this type of data")
### optional spot-level information to be kept in arrayCGH
  if (!is.null(spot.names)) {
    spot.data <- data[, union(var.names, spot.names)]
    names(spot.data) <- c("Col", "Row", spot.names) ## useful if clone.names is of length 1
  }
  else
    spot.data <- data[, var.names]
  list(spot.data=spot.data, design=design)
}
