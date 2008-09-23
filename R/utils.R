## useful auxiliary functions

## Copyright (C) 2004 Institut Curie
## Author(s): Pierre Neuvial (Institut Curie) 2004
## Contact: manor@curie.fr


### coefficient of variation
cv <- function(x)
{
## 	x <- x[!is.na(x)]
## 	if(length(x) > 0)
## 		ret <- sd(2^x)/mean(2^x)
## 	else
## 		ret <- as.numeric(NA)
## 	ret

  ### modification de la fonction le 23/09/2008 (P. Hupe)
  ### permet de retourner NA quand toutes les valeurs de x sont à NA (sinon il y a une erreur)
  if (length(attr(na.omit(x),"na.action"))==length(x))
    return(NA)
  else 
    return(sd(2^x,na.rm=TRUE)/mean(2^x,na.rm=TRUE))

        
}

### rename variables of a data.frame
rename <- function(data, old.vars, new.vars)
{
    if (length(old.vars)!=length(new.vars))
        warning("old.vars and new.vars have different lengths...\n")
    m <- match(old.vars, names(data))
    w <- which(is.na(m))
    if (length(w))
    {
        for (i in 1:length(w)) warning(paste("Variable not found: ", old.vars[w[i]], "\n", sep=""))
        m <- m[-w]
        new.vars <- new.vars[-w]
    }
    names(data)[m] <- new.vars
    data
}

### aggregate.data.frame (light version)
my.aggregate.data.frame <- function (x, by, FUN, ...)
{
    x <- as.data.frame(x)
    b <- x[, by]
    y <- lapply(x, tapply, list(b), FUN, ..., simplify = FALSE)
    if (any(sapply(unlist(y, recursive = FALSE), length) > 1))
        stop("`FUN' must always return a scalar")
    y <- data.frame(lapply(y, unlist, use.names = FALSE))
    names(y) <- c(names(x))
    y
}

## from character/factor to numeric chromosome index...
chrom <- function(c)
{
    char.c <- as.character(c)
    char.c[(char.c=="X")] <- 23
    char.c[(char.c=="Y")] <- 24
    char.c[(char.c=="Z")] <- 25
    char.c[(char.c=="")] <- 0
    chr <- as.numeric(char.c)
    chr
}

flag.aggregate.arrayCGH <- function(x)
                                        # transfer clone-level information to all spots from a clone
{
    if (length(grep("V", x)))
                                        # bad CLONE (the 3 spots have to be excluded)
        ret <- "V"
    else
    {
        if (length(grep("T", x)))
                                        # bad CLONE (the 3 spots have to be excluded)
            ret <- "T"
        else
            ret <- paste(x, collapse="")
    }
    ret
}

## check that data.names contain test.names
check.names <- function(test.names, data.names, optional=TRUE) {
    w <- which(is.na(match(test.names, data.names)))
    if (length(w)) {
        for (i in 1:length(w)) warning(paste("Variable not found: ", test.names[w[i]], "\n", sep=""))
        if (!optional)
            stop ("Mandatory variables not found")
        test.names <- test.names[-w]
    }
    test.names
}

## add empty lines for design compatibility (pseudo-gpr files)
add.lines <- function(data, sep="_")
{
    df <- NULL
    if (dim(data)[2]!=3)
        stop("Wrong dimension for input data")
    x1 <- data[[1]]
    x2 <- data[[2]]
    x3 <- data[[3]]

    test <- merge(data.frame(x1=1:max(x1)), data.frame(x2=1:max(x2)))
    test <- merge(data.frame(x3=1:max(x3)), test)

    sample <- paste(x1, x2, x3, sep=sep)
    patron <- paste(test$x1, test$x2, test$x3, sep=sep)

##    if(dim(test)[1]!=dim(r)[1])
    if(dim(test)[1]!=dim(data)[1])
    {
        ## design is now compatible with the number of rows in the input file
        w <- which(is.na(match(patron, sample)))              # locate discrepencies
        s <- matrix(unlist(strsplit(patron[w], sep)), ncol=3, byrow=TRUE)
        df <- as.data.frame(s)
        names(df) <- names(data)
    }
    df
}

center <-  function(data, var, by.var) {
    index.name <- "zzz.row.index"
    data[[index.name]] <- 1:(dim(data)[1])
    a <- aggregate(data[[var]], list(b=data[[by.var]]), median, na.rm=TRUE)
    var.median <- paste(var, ".median", sep="")
    names(a)[2] <- var.median
    m <- merge(data, a, by.y="b", by.x=by.var, all.x=TRUE, sort=FALSE)
    m <- m[order(m[[index.name]]),]
    m[[var]]-m[[var.median]]
}

getChromosomeArm <- function(arrayCGH, chrVar="Chromosome", posVar="Position") {
    cytoband <- NULL ## avoids a warning when loading cytoband data...
    
    data("cytoband") ### l'appel de la fonction data crée l'objet cytoband dans .GlobalEnv

    cytoband <- get("cytoband", envir=.GlobalEnv) ### on récupère l'objet cytoband contenu dans .GlobalEnv car sinon il reste positionné à NULL
    
    chrArmVar <- "ChromosomeArm"
    chrNumVar <- paste(chrVar, "num", sep=".")
    centromere <- cytoband[which(cytoband$Centro==1),]

    w <- NULL
    for (u in unique(centromere$Chromosome)) {
        wu <- which(centromere$Chromosome==u)
        w <- c(w, wu[which.max(centromere[["Start"]])])
    }

    centromere <- centromere[w,c("Chromosome","Start")]
    centromere$Chromosome <- ChrNumeric(centromere$Chromosome)
    names(centromere) <- c(chrNumVar,"CentroPos")

    arrayCGH$arrayValues[[chrNumVar]] <- ChrNumeric(arrayCGH$arrayValues[[chrVar]])
    arrayCGH$arrayValues <- merge(arrayCGH$arrayValues, centromere, all.x=TRUE)
    arrayCGH$arrayValues[[chrArmVar]] <- "p"
    indNA <- which(is.na(arrayCGH$arrayValues[[chrVar]]))
    if (length(indNA))
        arrayCGH$arrayValues[[chrArmVar]][indNA] <- NA

    ind <- which(arrayCGH$arrayValues[[posVar]]>arrayCGH$arrayValues$CentroPos)
    arrayCGH$arrayValues[[chrArmVar]][ind] <- "q"

    if (length(indNA)<dim(arrayCGH$arrayValues)[1])
        arrayCGH$arrayValues[[chrArmVar]][-indNA] <- paste(arrayCGH$arrayValues[[chrVar]][-indNA], arrayCGH$arrayValues[[chrArmVar]][-indNA], sep="")
    else
        arrayCGH$arrayValues[[chrArmVar]] <- paste(arrayCGH$arrayValues[[chrVar]], arrayCGH$arrayValues[[chrArmVar]], sep="")
    arrayCGH
}
