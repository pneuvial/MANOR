## normalization of an object of type arrayCGH

## Copyright (C) 2004 Institut Curie
## Author(s): Pierre Neuvial (Institut Curie) 2004
## Contact: manor@curie.fr

norm <- function(...)
    UseMethod("norm")

norm.arrayCGH <- function(arrayCGH, flag.list=NULL, var="LogRatio", printTime=FALSE, FUN=median, ...) {
    var.norm <- paste(var, "Norm", sep="")
    arrayCGH$arrayValues[[var.norm]] <- arrayCGH$arrayValues[[var]]
    
    ## apply flag list to arrayCGH
    if (is.null(arrayCGH$arrayValues$Flag))
        arrayCGH$arrayValues$Flag <- rep("", dim(arrayCGH$arrayValues)[1])
    if (is.null(arrayCGH$arrayValues$FlagT))
        arrayCGH$arrayValues$FlagT <- rep("", dim(arrayCGH$arrayValues)[1])
    temp.flags <- NULL ## list of 'temporary flags'
    if (length(flag.list))
        for (i in 1: length(flag.list))
        {
            f <- flag.list[[i]]
            print(names(flag.list)[[i]])
            if (printTime)
                t1 <- Sys.time()
            arrayCGH <- flag.arrayCGH(f, arrayCGH) ## compute flags of the list
            if (printTime)
                print(Sys.time()-t1)
            if (f$type=="temp.flag")
                temp.flags <- c(temp.flags, f$char)
        }
    ##    print(summary.factor(arrayCGH$arrayValues$Flag))
    ##    print(summary.factor(arrayCGH$arrayValues$FlagT))

### compute normalization coefficient
    ## exclude all flags from computation (including temporary flags)
    ##    w <- which(arrayCGH$arrayValues$Flag=="")
    w <- which((arrayCGH$arrayValues$Flag=="")&(arrayCGH$arrayValues$FlagT==""))

    stat <- arrayCGH$arrayValues[[var.norm]]
    stat[-w] <- NA
    if (!is.null(arrayCGH$cloneValues))
    {
        id.rep <- arrayCGH$id.rep
        rep <- arrayCGH$arrayValues[[id.rep]]
        ## mean of un-flagged replicates
        clone.stat <- tapply(stat, rep, mean, na.rm=TRUE)
        ##        print(summary(as.numeric(clone.stat)))
        m <- FUN(as.numeric(clone.stat), ...)                              ## apply normalization function to clone-level values
    }
    else
        m <- FUN(stat, ...)                                      ## apply normalization function to spot-level values
    arrayCGH$arrayValues[[var.norm]] <- arrayCGH$arrayValues[[var.norm]]-m

    ## flagged spots are NA-ed ('temp flags' have been removed, so they are not NA-ed !)
    arrayCGH$arrayValues[[var.norm]][which(arrayCGH$arrayValues$Flag!="")] <- NA

### add clone-level information
                                        #    w <- which(arrayCGH$arrayValues$Flag=="")
                                        #    stat <- arrayCGH$arrayValues[[var.norm]]
                                        #    stat[-w] <- NA
    stat <- arrayCGH$arrayValues[[var.norm]]
    if (!is.null(arrayCGH$cloneValues))
    {
        ##  update arrayCGH$cloneValues with aggregated information about flags and variable of interest
        id.rep <- arrayCGH$id.rep
        rep <- arrayCGH$arrayValues[[id.rep]]
        ## mean of un-flagged replicates
        clone.flag <- as.character(tapply(arrayCGH$arrayValues$Flag, rep, flag.aggregate.arrayCGH))
        clone.flag[clone.flag==""] <- "OK"                             ## un-flagged clones are flagged 'OK'
        clone.flagT <- as.character(tapply(arrayCGH$arrayValues$FlagT, rep, flag.aggregate.arrayCGH)) ## temp. flags...
        clone.stat <- tapply(stat, rep, mean, na.rm=TRUE)
        nb <- tapply(stat, rep, function(x) {sum(!is.na(x))} )         ## number of replicates before normalisation
        nb0 <- tapply(stat, rep, function(x) {length(x)} )             ## number of replicates after normalisation
        clone.add <- data.frame(names(clone.stat), as.numeric(clone.stat), as.numeric(nb), as.numeric(nb0), I(clone.flag), I(clone.flagT))
        names(clone.add) <- c(id.rep, var, "rep", "rep0", "FlagAgr", "FlagT")
        arrayCGH$cloneValues <- merge(arrayCGH$cloneValues, clone.add, by.x=id.rep, by.y=id.rep)
    }

### add spot-level information
    arrayCGH$arrayValues$Flag[which(arrayCGH$arrayValues$Flag=="")] <- "OK"  ## un-flagged spots are flagged "OK"

### Add flag information to arrayCGH
    ##    arrayCGH$flags <- c(arrayCGH$flags, flag.list)
    if (length(flag.list))
        arrayCGH$flags <- flag.summary(arrayCGH, flag.list)

### Add normalization coefficient to arrayCGH
    arrayCGH$norm.coef <- m

    arrayCGH
}
