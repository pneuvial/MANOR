## 'flag' objects: class and methods

## Copyright (C) 2004 Institut Curie
## Author(s): Pierre Neuvial (Institut Curie) 2004
## Contact: manor@curie.fr


### apply flag function to an arrayCGH
flag.arrayCGH <- function(flag, arrayCGH)
{
    formals(flag$FUN) <- c(alist(arrayCGH=), flag$args)
    if(!is.null(flag$char))
        ## flag FUN returns a list of spots
    {
        w <- flag$FUN(arrayCGH)
        if (flag$type=="perm.flag") ## permanent flags
        {
            if (is.na(match("Flag", names(arrayCGH$arrayValues))))
                ## add field "Flag" if it does not exist yet
                arrayCGH$arrayValues$Flag <- rep("", dim(arrayCGH$arrayValues)[1])
            arrayCGH$arrayValues$Flag[w] <- flag$char
        }
        else                        ## temporary flags are also kept
        {
            if (is.na(match("FlagT", names(arrayCGH$arrayValues))))
                ## add field "FlagT" if it does not exist yet
                arrayCGH$arrayValues$FlagT <- rep("", dim(arrayCGH$arrayValues)[1])
            arrayCGH$arrayValues$FlagT[w] <- flag$char
        }
    }
    else
        ## flag.FUN returns a new arrayCGH object
        arrayCGH <- flag$FUN(arrayCGH)
    arrayCGH
}
