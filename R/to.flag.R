## 'flag' objects: class and methods

## Copyright (C) 2004 Institut Curie
## Author(s): Pierre Neuvial (Institut Curie) 2004
## Contact: manor@curie.fr


### create a flag object
to.flag <- function(FUN, char=NULL, args=NULL, type="perm.flag", label=NULL)
{
    f <- list(FUN=FUN, char=char, args=args, type=type, label=label)
    class(f) <- "flag"
    f
}

## ajouter un controle:
##   (char==NULL) => type de retour de FUN=arrayCGH
##   (char!=NULL) => type de retour de FUN=liste de spots

print.flag <- function(x, ...)
{
    f <- x
    cat("Label\n")
    cat("\t", f$label, "\n\n")
    cat("Type\n")
    cat("\t", f$type, "\n\n")
    cat("Char\n")
    cat("\t", f$char, "\n\n")
    cat("Function\n\t")
    print(f$FUN)
    if(!is.null(f$args))
    {
        cat("\nArgs\n")
        for (i in 1:length(f$args))
            cat("\t", names(f$args)[i], ":\t", f$args[[i]], "\n")
        cat("\n")
    }
}