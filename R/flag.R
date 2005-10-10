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

flag.summary.arrayCGH <- function(arrayCGH, flag.list, flag.var="Flag", nflab="not flagged", ...) {
  spot.flags <- arrayCGH$arrayValues[[flag.var]]
  if (is.null(spot.flags))
      stop(paste("Variable", flag.var, "not found in arrayCGH$arrayValues"))
  flag.summary(spot.flags, flag.list, nflab=nflab)
}

flag.summary.default <- function(spot.flags, flag.list, nflab="not flagged", ...) {
  fchar <- NULL ## vector of flag names
  farg <- NULL ## vector of flag arguments
  flab <- NULL ## vector of flag labels

  for (i in 1:length(flag.list)) {
    f <- flag.list[[i]]
    nul <- is.null(f$char)
    num <- is.numeric(f$args[[1]])
    tmp <- (f$type=="temp.flag")
    if ((!nul)&&((num)||(!tmp))) {
      fchar <- c(fchar, f$char)
      if (num)
        farg <- c(farg, f$args[[1]])
      else
        farg <- c(farg, NA)
      if (!is.null(f$label))
        flab <- c(flab, f$label)
      else
        flab <- c(flab, NA)
    }
  }
  names(farg) <- fchar
  fcount <- summary.factor(spot.flags)
  n <- c(fchar, "OK")
  if (sum(is.na(flab))==length(flab)) ## no flag labels are provided
##    df <- data.frame(char=n, arg=I("-"), count=0)
    df <- data.frame(char=n, arg=NA, count=0)
  else                  ## flag labels are provided
##    df <- data.frame(char=n, label=c(flab, "not flagged"), arg=I("-"), count=0)
    df <- data.frame(char=n, label=I(c(flab, nflab)), arg=NA, count=0)

  m <- match(names(farg), n)
  for (i in 1:length(m))
    if (!is.na(farg[i]))
      df$arg[m[i]] <- farg[i]

  m <- match(names(fcount), n)
  for (i in 1:length(m))
    df$count[m[i]] <- fcount[i]

  df
}

flag.summary <- function(...)
  UseMethod("flag.summary")
