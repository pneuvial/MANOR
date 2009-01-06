## 'flag.summary': class and methods

## Copyright (C) 2004 Institut Curie
## Author(s): Pierre Neuvial (Institut Curie) 2004
## Contact: manor@curie.fr

flag.summary <- function(...)
  UseMethod("flag.summary")

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

    if (length(flag.list)) {
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
        if (is.null(flab) || (sum(is.na(flab))==length(flab))) ## no flag labels are provided
            ##    df <- data.frame(char=n, arg=I("-"), count=0)
            df <- data.frame(char=n, arg=NA, count=0)
        else                  ## flag labels are provided
            ##    df <- data.frame(char=n, label=c(flab, "not flagged"), arg=I("-"), count=0)
            df <- data.frame(char=n, label=I(c(flab, nflab)), arg=NA, count=0)

        m <- match(names(farg), n)
        if (length(m)) {
            for (i in 1:length(m))
                if (!is.na(farg[i]))
                    df$arg[m[i]] <- farg[i]
        }
        m <- match(names(fcount), n)
        if (length(m)) {
            for (i in 1:length(m))
                df$count[m[i]] <- fcount[i]
        }
        df
    }
}

