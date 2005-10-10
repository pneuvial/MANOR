## 'qscore' objects: class and methods

## Copyright (C) 2004 Institut Curie
## Author(s): Pierre Neuvial, Isabel Brito (Institut Curie) 2004
## Contact: manor@curie.fr


### create a qscore object
to.qscore <- function(FUN, name=NULL, args=NULL, label=NULL, dec=3)
  {
    qs <- list(FUN=FUN, name=name, args=args, label=label, value=NULL, dec=dec)
    class(qs) <- "qscore"
    qs
  }

### compute quality scores for a given arrayCGH
qscore.arrayCGH <- function(qscore, arrayCGH) {
  formals(qscore$FUN) <- c(alist(arrayCGH=), qscore$args)
  qscore$FUN(arrayCGH)
}

qscore.summary.arrayCGH <- function(arrayCGH, qscore.list) {
  score <- NULL
  name <- NULL
  label <- NULL
  for (i in 1: length(qscore.list)) {
    qs <- qscore.list[[i]]
    dec <- qs$dec
    score <- c(score, round(qscore.arrayCGH(qs, arrayCGH)*10^dec)/(10^dec))
    name <- c(name, qs$name)
    label <- c(label, qs$label)
  }
  data.frame(name=name, label=label, score=score)
}
