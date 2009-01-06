## sort clone-level information by genome order

## Copyright (C) 2004 Institut Curie
## Author(s): Pierre Neuvial (Institut Curie) 2004
## Contact: manor@curie.fr


sort.arrayCGH <- function(x, decreasing = FALSE, position.var="Position", chromosome.var="Chromosome", ...) {
  arrayCGH <- x
  chrBreak.var <- "LimitChr"
  posOrder.var <- "PosOrder"
  chrNum.var <- paste(chromosome.var, "num", sep=".")
  
  chr <- chrom(arrayCGH$cloneValues[[chromosome.var]])
  or <- order(chr, arrayCGH$cloneValues[[position.var]])
  arrayCGH$cloneValues[[chrNum.var]] <- chr
  arrayCGH$cloneValues <- arrayCGH$cloneValues[or,]
  
  ## only keep clones with known position
  arrayCGH$cloneValues <- arrayCGH$cloneValues[which(arrayCGH$cloneValues[[chrNum.var]]>0), ]
  l <- dim(arrayCGH$cloneValues)[1]
  ## position order of each clone along the genome
  arrayCGH$cloneValues[[posOrder.var]] <- 1:l
  
  ## flag last clone of each chromosome for genomic representation
  chr <- arrayCGH$cloneValues[[chrNum.var]]
  w <- which(chr[1:(l-1)]!=chr[2:l])
  arrayCGH$cloneValues[[chrBreak.var]] <- 0
  arrayCGH$cloneValues[[chrBreak.var]][w] <- 1
  
  arrayCGH
}
