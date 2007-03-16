## Write an arrayCGH object to a text file

## Copyright (C) 2004 Institut Curie
## Author(s): Pierre Neuvial (Institut Curie) 2004
## Contact: manor@curie.fr

arrayCGH2txt <- function(arrayCGH, dir, filename, sep="\t")
{
    if (!file.exists(dir))
        dir.create(dir)
    file.list <- names(arrayCGH)
    for(i in 1:length(file.list))
    {
        d <- arrayCGH[[file.list[i]]]
        if (class(d)=="data.frame")
            write.table(d, file=paste(dir, "/", file.list[i], filename, sep=""), sep=sep, row.names=FALSE, col.names=gsub("\\.", "_", names(d)), quote=FALSE)
    }
}
