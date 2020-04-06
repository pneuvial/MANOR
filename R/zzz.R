.onAttach <- function(libname, pkgname) {
    ## Skip deprecation warning on CI environments
    if (any(nzchar(Sys.getenv(c("GITHUB_ACTIONS", "TRAVIS", "APPVEYOR"))))) {
      .Deprecated <- message
    }
    
    msg <- sprintf(
        "Package '%s' is deprecated and will be removed from Bioconductor
         version %s", pkgname, "3.12")
    .Deprecated(msg=paste(strwrap(msg, exdent=2), collapse="\n"))
}
