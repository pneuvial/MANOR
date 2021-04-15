# MANOR 1.59.5

 * Merge inst/NEWS to NEWS.md

# MANOR 1.59.4

 * Update broken links in vignette.

 * Update broken links in vignette.

 * Add BugReports link.

 * Add missing dependencies 'rmarkdown' and 'bookdown' to Suggests.
 

# MANOR 1.59.3

 * Package undeprecated by the Bioconductor maintainers.


# MANOR 1.59.2

 * Turned old .Rnw vignette to .Rmd vignette.
 
 * GLAD no more listed in Depends (only in Imports).
 
 * Moved to UTF-8 encoding (to cope with French accents).
 
 * Updated maintainer email address.
 
 * Fixed "register native routines and to disable symbol search".
 
 * Use R's RNG instead of the one from the system in C code.

# MANOR 1.37.2 (2014-10-01)

 * Removed unused C functions for I/O in 'lib_io'.

# MANOR 1.37.1 (2014-10-01)

 * Minor changes to C code to avoid WARNINGS upon R CMD check.
 
 * Changed 'par' settings in vignette to avoid 'Figure margins too large' errors.


# MANOR 1.33.1 (2013-07-03)

 * Replaced calls to 'exit', 'fprintf' which now raise a WARNING upon check.

 * Iteration index not printed anymore in 'nem' (raised an error when compiling the vignette).

# MANOR 1.23.3 (2011-03-18)

 * Updated calls to 'GLAD:::daglad' in the vignette to fix an error
caused by changes in the defaults of 'daglad'.

# MANOR 1.21.1 (2010-10-01)

 * Cleaned 'data/flags.RData" which contained objects from an old 'globalenv'.

 * Updated maintainer's email address.

# MANOR 1.19.1 (2010-01-24)

 * Added (back) 'intensity.flag' to 'data/flags.RData' (had been removed since v 1.12.0 for an unknown reason).

# MANOR 1.15.4 (2009-01-15)

 * misplaced alignment tab in man/spatial.Rd.

# MANOR 1.15.3 (2009-01-13)

 * updated references in .Rd files.

 * fixed warnings due to incorrect use of \item in .Rd files.

# MANOR 1.15.2 (2009-01-06)

 * 'norm' and 'sort' are now S3 methods as well

# MANOR 1.15.1 (2009-01-04)

 * (almost) one file per function in R/

 * removed empty section \details in man/qscore.Rd

 * added a NAMESPACE

 * removed inst/doc/Makefile (not needed anymore because no html output required)

# MANOR 1.15.1 (2009-01-02)

 * removed another non-standard keyword

# MANOR 1.15.1 (2009-01-01)

 * only one keyword per \keyword entry...

# MANOR 1.15.0 (2008-12-31)

 * now use standard "keyword"s

 * changed \link{\code{stuff}} into \code{\link{stuff}}

# MANOR 1.15.0 (2008-11-26)
 
 * filled in "keyword" sections in .Rd files.
 
 * removed empty "examples" sections from .Rd files.
 
 * initialized a few variables upon declaration in C code to prevent warnings in R CMD CHECK.

# MANOR 1.13.2 (2008-09-23)
 
 * modification de la fonction cv pour retourner NA lorsque toutes la valeurs du vecteur sont à NA
 
 * modification de la function getChromosomeArm pour que cytoband ne soit pas positionnée à NULL

# MANOR 1.13.1 (2008-09-04)

 * added a CHANGELOG

 * updated outdated reference in the .bib file

 * changed the definition of flag "rep.flag" to avoid the error now caused by sd(NA, na.rm=TRUE)
