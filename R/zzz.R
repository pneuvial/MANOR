## Interaction with C

## Copyright (C) 2004 Institut Curie
## Author(s): Pierre Neuvial (Institut Curie) 2004
## Contact: manor@curie.fr


.First.lib <- function(lib, pkg){
    library.dynam("MANOR", pkg, lib)
    if (!require(GLAD))
      stop("Please make sure GLAD is installed before installing MANOR !")
    print("Now entering MANOR...")
}
