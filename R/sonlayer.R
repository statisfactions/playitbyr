sonlayer <- function(shape="notes", shape_params=NULL, stat=NULL,
                     stat_params=NULL, data=NULL, mapping=NULL) {
  ## TODO Let's only make live the parts of this we're actually supporting
  ## TODO man page
  ## TODO add to namespace

  dataname <- deparse(substitute(data)) # Used by summary.sonify()
  l <- list(list(shape, shape_params), list(stat, stat_params), data, dataname, mapping)
  
  names(l) <- c("shape", "stat", "data", "dataname", "mapping")
  names(l$stat) <- c("stat", "stat_params")
  names(l$shape) <- c("shape", "shape_params")
  class(l) <- c("sonlayer")

  .checkData(l)    
  l
}

shape_notes <- function(...) sonlayer("notes",...)
## TODO add to namespace
## TODO man page
## Convenience function for the only supported layer type, notes.
