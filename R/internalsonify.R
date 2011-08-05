.checkRendering <- function(x) {
  ## Checks whether input string x is a valid rendering type
  ## This is a dopey function for now, but is likely to become
  ## more complex as more rendering types are supported,
  ## some of which may even be machine or architecture dependent.
  ## This is called by 'rendering()' and will be called by
  ## 'sonify()' once there are any non-audio rendering options.
  
  if(!(x %in% c("audio")))
    stop("'",x, "' is not a valid rendering")
  if(length(x) > 1 | !is.character(x))
    stop("Renderings must be a character vector of length one.")
  
}

.checkData <- function(x) {
  ## Checks if sonify object or sonlayer x has an invalid data.frame
  ## It is okay for x$data to be null since one of either the sonify
  ## object or the layers can have NULL data frame.
  ## Called by 'sonify()' and 'sonlayer()'

  if(!is.null(x$data) & !is.data.frame(x$data))
    stop("'data' must be a data.frame.")
}
