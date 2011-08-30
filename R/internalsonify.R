##' Internal functions related to layering and rendering
##' 
##' These all will eventually will be used as arguments to \code{sonify()} and
##' represent layering functionality that is not yet actually implemented.
##' 
##' @name internalsonify
##' @rdname internalsonify
##' @aliases rendering .checkRendering
##' @keywords internal
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

##' @rdname internalsonify
.checkData <- function(x) {
  ## Checks if sonify object or sonlayer x has an invalid data.frame
  ## It is okay for x$data to be null since one of either the sonify
  ## object or the layers can have NULL data frame.
  ## Called by 'sonify()' and 'sonlayer()'

  if(!is.null(x$data) & !is.data.frame(x$data))
    stop("'data' must be a data.frame.")
}

##' @rdname internalsonify
rendering <- function(x) {
  ## This function exists ENTIRELY for changing rendering
  ## in interactive use via '+.sonify';
  ## all it does is check the validitiy of its string argument
  ## and give it the class "sonrendering"
  ## (so that '+.sonify' knows what to do with it)
  
  .checkRendering(x)
  class(x) <- "sonrendering"
  x
}
