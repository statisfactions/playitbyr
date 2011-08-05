## TODO check checkSonify's new functionality
## and sonlayer and all the new stuff...

sonify <- function(data=NULL, mapping=sonaes(), scales=sonscaling()) {
  ## Creates a \code{sonify} object, which is a list containing the \code{data.frame}
  ## to be sonified, the mappings of data to sound parameters, the scaling
  ## of parameters, and additional options.
  

####################TEMPORARY####################
  ## This code is a temporary hack in place of future functionality
  ## TODO Make as arguments in sonify() once there are other options
  sonlayers <- list(shape_notes())
  rendering <- "audio"
####################END##########################

  .checkRendering(rendering)
  dataname <- deparse(substitute(data)) # Used by summary.sonify()
  
  s <- list(data, dataname, mapping, rendering, scales, sonlayers) 
  names(s) <- c("data", "dataname", "mapping", "rendering", "scales", "sonlayers") 
  class(s) <- c(rendering, "sonify")    # The class of rendering determines the
                                        # function called to render s
  .checkData(s)
  s
}
