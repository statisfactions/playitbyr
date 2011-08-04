linear.scale <- function(x, min, max) {
  ## Linearly rescales vector x so that "lower" is the minimum
  ## and "upper" the maximum

  if(min>max) {
    ## Allow for reversed polarity
    x <- -x
    oldmin <- min
    oldmax <- max
    min <- oldmax
    max <- oldmin
  }
  
  nrange <- abs(max-min)
  out <- ((x-min(x))*nrange/(max(x)-min(x)) + min)
  out
}
