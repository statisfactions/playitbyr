##' Print method for \code{sonify} objects
##' 
##' Printing a sonify object renders it to sound (analogously to how
##' printing \code{ggplot} objects renders them to screen in the
##' \href{http://had.co.nz/ggplot2}{ggplot2} package).
##' 
##'
##' @aliases print.sonify
##' @param x, A \code{sonify} object
##' 
##' @param \dots The additional argument \code{render_real_time} can
##' be specified here; by default, option \code{"render_real_time"} is
##' used, but the choice here overrides that option.
##'
##' The additional argument \code{non_real_time_test}, if included, allows
##' testing of the csound player. 
##' 
##' @return \code{print.sonify} is called for its side-effect, which
##' is to actually render the object to a sound. It does, however,
##' return the length of the resulting sonification.
##' 
##' @seealso \code{\link{sonify}} for the creation of these objects
##' @method print sonify
##' @export
print.sonify <- function(x, ...) {
  dots <- list(...)
  opts <- x$opts
  
  if("render_real_time" %in% names(dots)) 
    realtime <- dots$render_real_time
  else
    realtime <- getOption("render_real_time")

  if("play" %in% names(dots))
    play <- dots$play
  else
    play <- TRUE


  
  if(!realtime) {
    if("file" %in% names(dots))
      outfile <- dots$file
    else
      outfile <- tempfile()

    if("out" %in% names(dots))
      out <- dots$out
    else
      out <- "dac"
    
      
    length <- render(.getScore(x), opts = x$opts, file = outfile)
    if("play" %in% names(dots) & dots$play == TRUE)
      createPerformance(i = list(matrix(c(3, 0, length,
                        paste("\"", outfile, "\"", sep  = "")),
                        nrow = 1)), out = out,
                                orcfile = system.file("orc/playitbyr.orc", package = "playitbyr"),
                      realTime = FALSE)
  } else {
    length <- render(.getScore(x), opts = x$opts)
  }
  invisible(length)
}
