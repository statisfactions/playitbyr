##' Sonification rendering options
##'
##' Set options for a single sonification
##'
##' Use this function if you want to modify a few settings for a
##' single sonification.
##'
##' When \code{rendering = "csound"}, the default, the following
##' options are available, which mirror the input arguments to
##' \code{\link{createPerformance}} from the \pkg{csound} package:
##'
# Parameter list imported from createPerformance on Friday, December 16, 2011
##' \describe{
##' \item{i}{Usually this is not specified, since this will
##' replace any intended sonification. A list of \code{matrix}
##' objects. Each \code{matrix} is not specified in \code{sonopt}
##' since these values are usually created and passed by
##' \pkg{playitbyr} itself. the instructions for a single
##' instrument. Each row of the \code{matrix} is an \code{i}
##' statement, which instructs Csound to make an instrument active at
##' a specific time and for a certain duration, and with certain
##' parameters (p-fields). These p-fields are interpreted in the order
##' of the columns of the \code{matrix}.}
##'
##' \item{f}{Not used for \code{"built-in.orc"}. A list of numeric
##' vectors; these create the function tables Csound uses for
##' oscillators and various other uses.}
##'
##' \item{orcfile}{The path of the orchestra file to be used for the
##' performance. If this equals \code{"built-in.orc"}, the default,
##' the orchestra included with this package will be used (see
##' \code{\link{scoreMatrices}} for more details of using the built-in
##' instruments.)}
##'
##' \item{scorefile}{The path of the score file, if any, to be used
##' for the performance. The whole purpose of this function is to feed
##' the score statements to Csound and bypass the need for score
##' files, but this option is provided in any case.}
##'
##' \item{out}{String representing where to send output sound; the
##' default, \code{"dac"}, indicates to send it your computer's sound
##' output. If you want to render a file, enter the path to the (WAV)
##' file you want.}
##'
##' \item{realTime}{Indicates whether the performance is to be
##' rendered in real time. If you are rendering to a file, you
##' probably want this as \code{FALSE}, since it can render a whole
##' lot faster than real-time to file.}
##'
##' \item{finishPerformance}{Should the performance be closed after completing
##' the score? If \code{TRUE}, the default, cleans up and
##' closes Csound. If \code{FALSE}, returns a pointer to a
##' Csound instance that can be used to continue the performance or
##' eventually close it.}
##'
##' \item{suppressDisplays}{Csound by default pops up with annoying
##' graphical widgets. This alloys you to suppress them (the default).}
##'
##' \item{moreflags}{A character vector of extra command-line flags to
##' pass to Csound upon compilation of the orchestra. See
##' \href{http://www.csounds.com/manual/html/CommandFlagsCategory.html}{The
##' Csound Manual's page on the Csound command-line options}.}
##'
##' \item{csInstance}{An instance of Csound that can be used to
##' continue or close the current performance.}
##'
##' If \code{rendering = "audio"}, there is only one parameter to identify:
##'
##' \item{samp.rate}{The sampling rate in Hertz. For instance, CD
##' quality is 44100 Hertz.}
##' }
##' @param rendering The rendering type. Only \code{"audio"} and
##' \code{"csound"} are currently supported.
##' @param \dots Additional named parameters for setting rendering
##' options. See Details.
##' @export
sonopts <- function(rendering = "csound", ...) {
  out <- list(...)

  ## Check parameters valid
  if(rendering %in% "csound")
    mismatch <- names(out)[!(names(out) %in% names(formals(createPerformance)))]
  else if(rendering %in% "audio")
    mismatch <- names(out)[!(names(out) %in% "samp.rate")]
  else stop("rendering must be 'csound' or 'audio'")
  
  if(length(mismatch)>0)
    stop("Unrecognized ", rendering, " parameters ",
         paste(paste("'", mismatch, "'", sep = ""), collapse = ", "))

  out <- c(rendering = rendering, out)
  class(out) <- "sonopts"
  return(out)
}


