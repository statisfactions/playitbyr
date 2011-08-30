##' Convert from csound-style oct notation for pitches to frequency in Hertz
##' 
##' This function converts from the
##' \href{http://www.csounds.com/manual/html/cpsoct.html}{\sQuote{oct} notation}
##' notation for pitches, where 8 is middle C and 1 represents an octave, to
##' the corresponding frequency in Hertz.
##' 
##' Most users of \code{playitbyr} will not need this function, but those
##' wishing to make add-ons may find it useful.
##' 
##' The conversion to Hertz is quite simple:
##' 
##' \preformatted{440*2^(oct-8.75)}where \code{oct} is the input value. Note
##' that both 440 Hz and 8.75 are tuning A.
##' 
##' @param oct A \code{numeric} of length 1, which is a pitch in \sQuote{oct}
##' notation
##' @return A \code{numeric} value of length 1, which is a frequency in Hertz.
##' @seealso \code{\link{sonaes}}
##' @examples
##' 
##' ## In oct, middle c is 8
##' octToFreq(8)
##' 
##' ## An octave below tuning A, in Hz
##' octToFreq(7.75)
##' 
##' ## A little more than that, in Hz
##' octToFreq(7.76)
##'
##' @export
octToFreq <- function(oct) {
  ## Converts "oct" notation to an actual frequency
  440*2^(oct-8.75)
}
