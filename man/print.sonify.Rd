\name{print.sonify}
\alias{print.sonify}
\title{Print method for \code{sonify} objects}
\usage{
  \method{print}{sonify} (x, ...)
}
\arguments{
  \item{x,}{A \code{sonify} object}

  \item{\dots}{Additional optional arguments:

  \code{render_real_time}: Render the sonification in real
  time? If \code{TRUE}, the sonification is rendered in
  real time (which is faster but may not work as well on
  slower computers); if \code{FALSE} the sonification is
  rendered to a file on disk before playing. The default
  behavior is given by the option \code{"render_real_time"}

  \code{file}: The file to render to (if
  \code{render_real_time = FALSE}). The default is a
  temporary file.

  \code{play}: If rendering to a file, should the file then
  be played? Default is \code{TRUE}.

  \code{playout}: If playing from a file, what channel
  should be used for output? Default is "dac", the default
  audio out (detected by Csound).}
}
\value{
  \code{print.sonify} is called for its side-effect, which
  is to actually render the object to a sound. It does,
  however, return the length of the resulting sonification.
}
\description{
  Printing a sonify object renders it to sound (analogously
  to how printing \code{ggplot} objects renders them to
  screen in the \href{http://had.co.nz/ggplot2}{ggplot2}
  package).
}
\seealso{
  \code{\link{sonify}} for the creation of these objects
}

