\name{print.sonify}
\alias{print.sonify}
\title{Print method for \code{sonify} objects}
\usage{
  \method{print}{sonify} (x, ...)
}
\arguments{
  \item{x}{A \code{sonify} object}

  \item{\dots}{Additional optional arguments:

  \describe{ \item{\code{render_real_time}}{Render the
  sonification in real time? If \code{TRUE}, the
  sonification is rendered in real time (which is faster
  but may not work as well on slower computers); if
  \code{FALSE} the sonification is rendered to a file on
  disk before playing. The default behavior is given by the
  option \dQuote{\code{render_real_time}}.}

  \item{\code{file}}{The file to render to (if
  \code{render_real_time = FALSE}). The default is a
  temporary file.}

  \item{\code{play}}{If rendering to a file, should the
  file then be played? Default is \code{TRUE}.}

  \item{\code{playout}}{If playing from a file, what
  channel should be used for output? This string is passed
  to the Csound \code{-o} option. Default is \code{"dac"},
  the default audio out.} }}
}
\value{
  \code{print.sonify} is called for its side-effect, which
  is to actually render the object to a sound. It invisibly
  returns the length of the resulting sonification.
}
\description{
  Printing a sonify object renders it to sound (analogously
  to how printing \code{ggplot} objects renders them to
  screen in the \href{http://had.co.nz/ggplot2}{ggplot2}
  package).
}
\details{
  In interactive use, you can simply type the name of the
  \code{sonify} object to sonify it. In loops, functions,
  and \code{\link{source}}-ing use, however, you need to
  explicitly call print by using \code{print(x)} to print
  the object \code{x}.
}
\note{
  By default, a rendering is saved to a file and then
  immediately played for compatibility with slower systems.
  If you have a faster computer and want to play the
  sonification as you render it, you can set
  \code{options("render_real_time" = TRUE)}. (It is
  \code{FALSE} by default.)
}
\seealso{
  \code{\link{sonify}} for the creation of these objects,
  \code{\link{sonsave}} for a convenience function that
  saves a sound file, \code{\link{sonopts}} for advanced
  and low-level rendering options
}

