\name{sonopts}
\alias{sonopts}
\title{Sonification rendering options}
\usage{
  sonopts(...)
}
\arguments{
  \item{\dots}{Named parameters for setting rendering
  options. See Details.}
}
\description{
  Set advanced options for a sonification
}
\details{
  Use this function if you want to modify a few settings
  for a single sonification.

  The following options are available, which mirror the
  input arguments to \code{\link{createPerformance}} from
  the \pkg{csound} package.  Note that most of these will
  be \strong{ignored} for many shapes, since they set these
  options themselves on the back end.

  \describe{ \item{i}{Usually this is not specified, since
  this will replace any intended sonification. A list of
  \code{matrix} objects. Each \code{matrix} is not
  specified in \code{sonopt} since these values are usually
  created and passed by \pkg{playitbyr} itself. the
  instructions for a single instrument. Each row of the
  \code{matrix} is an \code{i} statement, which instructs
  Csound to make an instrument active at a specific time
  and for a certain duration, and with certain parameters
  (p-fields). These p-fields are interpreted in the order
  of the columns of the \code{matrix}.}

  \item{f}{Not used for \code{"built-in.orc"}. A list of
  numeric vectors; these create the function tables Csound
  uses for oscillators and various other uses.}

  \item{orcfile}{The path of the orchestra file to be used
  for the performance. If this equals
  \code{"built-in.orc"}, the default, the orchestra
  included with this package will be used (see
  \code{\link{scoreMatrices}} for more details of using the
  built-in instruments.)}

  \item{scorefile}{The path of the score file, if any, to
  be used for the performance. The whole purpose of this
  function is to feed the score statements to Csound and
  bypass the need for score files, but this option is
  provided in any case.}

  \item{out}{String representing where to send output
  sound; the default, \code{"dac"}, indicates to send it
  your computer's sound output. If you want to render a
  file, enter the path to the (WAV) file you want.}

  \item{realTime}{Indicates whether the performance is to
  be rendered in real time. If you are rendering to a file,
  you probably want this as \code{FALSE}, since it can
  render a whole lot faster than real-time to file.}

  \item{finishPerformance}{Should the performance be closed
  after completing the score? If \code{TRUE}, the default,
  cleans up and closes Csound. If \code{FALSE}, returns a
  pointer to a Csound instance that can be used to continue
  the performance or eventually close it.}

  \item{suppressDisplays}{Csound by default pops up with
  annoying graphical widgets. This alloys you to suppress
  them (the default).}

  \item{moreflags}{A character vector of extra command-line
  flags to pass to Csound upon compilation of the
  orchestra. See
  \href{http://www.csounds.com/manual/html/CommandFlagsCategory.html}{The
  Csound Manual's page on the Csound command-line
  options}.}

  \item{csInstance}{An instance of Csound that can be used
  to continue or close the current performance.} }
}
\note{
  By default, a rendering is saved to a file and then
  immediately played for compatibility with slower systems.
  If you have a faster computer and want to play the
  sonification as you render it, you can set
  \code{options("render_real_time" = TRUE)}. (It is
  \code{FALSE} by default.)
}

