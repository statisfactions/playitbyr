\name{print.sonify}
\alias{print.sonify}
\title{Print method for \code{sonify} objects}
\usage{
  \method{print}{sonify} (x, ...)
}
\arguments{
  \item{x,}{A \code{sonify} object}

  \item{\dots}{The additional argument
  \code{render_real_time} can be specified here; by
  default, option \code{"render_real_time"} is used, but
  the choice here overrides that option.

  The additional argument \code{non_real_time_test}, if
  included, allows testing of the csound player.}
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

