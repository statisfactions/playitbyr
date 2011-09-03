\name{sonscaling}
\alias{sonscaling}
\title{Setting scales for \code{sonify} objects}
\usage{
  sonscaling(...)
}
\arguments{
  \item{\dots}{See Details section.}
}
\value{
  A \code{sonscaling} object that
}
\description{
  \code{sonscaling()} is invoked in the call to
  \code{sonify} or added onto it in order to specify
  precisely how the data parameters are mapped onto sonic
  parameters. Currently, only continuous scales are
  supported.
}
\details{
  Each argument of \code{sonscaling} is in the form of a
  list, where the first element is the minimum value of the
  sonic parameter, the second is the maximum, and the third
  is a function that maps the data column onto the range of
  the sonic parameter. The only such function included with
  \pkg{playitbyr} right now is \code{\link{linear.scale}}.

  \code{\link{scaleShortcuts}} provides a more intuitive
  interface to defining scales.
}
\seealso{
  \code{\link{sonify}} where this is eventually used;
  \code{\link{sonaes}} for defining the which data columns
  get mapped onto these sonic parameters;
  \code{\link{scaleShortcuts}} for easy shortcut functions
  for common use-cases of scaling; and
  \code{\link{linear.scale}} for an example of a scaling
  function.
}

