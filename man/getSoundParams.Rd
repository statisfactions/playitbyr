\name{getSoundParams}
\alias{checkSoundParams}
\alias{getSoundParams}
\title{Get and check currently possible sound parameters.}
\usage{
  getSoundParams(shape = "any")

  checkSoundParams(paramnames, shape = "any")
}
\arguments{
  \item{shape}{The shape to return the sound parameters of.
  The default, \code{"any"}, returns sound parameters of
  all shapes.}

  \item{paramnames}{A character vector}
}
\value{
  \code{getSoundParams} returns a character vector with the
  name of each parameter. \code{checkSoundParams} returns
  \code{NULL} if successful.
}
\description{
  Gets all supported sound parameters, or checks a
  character vector of sound parameters for validity. This
  is used by \code{\link{sonaes}} and
  \code{\link{sonscaling}} to provide a minimal check that
  the names of their arguments are valid.
}
\keyword{internal}

