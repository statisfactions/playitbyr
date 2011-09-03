\name{getSoundParams}
\alias{checkSoundParams}
\alias{getSoundParams}
\title{Get and check currently possible sound parameters.}
\usage{
  getSoundParams(shapes = getShapeNames())

  checkSoundParams(paramnames, shapes = getShapeNames())
}
\arguments{
  \item{shapes}{The shapes to return the sound parameters
  of. The default is to get or check all currently
  available shapes (see \code{link{getShapeNames}}).}

  \item{paramnames}{A character vector of sound parameters
  to check for validity}
}
\value{
  \code{getSoundParams} returns \code{data.frame} with the
  name of each parameter and the name of the shape it is a
  part of. \code{checkSoundParams} returns \code{NULL} if
  successful.
}
\description{
  Gets all supported sound parameters, or checks a
  character vector of sound parameters for validity. This
  is used by \code{\link{sonaes}} and
  \code{\link{sonscaling}} to provide a minimal check that
  the names of their arguments are valid.
}
\keyword{internal}

