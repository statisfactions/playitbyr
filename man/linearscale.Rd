\name{linear_scale}
\alias{linear_scale}
\title{Create a linear parameter scaling with given minimum and maximum}
\usage{
  linear_scale(x, limits = NULL, soundlimits, by = NULL)
}
\arguments{
  \item{x}{A numeric vector or matrix}

  \item{limits}{The limits of the data to train, a numeric
  vector of length 2. All data values outside these limits
  are returned as \code{NA}. If \code{NULL}, the default,
  the function takes the minimum and maximum of the data}

  \item{soundlimits}{The limits of the sound parameter.}

  \item{by}{The unit to round the sound parameter to. See
  examples.}
}
\value{
  A numeric vector or matrix of the same type as \code{x},
  linearly rescaled in the desired way. If \code{x} only
  has one value, \code{linear_scale} simply returns the
  midpoint between \code{min} and \code{max}.
}
\description{
  \code{linear_scale()} linearly transforms its input
  vector \code{x} so that its minimum value is \code{min}
  and its maximum value is \code{max}. This is for use as a
  scaling function in \code{\link{sonscaling}}. It is
  unlikely to be directly called by the user.
}
\details{
  This simple function does the important work of actually
  rescaling the given data of a \code{sonify} object into
  the range of the given sonic parameter, which is why it's
  included in \pkg{playitbyr}. But it can be used on any
  vector or matrix.
}
\note{
  If \code{by} is specified, unexpected results may happen
  when \code{soundlimits[2] != (soundlimits[1] + n * by)}
  for some \code{n}; in these cases the data will be
  trained to the largest value of \code{(soundlimits[1] + n
  * by)} less than \code{soundlimits[2]}.
}
\examples{
x <- 1:10
linear_scale(x, soundlimits = c(-2, -1.5))

## If max<min, it's rescaled in reverse:
linear_scale(x, soundlimits = c(10, 1))

## If 'limits' is specified, these are taken as the maximum and
## minimum of the data to be rescaled to
linear_scale(x, limits = c(-10, 10), soundlimits = c(10, 0))

## any values outside of 'limits' are therefore NA in result
linear_scale(x, limits = c(1, 2), soundlimits = c(1, 10))

## 'by' rounds values appropriately
linear_scale(x, soundlimits = c(1, 2), by = .25)
}
\seealso{
  \code{\link{sonscaling}}
}
\keyword{internal}

