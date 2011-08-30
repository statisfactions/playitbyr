\name{linear.scale}
\alias{linear.scale}
\title{Create a linear parameter scaling with given minimum and maximum}
\usage{
  linear.scale(x, min, max)
}
\arguments{
  \item{x}{A numeric vector or matrix}

  \item{min}{The desired minimum value, a \code{numeric} of
  length 1}

  \item{max}{The desired maximum value, a \code{numeric} of
  length 1}
}
\value{
  A numeric vector or matrix of the same type as \code{x},
  linearly rescaled in the desired way.
}
\description{
  \code{linear.scale()} linearly transforms its input
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
\examples{
x <- 1:10
linear.scale(x, min=-2, max=-1.5)

## If max<min, it's rescaled in reverse:
linear.scale(x, min=10, max=1)
}
\seealso{
  \code{\link{sonscaling}}, \code{\link{scaleShortcuts}}
}

