\name{linear.scale}
\alias{linear.scale}
\usage{linear.scale(x, min, max)}
\title{Create a linear parameter scaling with given minimum and
  maximum}
\description{\code{linear.scale()} linearly transforms its input vector \code{x}
  so that its minimum value is \code{min} and its maximum value is
  \code{max}. This is for use as a scaling function in \code{\link{sonscaling}}}
\arguments{
  \item{x}{A numeric vector or matrix}
  \item{min}{The desired minimum value, a \code{numeric} of length 1}
  \item{max}{The desired maximum value, a \code{numeric} of length 1}
}
\value{
  A numeric vector or matrix of the same type as \code{x}, linearly
  rescaled in the desired way.}
\seealso{
  \code{\link{sonscaling}}, \code{\link{scaleShortcuts}}

