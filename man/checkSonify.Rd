\name{checkSonify}
\alias{checkSonify}
\title{Check if a \code{sonify} object can be rendered}
\usage{
  checkSonify(x)
}
\arguments{
  \item{x}{A \code{sonify} object.}
}
\description{
  This function is called by \code{\link{print.sonify}}
  before rendering to check if the object can be rendered;
  it can also be called directly by the user for diagnostic
  purposes.
}
\seealso{
  \code{\link{sonify}}, \code{\link{sonaes}},
  \code{\link{sonscaling}} all give more details on how to
  correctly form a \code{sonify} object.
}

