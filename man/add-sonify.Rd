\name{+.sonify}
\alias{+.sonify}
\title{Add additional layers or options onto sonify objects}
\usage{
  \method{+}{sonify} (x, y)
}
\arguments{
  \item{x}{A \code{sonify} object}

  \item{y}{A \code{\link{sonlayer}} (such as
  \code{\link{shape_scatter}}), \code{\link{sonscaling}},
  \code{\link{sonaes}}, or \code{\link{sonopts}} object.}
}
\value{
  A sonify object with the relevant \code{y} value added
  into the object.
}
\description{
  \code{sonify} objects can be incrementally added onto to
  form a complete sonification, much like \code{ggplot}
  objects
}
\seealso{
  \code{\link{sonify}} for the creation of these objects
}

