\name{+.sonify}
\alias{+.sonify}
\title{Add additional layers or options onto sonify objects}
\usage{
  \method{+}{sonify} (x, y)
}
\arguments{
  \item{x}{A \code{sonify} object}

  \item{y}{An object to be added into the sonify object
  (see Details).}
}
\value{
  A sonify object with the relevant \code{y} value added
  into the object.
}
\description{
  \code{\link{sonify}} objects can be incrementally added
  onto to form a complete sonification, much like
  \code{ggplot} objects.
}
\details{
  Adding mappings, layers, facets, and other parameters
  onto a \code{sonify} object is the easiest method of
  creating a sonification. The following kinds of objects
  can be added:

  \describe{

  \item{\code{\link{sonaes}}}{Mappings of data columns to
  sound parameters}

  \item{\code{\link{sonlayer}}}{Layers, such as
  \code{\link{shape_scatter}}}

  \item{\code{\link{sonscaling}}}{Scalings, such as
  \code{\link{scale_time_continuous}}}

  \item{\code{\link{sonfacet}}}{Instructions for faceting
  the sonification}

  \item{\code{\link{sonopts}}}{Additional options}

  }
}
\seealso{
  \code{\link{sonify}} for the creation of these objects
}

