\name{scale_mod_continuous}
\alias{scale_mod_continuous}
\alias{scale_mod_exp}
\title{Continuous modulation multiplier scales}
\usage{
  scale_mod_continuous(soundlimits, limits = NULL, ...)

  scale_mod_exp(soundlimits, limits = NULL, ...)
}
\arguments{
  \item{\dots}{Other parameters (currently ignored)}
}
\value{
  A \code{sonscaling} object, to be used in \code{sonify}
  or added onto a \code{sonify} object.
}
\description{
  These shortcut functions (with the notation
  \code{scale_}\kbd{[sound parameter]}\code{_}\kbd{[scaling
  function]}) are intended to be added onto a \code{sonify}
  object to specify how \kbd{[data parameter]} from the
  data (a column of the data) is to be mapped to sonic
  parameters (here, \code{"mod"}, the modulation frequency
  as a multiple of the carrier frequency).
}
\details{
  \code{_continuous} scales are a linear scaling, whereas
  \code{_exp} is an exponential scale.
}
\seealso{
  \code{\link{sonscaling}} for the full syntax and range of
  possibilities for specifying scaling of \code{sonify}
  objects; \code{\link{linear_scale}}
  \code{\link{exp_scale}} for the pre-defined scaling
  functions which this incorporates; \code{\link{+.sonify}}
  for the addition onto \code{sonify} objects.
}

