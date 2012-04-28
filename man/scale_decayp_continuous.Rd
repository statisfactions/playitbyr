\name{scale_decayp_exp}
\alias{scale_decayp_continuous}
\alias{scale_decayp_exp}
\title{Continuous note decay scales}
\usage{
  scale_decayp_exp(soundlimits, limits = NULL, by = NULL,
    ...)

  scale_decayp_continuous(soundlimits, limits = NULL,
    by = NULL, ...)
}
\arguments{
  \item{\dots}{Other parameters (currently ignored)}

  \item{soundlimits}{The limits of the sound parameter.}

  \item{limits}{The limits of the data to train, a numeric
  vector of length 2. All data values outside these limits
  are returned as \code{NA}. If \code{NULL}, the default,
  the function takes the minimum and maximum of the data}

  \item{by}{The unit to round the sound parameter to.}
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
  parameters (here, \code{"decayp"}, the percentage of the
  note devoted to the decay/fade out).
}
\details{
  In \code{\link{shape_scatter}}, there are 3 phases of a
  note: the attack (the initial fade-in of the note),
  sustain (where the note is held), and the decay (the
  final fade-out of the note. \code{decayp} is the
  percentage of the note devoted to the decay; so a note
  with \code{decayp} would have a very abrupt ending,
  whereas a note with \code{decayp = 0.5} would gradually
  fade out.

  \code{_continuous} scales are a linear scaling, whereas
  \code{_exp} is an exponential scale.
}
\seealso{
  \code{\link{shape_scatter}}, which has more information
  about this parameter
}

