\name{scale_attkp_continuous}
\alias{scale_attkp_continuous}
\alias{scale_attkp_exp}
\title{Continuous scales for note attacks}
\usage{
  scale_attkp_continuous(soundlimits, limits = NULL,
    by = NULL, ...)

  scale_attkp_exp(soundlimits, limits = NULL, by = NULL,
    ...)
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
  parameters (here, \code{"attkp"}, the sharpness of a
  note's initial attack).
}
\details{
  In \code{\link{shape_scatter}}, there are 3 phases of a
  note: the attack (the initial fade-in of the note),
  sustain (where the note is held), and the decay (the
  final fade-out of the note. \code{attkp} is the
  percentage of the note devoted to the attack; so a note
  with \code{attkp = 0.01} would have a very quick start of
  the note, whereas a note with \code{attkp = 0.5} would
  gradually fade in.

  \code{_continuous} scales are a linear scaling, whereas
  \code{_exp} is an exponential scale.
}
\seealso{
  \code{\link{shape_scatter}}, which has more information
  about this parameter
}

