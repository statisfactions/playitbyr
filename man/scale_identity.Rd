\name{scale_time_identity}
\alias{scale_attkp_identity}
\alias{scale_decayp_identity}
\alias{scale_dur_identity}
\alias{scale_indx_identity}
\alias{scale_mod_identity}
\alias{scale_pan_identity}
\alias{scale_pitch_identity}
\alias{scale_tempo_identity}
\alias{scale_time_identity}
\alias{scale_vol_identity}
\title{Use values without scaling}
\usage{
  scale_time_identity(...)

  scale_pitch_identity(...)

  scale_dur_identity(...)

  scale_vol_identity(...)

  scale_pan_identity(...)

  scale_tempo_identity(...)

  scale_attkp_identity(...)

  scale_decayp_identity(...)

  scale_mod_identity(...)

  scale_indx_identity(...)
}
\arguments{
  \item{\dots}{Ignored.}
}
\description{
  These functions can be added onto a \code{sonify} object
  to indicate that a given parameter is not to be scaled;
  the values given in the input data should be take as the
  given sound values.
}

