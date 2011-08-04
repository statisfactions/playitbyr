## Convenience functions for specifying scales.
scale_time_linear <- function(min, max) sonscaling(time=list(min, max, linear.scale))
scale_pitch_linear <- function(min, max) sonscaling(pitch=list(min, max, linear.scale))
scale_dur_linear <- function(min, max) sonscaling(dur=list(min, max, linear.scale))
scale_vol_linear <- function(min, max) sonscaling(vol=list(min, max, linear.scale))
scale_pan_linear <- function(min, max) sonscaling(pan=list(min, max, linear.scale))
scale_tempo_linear <- function(min, max) sonscaling(tempo=list(min, max, linear.scale))
