  time <- shapeParam("time",
                     0,
                     Inf,
                     0,
                     list(limits = NULL, soundlimits = c(0, 5), linear_scale),
                     NULL,
                     "The starting time. Either time or tempo, but not both, must be specified for an object to be rendered.")
  pitch <- shapeParam("pitch",
                      4,
                      15,
                      8,
                      list(limits = NULL, soundlimits = c(8, 8), linear_scale),
                      NULL,
                      "Pitch represented such that 8 represents middle C, 9 represents the octave above, etc. (This is Csound's 'oct' notation")
  dur <- shapeParam("dur",
                    0,
                    Inf,
                    0.5,
                    list(limits = NULL, soundlimits = c(0.25, 4), linear_scale),
                    NULL,
                    "The duration of the hit (seconds)")
  pan <- shapeParam("pan",
                    0,
                    1,
                    0.5,
                    list(limits = NULL, soundlimits = c(0, 1), linear_scale),
                    NULL,
                    "The desired balance between left and right stereo channels, where 0 is all the left channel, 0.5 balanced, and 1 all the right.")
  tempo <- shapeParam("tempo",
                      0,
                      Inf,
                      NULL,
                      list(limits = NULL, soundlimits = c(120, 240), linear_scale),
                      NULL,
                      "The desired tempo of events in beats per minute. Either time or tempo, but not both, must be specified for an object to be rendered.")

dotplot <- shapeDef("A sonification using only synthesized snare hits to denote observations on the time axis.", "csound", list(), c(time, dur, pitch, pan, tempo))



                                
                                
