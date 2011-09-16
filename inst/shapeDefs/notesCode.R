  time <- shapeParam("time",
                     0,
                     Inf,
                     0,
                     list(0, 5, linear.scale),
                     NULL,
                     "The starting time. Either time or tempo, but not both, must be specified for an object to be rendered.")
  pitch <- shapeParam("pitch",
                      4,
                      15,
                      8,
                      list(8, 9, linear.scale),
                      NULL,
                      "Pitch represented such that 8 represents middle C, 9 represents the octave above, etc. (This is Csound's 'oct' notation")
  dur <- shapeParam("dur",
                    0,
                    Inf,
                    2,
                    list(0.25, 4, linear.scale),
                    NULL,
                    "The relative desired duration of all events in beats, where 1 beat equals the length of time for one event if all events were equal length")
  vol <- shapeParam("vol",
                    0,
                    1,
                    0.5,
                    list(0.2, 1, linear.scale),
                    NULL,
                    "The desired volume of all events as a number between 0, silence, and 1, the maximum possible amplitude.")
  pan <- shapeParam("pan",
                    0,
                    1,
                    0.5,
                    list(0, 1, linear.scale),
                    NULL,
                    "The desired balance between left and right stereo channels, where 0 is all the left channel, 0.5 balanced, and 1 all the right.")
  tempo <- shapeParam("tempo",
                      0,
                      Inf,
                      NULL,
                      list(120, 240, linear.scale),
                      NULL,
                      "The desired tempo of events in beats per minute. Either time or tempo, but not both, must be specified for an object to be rendered.")

notes <- shapeDef("A bare-bones sonification using pure sine tones", "audio", list(), c(time, pitch, dur, vol, pan, tempo))

                                
                                
