/* ----R Header: P-field list-----
inst, start, dur, pitch, amp, pan, 
-----end header---- */


sr=44100
kr=4410
0dbfs=1
nchnls=2

    instr 1
/* 'drum' timbre */
  ipan      =  p5
  ipitch    =  octcps(p4)
  kenv      linen     p6, 0.01*p3, p3, 0.99*p3
  anoise    rand      kenv
  asig      reson     anoise, ipitch, 500
            outs      asig*ipan, asig*(1-ipan)
    endin

/*
"Using Global Csound Instruments for Meta-Parameter Control", instr 1802 and 1803
*/

  gkfr      init      0                           ; INIT FREQUENCY

    instr 3                                       ; CONTROLS INST 2 FREQ
  koct      linseg   p4, p3, p5
  gkfr      =        cpsoct(koct)
    endin

    instr 4                                       
  kenv      linen     1, 0.01, p3, 0.01
  a1        oscil     kenv, gkfr, 1
            outs       a1,0*a1
    endin

    instr 2                                        ; ONE CONTINUOUS TONE               
  koct      linseg    5, p3, 9
  kfr       =         cpsoct(koct)
  kenv      linen     1, 0.01, p3, 0.01
  a1        oscil     kenv, kfr, 1
            outs       0*a1,a1
    endin



</CsScore>
</CsoundSynthesizer>
