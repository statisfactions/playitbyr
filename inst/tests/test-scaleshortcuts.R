context("scalings")

test_that("scaling function sanity checks", {
  expect_equal(rep(5, 5), linear_scale(rep(22, 5), NULL, c(0, 10)))
  expect_equal(1:5, linear_scale(16:20*300, NULL, c(1,5)))
  expect_equal(c(NA, 1, 5, NA, NA), linear_scale(16:20, 17:18, c(1, 5)))
  expect_equal(rep(5, 5), exp_scale(rep(22, 5), NULL, c(0, 10)))
  expect_equal(1:3, exp_scale(c(1, 10, 100), NULL, c(1, 3)))
  expect_equal(c(NA, 2, 3, NA), exp_scale(c(1, 10, 100, -5), c(10, 100) , c(2, 3)))
  expect_equal(c(2, 2), linear_scale(c(1, 1), c(0, 1), c(1, 2)))
  expect_equal(c(2, 2), exp_scale(c(1, 1), c(0.1, 1), c(1, 2)))
  expect_equal(c(15, 1, 1),  linear_scale(c(1, 2, 2), limits = c(1, 2), soundlimits = c(15, 1)))
  expect_error(exp_scale(c(1, 1), c(0, 1), c(1, 2)))
})

test_that("removes all events in data that are outside limits on any scaling", {
  x <- sonify(iris[1:10,], sonaes(pitch = Sepal.Width, indx = Petal.Width, time = 1:10)) +
    shape_scatter(jitter = 0.3) + scale_indx_continuous(c(1, 20)) +
      scale_pitch_continuous(c(8, 10), c(3.5, 3.9))
  soncompare(x, "test-removeoutlimits.wav")
})

test_that("scaling function 'by' argument", {
  expect_error(linear_scale(1:10, soundlimits = c(1, 10), by = 0))
  expect_error(linear_scale(1:10, soundlimits = c(1, 10), by = NA))
  expect_equal(c(1, 1, 1.25, 1.25, 1.5, 1.5, 1.5, 1.75, 1.75, 2, 2), linear_scale(0:10, soundlimits = c(1, 2), by = 0.25))
  expect_equal(c(1, 1, 1.25, 1.25, 1.5, 1.5, 1.5, 1.75, 1.75, 2, 2), exp_scale(10^(0:10), soundlimits = c(1, 2), by = 0.25))
  expect_equal(c(1, 1.4, 1.8, 2.2), linear_scale(1:4, soundlimits = c(1, 2.2), by = 0.4))
})

test_that("scaling 'by' works on all linear scaling shortcuts", {
  iden <- data.frame(time = seq(0, 2, by = 0.1), pitch = seq(7, 11, by = .2),
                     dur = seq(0, 2, by = 0.1),
                     vol = seq(0, 1, by = 0.05),
                     pan = seq(0.5, 1, by = 0.025),
                     attkp = seq(0, 1, by = 0.05),
                     decayp = seq(0.25, 0.5, by = 0.0125),
                     mod = seq(1, 9, by = 0.4),
                     indx = seq(1, 17, by = 0.8))
  also <- as.data.frame(lapply(iden, function(x) x*abs(rnorm(1))*30))
  x <- sonify(iden, sonaes(time = time, pitch = pitch, dur = dur,
                           vol = vol, pan = pan, attkp = attkp,
                           decayp = decayp, mod = mod, indx = indx)) +
                             scale_time_identity() +
                               scale_pitch_identity() +
                                 scale_dur_identity() +
                                   scale_vol_identity() +
                                     scale_pan_identity() +
                                       scale_tempo_identity() +
                                         scale_attkp_identity() +
                                           scale_decayp_identity() +
                                             scale_mod_identity()  +
                                               scale_indx_identity() +
                                                 shape_scatter()

  y <- sonify(also, sonaes(time = time, pitch = pitch, dur = dur,
                           vol = vol, pan = pan, attkp = attkp,
                           decayp = decayp, mod = mod, indx = indx)) +
                             scale_time_continuous(soundlimits = c(0, 2), by = 0.1) +
                             scale_pitch_continuous(soundlimits = c(7, 11), by = .2) +
                     scale_dur_continuous(soundlimits = c(0, 2), by = 0.1) +
                     scale_vol_continuous(soundlimits = c(0, 1), by = 0.05) +
                     scale_pan_continuous(soundlimits = c(0.5, 1), by = 0.025) +
                     scale_attkp_continuous(soundlimits = c(0, 1), by = 0.05) +
                     scale_decayp_continuous(soundlimits = c(0.25, 0.5), by = 0.0125) +
                     scale_mod_continuous(soundlimits = c(1, 9), by = 0.4) +
                     scale_indx_continuous(soundlimits = c(1, 17), by = 0.8) +
                       shape_scatter()
  expect_equal(.getScore(x), .getScore(y))
})
 
 
  
