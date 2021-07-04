library(spiro)

test_that("device guess turn right", {
  expect_match(spiro:::guess_device(spiro_example("zan_gxt")),"zan")
  expect_match(spiro:::guess_device(spiro_example("hr_ramp.tcx")),"none")
})

