library(spiro)

hr_file <- spiro_example("hr_ramp.tcx")

test_that("heart rate import works", {
  expect_snapshot_output(hr_import(hr_file))
})
