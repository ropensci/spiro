library(spiro)

data <- spiro(spiro_example("zan_ramp"))
data_hr <- add_hr(data, spiro_example("hr_ramp.tcx"))
p <- spiro_plot(data, smooth = 15)
pb <- spiro_plot(data, smooth = "15b")
pbh <- spiro_plot(data_hr, smooth = "15b")

test_that("visualization works", {
  expect_snapshot_output(p)
  expect_snapshot_output(pb)
  expect_snapshot_output(pbh)
})

test_that("input is validated", {
  expect_snapshot_error(spiro_plot(data, which = 20))
  expect_snapshot_error(spiro_plot(data, style_args = "nolist"))
  expect_snapshot_error(spiro_plot(data, grid_args = "nolist"))
})
