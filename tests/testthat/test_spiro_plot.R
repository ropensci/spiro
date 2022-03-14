library(spiro)

data <- spiro(spiro_example("zan_ramp"))
p <- spiro_plot(data, smooth = 15)

test_that("visualization works", {
  expect_snapshot_output(p)
})
