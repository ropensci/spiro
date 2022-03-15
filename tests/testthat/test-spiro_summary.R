library(spiro)

gxt_data <- spiro(spiro_example("zan_gxt"))
ramp_data <- spiro(spiro_example("zan_ramp"))

test_that("stepwise summary works", {
  expect_snapshot_output(spiro_summary(gxt_data))
  expect_snapshot_output(spiro_summary(ramp_data))
})

test_that("spiro_summary returns message when shortening interval", {
  expect_message(spiro_summary(gxt_data))
  expect_message(spiro_summary(ramp_data))
  expect_snapshot(spiro_summary(ramp_data, interval = 300))
})

test_that("spiro_summary returns no message when set to quiet", {
  expect_message(spiro_summary(gxt_data, quiet = TRUE), regexp = NA)
  expect_message(spiro_summary(ramp_data, quiet = TRUE), regexp = NA)
})

test_that("spiro_summary excludes unfinished steps if desired", {
  expect_equal(nrow(spiro_summary(ramp_data, exclude = FALSE)), 25)
  expect_equal(nrow(spiro_summary(ramp_data, exclude = TRUE)), 24)
})

test_that("input is validated", {
  expect_snapshot_error(spiro_summary(ramp_data, interval = -10))
  expect_snapshot_error(spiro_summary(ramp_data, quiet = 1))
  expect_snapshot_error(spiro_summary(ramp_data, exclude = "a"))
  expect_snapshot_error(
    spiro_summary(spiro(spiro_example("zan_gxt"), protocol = NA))
  )
})
