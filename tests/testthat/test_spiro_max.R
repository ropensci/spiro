library(spiro)

gxt_data <- spiro(spiro_example("zan_gxt"))
ramp_data <- spiro(spiro_example("zan_ramp"))
ramp_data_hr <- add_hr(ramp_data, spiro_example("hr_ramp.tcx"))

test_that("spiro_max returnes maximum values for ramp tests", {
  expect_type(spiro_max(ramp_data)$VO2, "double")
  expect_snapshot_output(spiro_max(ramp_data))
})

test_that("spiro_max can control smoothing of heart rate data", {
  expect_identical(
    spiro_max(ramp_data_hr)$HR > spiro_max(ramp_data_hr, hr_smooth = TRUE)$HR,
    TRUE
  )
  expect_message(spiro_max(ramp_data_hr, smooth = "15b", hr_smooth = TRUE))
  expect_error(spiro_max(ramp_data_hr, hr_smooth = 10))
})

test_that("different smoothing methods work", {
  expect_snapshot_output(spiro_max(ramp_data, smooth = 60))
  expect_snapshot_output(spiro_max(ramp_data, smooth = "60b"))
  expect_snapshot_output(spiro_max(ramp_data, smooth = "0.02f4"))
  expect_snapshot_output(spiro_max(ramp_data, smooth = "0.02fz4"))
  expect_identical(
    spiro_max(ramp_data, smooth = "40"),
    spiro_max(ramp_data, smooth = 40)
  )
})
