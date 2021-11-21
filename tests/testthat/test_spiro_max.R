library(spiro)

gxt_data <- spiro(spiro_example("zan_gxt"))
ramp_data <- spiro(spiro_example("zan_ramp"))
ramp_data_hr <- add_hr(ramp_data,spiro_example("hr_ramp.tcx"))

test_that("spiro_max returnes maximum values for ramp tests", {
  expect_type(spiro_max(ramp_data)$VO2, "double")
  expect_snapshot_output(spiro_max(ramp_data))
})

test_that("spiro_max can control smoothing of heart rate data", {
  expect_identical(
    spiro_max(ramp_data_hr)$HR > spiro_max(ramp_data_hr, hr_smooth = TRUE)$HR,
    TRUE
  )
})
