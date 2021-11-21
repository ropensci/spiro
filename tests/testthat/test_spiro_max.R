library(spiro)

gxt_data <- spiro(spiro_example("zan_gxt"))
ramp_data <- spiro(spiro_example("zan_ramp"))

test_that("spiro_max returnes maximum values for ramp tests", {
  expect_type(spiro_max(ramp_data)$VO2, "double")
  expect_snapshot_output(spiro_max(ramp_data))
})
