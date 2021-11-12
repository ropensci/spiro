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
})

test_that("spiro_max returnes maximum values for ramp tests", {
  expect_type(spiro_max(ramp_data)$VO2, "double")
  expect_snapshot_output(spiro_max(ramp_data))
})
