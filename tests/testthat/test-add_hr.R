hr_file <- spiro_example("hr_ramp.tcx")
file <- spiro_example("zan_ramp")

test_that("heart rate import works", {
  expect_snapshot_output(hr_import(hr_file))
})

test_that("heart rate is shown as NAs if not present", {
  expect_identical(all(is.na(spiro(file)$HR)), TRUE)
})

test_that("heart rate data adding works", {
  expect_identical(all(add_hr(spiro(file), hr_file)$HR > 0), TRUE)
})

test_that("heart rate offset works", {
  expect_identical(all(is.na(add_hr(spiro(file), hr_file, 60)$HR[1:60])), TRUE)
  expect_identical(add_hr(spiro(file), hr_file, 60)$HR[61], 127)
  expect_equal(
    max(which(!is.na(add_hr(spiro(file), hr_file, -900)$HR))),
    24
  )
})
