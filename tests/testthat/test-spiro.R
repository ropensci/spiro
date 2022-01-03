library(spiro)

file <- spiro_example("zan_ramp")

test_that("spiro returns correct class", {
  expect_s3_class(spiro(file), "data.frame")
  expect_s3_class(spiro(file), "spiro")
})

test_that("import attributes raw data", {
  expect_s3_class(attr(spiro(file), "raw"), "data.frame")
})

test_that("protocol attribute set to NA stops protocol guessing", {
  expect_null(attr(spiro(file, protocol = NA), "protocol"))
})

test_that("weight argument is numeric and positive", {
  expect_error(spiro(file, weight = "sixty"))
  expect_error(spiro(file, weight = -100))
})

test_that("heart rate import works within spiro()", {
  expect_snapshot_output(spiro(file, hr_file = spiro_example("hr_ramp.tcx")))
})

test_that("spiro() returnes a warning for non breath-by-breath data", {
  expect_warning(spiro(spiro_example("cosmed_test.xlsx")))
})
