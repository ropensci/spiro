library(spiro)

file <- spiro_example("zan_ramp")

test_that("spiro returns correct class", {
  expect_s3_class(spiro(file), "data.frame")
  expect_s3_class(spiro(file), "spiro")
})

