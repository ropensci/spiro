file <- spiro_example("zan_gxt")

an_true <- spiro(file, anonymize = TRUE)
an_false <- spiro(file, anonymize = FALSE)

test_that("raw data extraction works", {
  expect_identical(spiro_raw(spiro(file)), spiro_raw(file))
})

test_that("overriding of anonymization works", {
  expect_identical(
    attr(an_true, "info"),
    attr(spiro_raw(an_false, anonymize = TRUE), "info")
  )
  expect_warning(spiro_raw(an_true, anonymize = FALSE))
  expect_no_warning(spiro_raw(an_false, anonymize = FALSE))
})

test_that("device argument is ignored for spiro class method", {
  expect_warning(spiro_raw(spiro(file), device = "cortex"))
})
