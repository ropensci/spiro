library(spiro)

file <- spiro_example("zan_gxt")
wfile <- spiro_example("hr_ramp.tcx")

test_that("device guess turns right", {
  expect_match(spiro:::guess_device(file), "zan")
  expect_match(spiro:::guess_device(wfile), "none")
})

test_that("import returns a data frame", {
  expect_s3_class(spiro_import_zan(file), "data.frame")
})

test_that("meta data is imported", {
  expect_s3_class(attr(spiro_import_zan(file), "info"), "data.frame")
  expect_s3_class(attr(spiro_import_zan(file), "info")$sex, "factor")
  expect_snapshot_output(attr(spiro_import_zan(file), "info"))
})
