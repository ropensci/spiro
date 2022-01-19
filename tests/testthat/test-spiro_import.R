library(spiro)

file <- spiro_example("zan_gxt")
wfile <- spiro_example("hr_ramp.tcx")
cosmed_file <- spiro_example("cosmed_test.xlsx")
cortex_file <- spiro_example("cortex_test.xml")
vyntus_file <- spiro_example("vyntus_test.txt")

test_that("device guess turns right", {
  expect_match(spiro:::guess_device(file), "zan")
  expect_match(spiro:::guess_device(wfile), "none")
  expect_match(spiro:::guess_device(cosmed_file), "cosmed")
  expect_match(spiro:::guess_device(cortex_file), "cortex")
  expect_match(spiro:::guess_device(vyntus_file), "vyntus")
})

test_that("import returns a data frame", {
  expect_snapshot_output(spiro_import_zan(file))
  expect_snapshot_output(spiro_import_cosmed(cosmed_file))
  expect_snapshot_output(spiro_import_cortex(cortex_file))
  expect_snapshot_output(spiro_import_vyntus(vyntus_file))
})

test_that("meta data is imported", {
  expect_s3_class(attr(spiro_import_zan(file), "info"), "data.frame")
  expect_s3_class(attr(spiro_import_zan(file), "info")$sex, "factor")
  expect_snapshot_output(attr(spiro_import_zan(file), "info"))
  expect_snapshot_output(attr(spiro_import_cosmed(cosmed_file), "info"))
  expect_snapshot_output(attr(spiro_import_cortex(cortex_file), "info"))
  expect_snapshot_output(attr(spiro_import_vyntus(vyntus_file), "info"))
})

test_that("anonymization works", {
  expect_snapshot_output(attr(spiro_import(file), "info"))
  expect_snapshot_output(attr(spiro_import(file, anonymize = FALSE), "info"))
  expect_equal(get_id("Jesse", "Owens", "12.09.1913"), "e09d4015")
  expect_equal(get_id("Jesse", "Owens"), "15a358c3")
  expect_equal(
    attr(spiro_import(file), "info")$id,
    get_id("Simon", "Nolte", "04.10.1998")
  )
})
