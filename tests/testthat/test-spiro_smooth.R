library(spiro)

s <- spiro(spiro_example("zan_gxt"))

test_that("smooth argument is correctly matched", {
  expect_identical(smooth_match(40), list(type = "time", param = 40))
  expect_identical(smooth_match(20), list(type = "time", param = 20))
  expect_identical(smooth_match("20"), list(type = "time", param = 20))
  expect_identical(smooth_match("20b"), list(type = "breath", param = 20))
  expect_identical(
    smooth_match("f"),
    list(type = "bw", param = list(W = NULL, n = NULL))
  )
  expect_identical(
    smooth_match("0.1f"),
    list(type = "bw", param = list(W = 0.1, n = NULL))
  )
  expect_identical(
    smooth_match("f4"),
    list(type = "bw", param = list(W = NULL, n = 4))
  )
  expect_identical(
    smooth_match("0.05f2"),
    list(type = "bw", param = list(W = 0.05, n = 2))
  )
  expect_identical(
    smooth_match("fz"),
    list(type = "bw_zl", param = list(W = NULL, n = NULL))
  )
  expect_identical(
    smooth_match("0.1fz"),
    list(type = "bw_zl", param = list(W = 0.1, n = NULL))
  )
  expect_identical(
    smooth_match("fz4"),
    list(type = "bw_zl", param = list(W = NULL, n = 4))
  )
  expect_identical(
    smooth_match("0.05fz2"),
    list(type = "bw_zl", param = list(W = 0.05, n = 2))
  )
})

test_that("time averaging works", {
  expect_snapshot_output(spiro_smooth(s))
  expect_snapshot_output(spiro_smooth(s, columns = c("VO2", "RER")))
  expect_error(spiro_smooth(s, columns = c("RR", "CVO2")))
})

test_that("breath averaging works", {
  expect_snapshot_output(spiro_smooth(s, "20b"))
  expect_snapshot_output(spiro_smooth(s, "20b", columns = c("VO2", "VCO2")))
  expect_warning(spiro_smooth(s, "10b", columns = c("VO2", "RER")))
  expect_error(spiro_smooth(s, "40b", c("VE", "SSF")))
  expect_equal(
    suppressWarnings(spiro_smooth(s, "3b", "RER")),
    spiro_smooth(s, 3, "RER"),
    ignore_attr = "smooth_method"
  )
})

test_that("Butterworth filter work", {
  # this snapshot lead to rounding errors based on the machine run
  expect_snapshot_output(round(spiro_smooth(s, "f"), 4))
  expect_snapshot_output(spiro_smooth(s, "0.02f4", c("VO2", "VE")))
  expect_warning(spiro_smooth(s, "fz", "RER"))
  expect_error(spiro_smooth(s, "0.02fz4", c("VCO2", "VO3")))
})

test_that("input is validated", {
  expect_snapshot_error(spiro_smooth(s, "0b"))
  expect_snapshot_error(spiro_smooth(s, "50k"))
})
