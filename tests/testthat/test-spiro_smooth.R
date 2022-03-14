library(spiro)

d1 <- c(5,3,4,1,2,3)
d2 <- c(2,1,2,3,NA,1)
dd <- data.frame(a = d1, b = d2)
dl <- list(a = d1, b = d2)

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

test_that("vectorization works", {
  expect_type(spiro_smooth(d1, 3), "double")
  expect_s3_class(spiro_smooth(dd, 3), "data.frame")
})

test_that("time averaging works", {
  expect_equal(spiro_smooth(d1, 3), c(NA,4,8/3,7/3,2,NA))
  expect_equal(spiro_smooth(d1, 5), c(NA,NA,3,2.6,NA,NA))
  expect_equal(spiro_smooth(d2, 3), c(NA,5/3,2,7/3,2,NA))
})

test_that("breath averaging works", {
  expect_snapshot_output(spiro_smooth(s["VO2"], "20b", rawsource = s))
  expect_snapshot_output(spiro_smooth(s[c("VO2","VCO2")], "5b", rawsource = s))
  expect_error(spiro_smooth(s["VO2"], "5b"))
  expect_warning(spiro_smooth(s$VO2, "10b", rawsource = s))
  expect_warning(spiro_smooth(d1, "3b"))
  expect_equal(suppressWarnings(spiro_smooth(d1, "3b")), spiro_smooth(d1, 3))
})

test_that("Butterworth filter work", {
  expect_snapshot_output(spiro_smooth(d1, "f"))
  expect_snapshot_output(spiro_smooth(d2, "0.02f4"))
  expect_snapshot_output(spiro_smooth(d1, "fz"))
  expect_snapshot_output(spiro_smooth(d2, "0.02fz4"))
})
