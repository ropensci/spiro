library(spiro)

file <- spiro_example("zan_gxt")

p1 <- set_protocol(pre(60),wu(60,50),const(60,100,3),steps(180,150,50,4))

test_that("protocol guessing works", {
  expect_s3_class(get_protocol(spiro_import(file)), "data.frame")
  expect_snapshot_output(get_protocol(spiro_import(file)))
})

test_that("protocol is attributed in spiro()", {
  expect_s3_class(attr(spiro(file),"protocol"), "data.frame")
})

test_that("protocol setting works", {
  expect_snapshot_output(p1)
})

test_that("protocol features can be extracted", {
  expect_identical(max(get_features(p1)$code), 7)
  expect_identical(
    get_features(p1)$type,
    c("pre measures","warm up", rep.int("load",7))
  )
})
