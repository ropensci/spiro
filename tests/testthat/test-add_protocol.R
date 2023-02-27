file <- spiro_example("zan_gxt")

p1 <- set_protocol(
  pt_pre(60),
  pt_wu(60, 50),
  pt_const(60, 100, 3),
  pt_steps(180, 150, 50, 4)
)

p2 <- set_protocol_manual(
  duration = c(100, 200, 500, 100),
  load = c(50, 150, 250, 350)
)

dtf1 <- data.frame(
  duration = c(100, 200, 500, 100),
  load = c(50, 150, 250, 350)
)

dtf2 <- data.frame(
  col1 = c(100, 200, 500, 100),
  col2 = c(50, 150, 250, 350)
)

test_that("protocol guessing works", {
  expect_s3_class(get_protocol(spiro_raw(file)), "data.frame")
  expect_snapshot_output(get_protocol(spiro_raw(file)))
})

test_that("protocol is attributed in spiro()", {
  expect_s3_class(attr(spiro(file), "protocol"), "data.frame")
})

test_that("protocol setting works", {
  expect_snapshot_output(p1)
  expect_snapshot_output(p2)
  expect_snapshot_output(set_protocol_manual(dtf1))
  expect_snapshot_output(set_protocol_manual(dtf2))
})

test_that("protocol features can be extracted", {
  expect_identical(max(get_features(p1)$code), 7)
  expect_identical(
    get_features(p1)$type,
    c("pre measures", "warm up", rep.int("load", 7))
  )
})

test_that("last.load argument works", {
  expect_identical(
    pt_steps(30, 100, 50, 3, 10, 20),
    data.frame(
      duration = c(30, 10, 30, 10, 20),
      load = c(100, 0, 150, 0, 200)
    )
  )
})
