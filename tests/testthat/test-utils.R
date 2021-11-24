test_that("converting time formats to seconds succeeds", {
  expect_identical(to_seconds(c("01:02:59", "10:15:00")), c(3779, 36900))
  expect_identical(to_seconds(c("02:59", "58:04")), c(179, 3484))
})
