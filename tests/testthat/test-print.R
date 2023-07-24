s <- spiro(spiro_example("zan_gxt"))

s$add <- "new"

test_that("printing works for non-numeric columns", {
  expect_no_error(print(head(s, 1)))
})
