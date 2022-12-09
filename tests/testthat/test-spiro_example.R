test_that("spiro_example returns file path", {
  expect_type(spiro_example("zan_gxt"), "character")
})

test_that("spiro_example returns all example files", {
  expect_equal(length(spiro_example()), 3)
})
