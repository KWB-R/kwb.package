#library(testthat)

test_that("getBinaryPaths() works", {

  f <- kwb.package:::getBinaryPaths

  result <- f()

  expect_type(result, "character")
  expect_true(length(result) > 0L)
  expect_true(grepl("windows", result[1L]))
})
