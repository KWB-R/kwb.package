#library(testthat)

test_that("getRVersionMajorMinor() works", {

  f <- kwb.package::getRVersionMajorMinor

  result <- f()

  expect_true(grepl("^\\d+\\.\\d+$", result))
  
})
