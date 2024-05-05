#library(testthat)

test_that("isOnCran() works", {

  f <- kwb.package:::isOnCran

  result <- f()

  expect_type(result, "logical")
  expect_length(result, 1L)
  
})
