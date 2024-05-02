#library(testthat)

test_that("getServername() works", {

  f <- kwb.package:::getServername

  result <- f()

  expect_type(result, "character")
  expect_length(result, 1L)
  
})
