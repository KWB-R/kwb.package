#library(testthat)

test_that("defaultPackageDir() works", {

  f <- kwb.package:::defaultPackageDir

  result <- f()

  expect_type(result, "character")
  expect_length(result, 1L)
  
})
