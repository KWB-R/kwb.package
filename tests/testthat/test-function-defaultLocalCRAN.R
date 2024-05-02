#library(testthat)

test_that("defaultLocalCRAN() works", {

  f <- kwb.package:::defaultLocalCRAN

  result <- f()
  expect_true(endsWith(result, "/local-cran"))

})
