#library(testthat)

test_that("getCranPackageDatabase() works", {

  f <- kwb.package::getCranPackageDatabase

  result <- f()

  expect_s3_class(result, "data.frame")
  expect_true(all(c("Package", "Depends", "Imports") %in% names(result)))
  
})
