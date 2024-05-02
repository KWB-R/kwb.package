#library(testthat)

test_that("currentCranVersion() works", {

  f <- kwb.package:::currentCranVersion

  result <- f()
  
  expect_s3_class(result, "data.frame")
  expect_identical(names(result), c("package", "version", "date", "date_type"))

})
