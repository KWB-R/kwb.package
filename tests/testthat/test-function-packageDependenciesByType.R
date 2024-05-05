#library(testthat)

test_that("packageDependenciesByType() works", {

  f <- kwb.package::packageDependenciesByType

  expect_null(f())

  result <- f("testthat")
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 1L)
  expect_identical(names(result), c("package", "type"))
  
})
