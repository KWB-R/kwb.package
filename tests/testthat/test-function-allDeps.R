#library(testthat)

test_that("allDeps() works", {

  f <- kwb.package:::allDeps

  expect_error(f())

  suppressMessages(result <- f(name = "abc", max_depth = 1L))
  
  expect_s3_class(result, "data.frame")
  
  expect_identical(names(result), c(
    "name", "compare", "version", "type", "depth", "namever"
  ))
})
