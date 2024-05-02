#library(testthat)

test_that("packageDependencies() works", {

  f <- kwb.package::packageDependencies

  result <- f()

  expect_type(result, "list")
  expect_true(all(sapply(result, typeof) == "character"))
  
})
