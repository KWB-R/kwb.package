#library(testthat)

test_that("installedKwbPackages() works", {

  f <- kwb.package::installedKwbPackages

  result <- f()

  expect_type(result, "character")

})
