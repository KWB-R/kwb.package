#library(testthat)

test_that("getPackageFilesToInstall() works", {

  f <- kwb.package::getPackageFilesToInstall

  suppressWarnings(result <- f())

  expect_type(result, "character")
  expect_true(all(file.exists(result)))
  
})
