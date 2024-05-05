#library(testthat)

test_that("dirPackageZips() works", {

  f <- kwb.package:::dirPackageZips

  expect_error(f())

})
