#library(testthat)

testthat::skip("Do not detach testthat!")

test_that("detachAllNonSystemPackages() works", {

  f <- kwb.package::detachAllNonSystemPackages

  expect_output(f())

})
