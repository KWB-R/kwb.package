#library(testthat)

testthat::skip("Do not detach testthat!")

test_that("detachRecursively() works", {

  f <- kwb.package::detachRecursively

  expect_error(f())

})
