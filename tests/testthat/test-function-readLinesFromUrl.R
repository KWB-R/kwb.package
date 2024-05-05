#library(testthat)

test_that("readLinesFromUrl() works", {

  f <- kwb.package:::readLinesFromUrl

  expect_null(f())

})
