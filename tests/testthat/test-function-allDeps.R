#library(testthat)

test_that("allDeps() works", {

  f <- kwb.package:::allDeps

  expect_error(f())

  f("abc")
})
