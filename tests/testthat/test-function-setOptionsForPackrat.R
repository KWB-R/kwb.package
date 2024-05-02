#library(testthat)

test_that("setOptionsForPackrat() works", {

  f <- kwb.package::setOptionsForPackrat

  expect_silent(result <- f())

  expect_type(result, "list")

  options(result)
  
})
