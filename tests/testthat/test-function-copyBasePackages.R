#library(testthat)

test_that("copyBasePackages() works", {

  f <- kwb.package::copyBasePackages

  expect_error(f())

  target_lib <- kwb.utils::tempSubdirectory("test")
  
  expect_output(result <- f(target_lib))
  expected <- sort(kwb.package::systemPackages(set_number = 2L))
  expect_identical(sort(dir(target_lib)), expected)
  expect_true(all(file.exists(result)))
  
})
