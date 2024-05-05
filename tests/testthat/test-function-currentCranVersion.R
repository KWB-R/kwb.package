#library(testthat)

test_that("currentCranVersion() works", {

  f <- kwb.package:::currentCranVersion

  result <- f()
  
  check_general <- function(x) {
    expect_s3_class(x, "data.frame")
    expect_identical(names(x), c("package", "version", "date", "date_type"))
  }

  check_general(result)
  
  result <- f("abc")
  check_general(result)
  expect_identical(nrow(result), 1L)
  expect_true(result$date <= Sys.Date())

})
