#library(testthat)

test_that("archivedCranVersions() works", {

  f <- kwb.package::archivedCranVersions

  expect_error(f())

  check_general <- function(x) {
    expect_s3_class(x, "data.frame")
    expect_identical(names(x), c(
      "package", "version", "date", "archive_file", "date_type"
    ))
  }
  
  result <- f("no-such-package")
  check_general(result)
  expect_true(nrow(result) == 0L)
  
  result <- f("abc")
  check_general(result)
  expect_true(nrow(result) > 10L)
  expect_true(all(result$package == "abc"))
  
  result <- f(c("abc", "xyz"))
  check_general(result)
  expect_identical(unique(result$package), c("abc", "xyz"))
})
