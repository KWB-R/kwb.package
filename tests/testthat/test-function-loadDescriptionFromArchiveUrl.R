library(testthat)

test_that("loadDescriptionFromArchiveUrl() works", {

  f <- kwb.package:::loadDescriptionFromArchiveUrl

  expect_error(f())

  url <- "https://cran.r-project.org/src/contrib/Archive/ggplot2/ggplot2_2.1.0.tar.gz"
  
  suppressMessages(result <- f(url))
  
  expect_type(result, "list")
  expect_true(grepl("Hadley", result$author))
})
