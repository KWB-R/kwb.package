#
# This file was generated by kwb.test::create_test_files(), 
# launched by hsonne on 2024-05-02 18:06:13.
# Please modify the dummy functions so that real cases are
# tested. Then, delete this comment.
#

test_that("gradToRad() works", {

  f <- kwb.package::gradToRad

  expect_error(
    f()
    # Argument "grad" fehlt (ohne Standardwert)
  )

})
