#
# This file was generated by kwb.test::create_test_files(), 
# launched by hsonne on 2024-05-02 17:58:26.
# Please modify the dummy functions so that real cases are
# tested. Then, delete this comment.
#

test_that("cranVersions() works", {

  f <- kwb.package::cranVersions

  expect_error(
    f()
    # Argument "name" fehlt (ohne Standardwert)
  )

})
