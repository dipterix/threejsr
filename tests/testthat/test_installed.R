# test_installed.R
testthat::context("Installed")

testthat::test_that("threejsr_installed", {
  testthat::expect_true("threejsr" %in% installed.packages())
})
