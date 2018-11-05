## use shift-cmd-t to run all tests
# This package is mainly a bridge between R and JS, mostly in JS side
# not much to test for R side
# However, I need to remove "no test" warning for R checks
testthat::test_check("threejsr")
