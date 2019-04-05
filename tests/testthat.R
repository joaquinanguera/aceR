Sys.setenv(R_TESTS="") # Because Stack Overflow said to?
library(testthat)
library(aceR)

test_check("aceR")
