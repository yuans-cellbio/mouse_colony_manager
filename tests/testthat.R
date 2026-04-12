library(testthat)

files <- list.files("R", pattern = "\\.[Rr]$", full.names = TRUE)
invisible(lapply(files, source))

test_dir("tests/testthat")
