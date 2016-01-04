context("test span")

vals = c(2, 2, 3, 4, 4, 6, 7, 7, 8, 6, 6, 6, 7, 8)
correct_span = 7

expect_equal(aceR:::ace_span(vals), correct_span)