context("basic calculations")

library(stats)
library(sciplot)

vals = as.numeric(InsectSprays$count)
group = as.character(InsectSprays$spray)

correct_mean = mean(vals, na.rm = TRUE)
correct_median = median(vals, na.rm = TRUE)
correct_sd = sd(vals, na.rm = TRUE)
correct_se = sciplot::se(vals, na.rm = TRUE)

expect_equal(aceR:::ace_mean(vals), correct_mean)
expect_equal(aceR:::ace_median(vals), correct_median)
expect_equal(aceR:::ace_sd(vals), correct_sd)
expect_equal(aceR:::ace_se(vals), correct_se)