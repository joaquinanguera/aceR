context("test detection")

rw = c(1000, 990, 980, 960, 920, 840, 680, 360, 400, 440, 520, 680, 670, 660, 640, 680, 720, 710, 700, 740, 730, 720, 700, 660, 700)
rt = c(669, 653, 599, 562, 640, 747, 604, NA, NA, NA, NA, 537, 627, 519, 507, NA,  601, 637, NA, 649, 654, 634, 668, NA, 662)
correct_rejections = c("hit", "hit", "hit", "hit", "hit", "hit", "hit", "false_alarm", "false_alarm", "false_alarm", "false_alarm", "hit", "hit", "hit", "miss", "false_alarm", "hit", "hit", "false_alarm", "hit", "hit", "hit", "hit", "false_alarm", NA)

num_rw = length(rw)
num_rt = length(rt)

expect_equal(num_rw, num_rt)
expect_equal(aceR:::identify_correct_rejections(rw, rt), as.factor(correct_rejections))