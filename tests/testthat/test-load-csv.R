context("load raw csv data")

sample_bad = paste(aceR_sample_data_path(), "bad-data.csv", sep = "/")
sample_valid_but_bad_because_empty = paste(aceR_sample_data_path(), "bad-data-2.csv", sep = "/")
sample_boxed = paste(aceR_sample_data_path(), "sample-boxed.csv", sep = "/")
sample_brt_pulvinar = paste(aceR_sample_data_path(), "sample-brt-pulvinar.csv", sep = "/")


expect_warning(load_ace_file(sample_bad))
expect_warning(load_ace_file(sample_valid_but_bad_because_empty))
expect_gt(nrow(load_ace_file(sample_boxed)), 0)
expect_gt(nrow(load_ace_file(sample_brt_pulvinar)), 0)
