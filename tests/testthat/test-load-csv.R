context("load raw csv data")

sample_bad = paste(aceR_sample_data_path(), "bad-data.csv", sep = "/")
sample_boxed = paste(aceR_sample_data_path(), "sample-boxed.csv", sep = "/")

expect_warning(load_ace_file(sample_bad))
expect_more_than(nrow(load_ace_file(sample_boxed)), 0)