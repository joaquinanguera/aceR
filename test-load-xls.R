context("load raw excel data")

sample_bad_excel = paste(aceR_sample_data_path(), "bad-data.xlsx", sep = "/")
sample_brt_excel = paste(aceR_sample_data_path(), "sample-brt.xlsx", sep = "/")

expect_warning(load_ace_file(sample_bad_excel))
expect_gt(nrow(load_ace_file(sample_brt_excel)), 0)
