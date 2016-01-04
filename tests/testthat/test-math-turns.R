context("test turns")

sample_boxed = paste(aceR_sample_data_path(), "sample-boxed.csv", sep = "/")
boxed_proc = load_ace_file(sample_boxed)

rws = boxed_proc$rw