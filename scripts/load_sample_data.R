
library("aceR")

sample_boxed = paste(aceR_sample_data_path(), "sample-boxed.csv", sep = "/")
dat = load_ace_file(sample_boxed)
boxed_proc = proc_by_module(dat)$BOXED