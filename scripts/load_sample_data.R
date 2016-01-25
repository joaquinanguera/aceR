
library("aceR")

sample_boxed = paste(aceR_sample_data_path(), "sample-boxed.csv", sep = "/")
dat = load_ace_file(sample_boxed)
boxed_proc = proc_by_module(dat)$BOXED

sample_brt = paste(aceR_sample_data_path(), "sample-brt.xlsx", sep = "/")
dat_brt = load_ace_file(sample_brt)
boxed_proc_brt = proc_by_module(dat_brt)