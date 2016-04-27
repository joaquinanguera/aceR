# loads processed files, adds demographics, spits out a bunch of plots

rm(list = ls())

BASE = "~/Desktop"
FIRST_BLOCK = "first_block"
setwd(BASE)

library(aceR)

# load demographics data
demographics_file = "All Participant Demographics.xlsx"
demographics = load_ace_demographics(demographics_file)

# load & process data by module
all_data_path = "ace_process"
out_path = "ace_process_demographs"
out_path_first_block = paste(out_path, FIRST_BLOCK, sep = "/")

# make ouput directories
make_directory(out_path)
make_directory(out_path_first_block)

all_files = list.files(path = all_data_path, recursive = TRUE)
mods = unique(basename(all_files))
for (module in mods) {
  # load data
  mod_dat = load_files(path = all_data_path, pattern = module, recursive = TRUE)
  # add demographic data
  mod_demo = merge(mod_dat, demographics, by = "pid")
  mod_demo = mod_demo[order(as.character(mod_demo$pid)), ]
  row.names(mod_demo) = NULL
  
  # output by tasks - nothing subsetted here
  mod_file_name = paste(out_path, module, sep ="/")
  write.csv(mod_demo, mod_file_name, na = "")
  
  # only look at first block
  mod_demo_first_block = aceR:::subset_first_block(mod_demo)
  mod_demo_first_block_file_name = paste(out_path, "first_block", module, sep = "/")
  write.csv(mod_demo_first_block, mod_demo_first_block_file_name, na = "")

}
