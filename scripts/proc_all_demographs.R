# loads processed files, adds demographics

rm(list = ls())

setwd("~/Desktop")

library(aceR)

# load demographics data
demographics_file = "All Participant Demographics.xlsx"
demographics = load_ace_demographics(demographics_file)

# load & process data by module
all_data_path = "ace_process"
out_path = "ace_process_demographs"

# make ouput directory
if (!dir.exists(out_path)) {
  dir.create(file.path(out_path), showWarnings = FALSE)
}

all_files = list.files(path = all_data_path, recursive = TRUE)
mods = unique(basename(all_files))
for (module in mods) {
  # load data
  mod_dat = load_files(path = all_data_path, pattern = module, recursive = TRUE)
  # add demographic data
  mod_demo = merge(mod_dat, demographics, by = "pid")
  mod_demo = mod_demo[order(as.character(mod_demo$pid)), ]
  row.names(mod_demo) = NULL
  
  # putut
  mod_file_name = paste(out_path, module, sep ="/")
  write.csv(mod_demo, mod_file_name)
  
}