# example script for loading all raw csv data in a directory

library(aceR)

raw_data_path = "~/Google Drive/ACE Studies_Raw Data/16 Person Study/Raw Data"
setwd(raw_data_path)

raw_files = files_in_directory()

out = data.frame()
for (raw_file in raw_files) {
  print(raw_file)
  dat = read_raw_csv(raw_file)
  out = plyr::rbind.fill(out, dat)
}