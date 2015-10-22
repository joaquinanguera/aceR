# load all files in google drive

library(aceR)

raw_data_path = "~/Google Drive/ACE Studies_Raw Data/Adaptivity/Raw Data Files/01"
setwd(raw_data_path)

raw_files = files_in_directory()

out = data.frame()
for (raw_file in raw_files) {
  print(raw_file)
  dat = read_raw_csv(raw_file, TRUE)
  out = plyr::rbind.fill(out, dat)
}

