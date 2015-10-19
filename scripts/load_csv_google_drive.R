# load all files in google drive

library(aceR)

raw_data_path = "~/Google Drive/ACE Studies_Raw Data/Adapativity"
setwd(raw_data_path)

raw_files = files_in_directory()

out = data.frame()
for (raw_file in raw_files) {
  print(raw_file)
  dat = read_raw_csv(raw_file)
  out = plyr::rbind.fill(out, dat)
}

# load individual files - each with their own unique structure

boxed = read_raw_csv("Raw Data/Boxed-2.csv")

flanker_one_block = read_raw_csv("Raw Data/Flanker-46.csv")

flanker_multi_block = read_raw_csv("Raw Data/Flanker-5.csv") # throws error

