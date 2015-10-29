# load all files in google drive

library(aceR)

raw_data_path = "~/Google Drive/ACE Studies_Raw Data/Adaptivity/Raw Data Files"
setwd(raw_data_path)

raw_files = files_in_directory()

out = data.frame()
for (raw_file in raw_files) {
  print(raw_file)
  dat = read_raw_csv(raw_file)
  out = plyr::rbind.fill(out, dat)
}


# variations

dat = read_raw_csv("01/Boxed_01.csv")

dat = read_raw_csv("01/BRT_01.csv")

dat = read_raw_csv("16/Boxed_16.csv")

dat = read_raw_csv("16/Stroop_16.csv")

dat = read_raw_csv("16/Flanker_16.csv") 

dat = read_raw_csv("12/Stroop_12.csv")

dat = read_raw_csv("16/Discrimination_16.csv")

dat = read_raw_csv("03/Discrimination_03.csv")

