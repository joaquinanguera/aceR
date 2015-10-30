# example script for loading all raw csv data in a directory

library(aceR)

raw_data_path = "~/Google Drive/ACE Studies_Raw Data/16 Person Study/Raw Data"
setwd(raw_data_path)

dat = read_raw_csv_in_directory()