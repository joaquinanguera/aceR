
library("aceR")
library("openxlsx")

DATA_PATH = "~/Desktop/ACE Studies_Raw Data/Brighten"

setwd(DATA_PATH)

file = "BRT_filtered.xlsx" # has "key" column
#file = "saat_filtered.xlsx" # no "key" column

dat = aceR:::load_ace_filtered_file(file)