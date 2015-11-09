# script for processing redbull data

rm(list = ls())

library(aceR)

setwd("~/Google Drive/ACE Studies_Raw Data/RedBull/Raw Data")

# load & process boxed dat
boxed = read_raw_csv_in_directory()
boxed_proc = module_boxed(boxed)

setwd("~/Desktop")
write.csv(boxed_proc, "redbull_boxed_v1.csv")