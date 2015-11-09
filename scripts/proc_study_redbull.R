# script for processing redbull data

rm(list = ls())

library(aceR)

setwd("~/Google Drive/ACE Studies_Raw Data/RedBull/Raw Data")

# load & process dat
dat = read_raw_csv_in_directory()
proc = proc_by_module(dat)

setwd("~/Desktop")
write.csv(proc, "proc_redbull.csv")