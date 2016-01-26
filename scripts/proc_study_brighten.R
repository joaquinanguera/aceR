# script for processing brighten data ("filtered" data)

rm(list = ls())

library(aceR)

setwd("~/Google Drive/ACE Studies_Raw Data/Brighten")

# load & process dat
dat = load_ace_bulk(recursive = FALSE)
proc = proc_by_module(dat, TRUE)

setwd("~/Desktop/brighten_test")
export_csv(proc)