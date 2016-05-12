# script for processing REST data

rm(list = ls())

library(aceR)

READ = "~/Desktop/REST Cognitive Tasks"
OUT = "~/Desktop/rest_process"

setwd(READ)

# load & process dat
dat = load_ace_bulk()
proc = proc_by_module(dat, TRUE)

setwd(OUT)
export_csv(proc)