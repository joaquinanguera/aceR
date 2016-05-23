# script for processing seacrest data

rm(list = ls())

library(aceR)

setwd("~/Desktop/Raw ACE Data Sea Crest School")

# load & process dat
dat = load_ace_bulk()
proc = proc_by_module(dat, TRUE)

setwd("~/Desktop/process")