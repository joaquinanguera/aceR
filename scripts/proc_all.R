# script for processing ALL data.
# assumes all files are unprocessed

rm(list = ls())

library(aceR)

# ACE Raw Data (*Processed Data Omitted)
setwd("~/Desktop/ACE Studies_Raw Data")

subdirs_to_ignore = c("Brighten", "Remaining _Jyoti","Remaining Raw Data (Brighten, BBT-MT, India, 16p-Adaptivity)")
files_to_ignore = c("AgileAcademic-IAN", "FlankerAllWithDate", "FlankerAll")
to_ignore = c(subdirs_to_ignore, files_to_ignore)

standard_ace_dat = load_ace_bulk(exclude = to_ignore)

# TODO: load "filtered" ace data