# script for processing redbull data

rm(list = ls())

library(aceR)

setwd("~/Google Drive/ACE Studies_Raw Data/RedBull/Raw Data")

# load & process dat
dat = load_ace_bulk()
proc = proc_by_module(dat, TRUE)

setwd("~/Desktop/process")
export_csv(proc)

# costs
costs = sapply(proc, function(df) {
  costs = df[stringr::str_detect(names(df), "cost")]
  return(costs)
})
setwd("~/Desktop/process/costs")
export_csv(costs)