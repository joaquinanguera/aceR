# script for processing brighten data ("filtered" data)

rm(list = ls())

library(aceR)

# this is data from dropbox NOT Google Drive

setwd("~/Desktop/ACE data")

# load & process dat
dat = load_ace_bulk(pattern = "BRT", recursive = FALSE)
proc = proc_by_module(dat, TRUE)

all_tasks = data.frame()
module_names = names(proc)
for (i in 1:seq(length(proc))) {
  module = proc[[i]]
  module = module[order(as.character(module$bid)), ]
  module_name = module_names[i]
  first_block = subset_first_block(module)
  col_names = names(first_block)
  names(first_block) = sapply(col_names, function(x) {
    if (grepl("pid", x)) {
      new_name = x
    } else {
      new_name = paste(module_name, x, sep = "_")
    }
    return (new_name)
  })
  all_tasks = plyr::join(all_tasks, first_block, by = "pid")
}

setwd("~/Desktop/brighten_test")
export_csv(proc)