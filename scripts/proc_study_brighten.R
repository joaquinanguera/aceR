# script for processing brighten data ("filtered" data)

rm(list = ls())

library(aceR)

# this is data from dropbox NOT Google Drive

setwd("~/Desktop/ACE Studies_Raw Data/Brighten")

# load demographics
demographics_file = "~/Desktop/All Participant Demographics.xlsx"
demographics = load_ace_demographics(demographics_file)

problematic_brighten = c("brt_filtered.xlsx", "saat_filtered.xlsx")

# load & process dat
dat = load_ace_bulk(recursive = FALSE, exclude = problematic_brighten)
proc = proc_by_module(dat, TRUE)

# TODO: this is sloooooow...
subset_first_block = function(df) {
  sorted_bid = df[order(as.character(df$bid)), ]
  sub_pid = aceR:::subset_by_col(sorted_bid, "pid")
  out = data.frame()
  for (sub in sub_pid) {
    out = plyr::rbind.fill(out, sub[1, ])
  }
  return (out)
}

all_tasks = data.frame(pid = "dummy")
module_names = names(proc)
for (i in seq(length(module_names))) {
  module = proc[[i]]
  module = merge(module, demographics, by = "pid")
  module_name = module_names[i]
  first_block = subset_first_block(module)
  col_names = names(first_block)
  names(first_block) = sapply(col_names, function(x) {
    if (grepl("pid", x)) {
      new_name = x
    } else {
      new_name = paste(module_name, x, sep = "-")
    }
    return (new_name)
  })
  if (i == 1) {
    all_tasks = first_block
  } else {
    all_tasks = plyr::join(all_tasks, first_block, by = "pid")
  }
}

# cleanup
clean = aceR:::replace_nas(all_tasks, "")
clean = aceR:::remove_empty_cols(clean)


setwd("~/Desktop")
write.csv(clean, "first_block.csv")