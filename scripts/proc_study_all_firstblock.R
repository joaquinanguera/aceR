# take processed data, get first block, merge by participant

rm(list = ls())

library(aceR)

setwd("~/Desktop/ace_process_demographs/first_block")

# load & process dat
dat = load_files(path = ".", pattern = ".csv", recursive = FALSE, verbose = TRUE)
dat$module = as.character(dat$module)
by_task = aceR:::subset_by_col(dat, "module")

all_tasks = data.frame(pid = "dummy")
module_names = names(by_task)
for (i in seq(length(module_names))) {
  module = by_task[[i]]
  module_name = module_names[i]
  first_block = aceR:::subset_first_block(module)
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