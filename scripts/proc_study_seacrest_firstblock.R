# script for processing seacrest data

rm(list = ls())

library(aceR)

# define constants

BASE_DIRECTORY = "~/Desktop/ace"
SEACREST_DIRECTORY = paste(BASE_DIRECTORY, "ace_processed", "Sea Crest School", sep = "/")
WOODCOCK_DIRECTORY = paste(BASE_DIRECTORY, "woodcock", sep = "/")
DEMOGRAPHICS_FILE = paste(BASE_DIRECTORY, "Sea Crest Demographics.xlsx", sep = "/")
TRANSFORMED_PATTERN = "transform"

EFA_VARS = c(
  "BRT-rt_mean.left",
  "BRT-rt_mean.right",
  "SPATIALSPAN-object_count_span.overall",
  "STROOP-acc_mean.congruent", 
  "STROOP-acc_mean.incongruent", 
  "STROOP-acc_mean.cost", 
  "STROOP-rt_mean.correct.congruent", 
  "STROOP-rt_mean.correct.incongruent", 
  "STROOP-rt_mean.correct.cost",
  "FLANKER-acc_mean.congruent", 
  "FLANKER-acc_mean.incongruent",
  "FLANKER-acc_mean.cost", 
  "FLANKER-rt_mean.congruent", 
  "FLANKER-rt_mean.incongruent", 
  "FLANKER-rt_mean.cost",
  "SAAT-acc_mean.impulsive", 
  "SAAT-acc_mean.sustained", 
  "SAAT-rt_mean.impulsive", 
  "SAAT-rt_mean.sustained",
  "BOXED-acc_mean.overall", 
  "BOXED-rt_mean.overall",
  "TASKSWITCH-rt_mean.stay", 
  "TASKSWITCH-rt_mean.switch",
  "TNT-acc_mean.cost"
  )

# load demographics
demographics = load_ace_demographics(DEMOGRAPHICS_FILE)

# load transformed woodcock metrics
woodcock = aceR:::load_woodcock_transformed(WOODCOCK_DIRECTORY, TRANSFORMED_PATTERN)

# load processed dat

dat = load_files(path = SEACREST_DIRECTORY, pattern = ".csv", recursive = FALSE, verbose = TRUE)
dat$module = as.character(dat$module)
dat$pid = as.character(dat$pid)
by_task = aceR:::subset_by_col(dat, "module")

# this is straight copy-paste from proc_study_all_firstblock.R
# TODO: put into aceR
all_tasks = data.frame(pid = "dummy")
module_names = names(by_task)
for (i in seq(length(module_names))) {
  module = by_task[[i]]
  module_name = module_names[i]
  print(module_name)
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

clean_subset = all_tasks[, c("pid", EFA_VARS)]
clean_demo = merge(clean_subset, demographics, by = "pid")
clean_woodcock = merge(clean_demo, woodcock, by = "pid")
write.csv(clean_woodcock, "seacrest_first_block_concise.csv")