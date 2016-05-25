# take processed data, get first block, merge by participant

rm(list = ls())

library(aceR)

# set directories

PROCESSED_DAT_DIRECTORY = "~/Desktop/ace/ace_processed"
RELEASE_DIRECTORY = "~/Desktop/ace/ace_processed_first_block"
DEMOGRAPHICS_FILE = "~/Desktop/ace/ACE Participant Demographics.xlsx"

# define constants

EFA_VARS = c(
  "BOXED-acc_mean.conj_cost",
  "BOXED-acc_mean.conjunction_12",
  "BOXED-acc_mean.conjunction_4",
  "BOXED-acc_mean.dist_cost_mean",
  "BOXED-acc_mean.feat_cost",
  "BOXED-acc_mean.feature_12",
  "BOXED-acc_mean.feature_4",
  "BOXED-rt_mean.conj_cost",
  "BOXED-rt_mean.conjunction_12",
  "BOXED-rt_mean.conjunction_4",
  "BOXED-rt_mean.dist_cost_mean",
  "BOXED-rt_mean.feat_cost",
  "BOXED-rt_mean.feature_12",
  "BOXED-rt_mean.feature_4",
  "BOXED-rw_mean.conj_cost",
  "BOXED-rw_mean.conjunction_12",
  "BOXED-rw_mean.conjunction_4",
  "BOXED-rw_mean.dist_cost_mean",
  "BOXED-rw_mean.feat_cost",
  "BOXED-rw_mean.feature_12",
  "BOXED-rw_mean.feature_4",
  "BRT-acc_mean.left",
  "BRT-acc_mean.right",
  "BRT-rt_mean.left",
  "BRT-rt_mean.right",
  "BRT-rw_mean.left",
  "BRT-rw_mean.right",
  "FLANKER-acc_mean.congruent",
  "FLANKER-acc_mean.cost",
  "FLANKER-acc_mean.incongruent",
  "FLANKER-rt_mean.congruent",
  "FLANKER-rt_mean.cost",
  "FLANKER-rt_mean.incongruent",
  "FLANKER-rw_mean.congruent",
  "FLANKER-rw_mean.cost",
  "FLANKER-rw_mean.incongruent",
  "SAAT-acc_mean.impulsive",
  "SAAT-acc_mean.sustained",
  "SAAT-rt_mean.impulsive",
  "SAAT-rt_mean.sustained",
  "SAAT-rw_mean.impulsive",
  "SAAT-rw_mean.sustained",
  "SPATIALSPAN-object_count_span.overall",
  "STROOP-acc_mean.congruent",
  "STROOP-acc_mean.cost",
  "STROOP-acc_mean.incongruent",
  "STROOP-rt_mean.congruent",
  "STROOP-rt_mean.cost",
  "STROOP-rt_mean.incongruent",
  "STROOP-rw_mean.congruent",
  "STROOP-rw_mean.cost",
  "STROOP-rw_mean.incongruent",
  "TASKSWITCH-acc_mean.cost",
  "TASKSWITCH-acc_mean.start",
  "TASKSWITCH-acc_mean.stay",
  "TASKSWITCH-rt_mean.cost",
  "TASKSWITCH-rt_mean.start",
  "TASKSWITCH-rt_mean.stay",
  "TASKSWITCH-rw_mean.cost",
  "TASKSWITCH-rw_mean.start",
  "TASKSWITCH-rw_mean.stay",
  "TNT-acc_mean.cost",
  "TNT-acc_mean.tap_only",
  "TNT-acc_mean.tap_trace",
  "TNT-rt_mean.cost",
  "TNT-rt_mean.tap_only",
  "TNT-rt_mean.tap_trace",
  "TNT-rw_mean.cost",
  "TNT-rw_mean.tap_only",
  "TNT-rw_mean.tap_trace"
  )

# load demographics

demo = load_ace_demographics(DEMOGRAPHICS_FILE)

# remove duplicates
duplicated_pids = which(duplicated(demo$pid))
demo_clean = demo[-duplicated_pids, ]

# load & process dat
dat = load_files(path = PROCESSED_DAT_DIRECTORY, pattern = ".csv", recursive = TRUE, verbose = TRUE)
dat$module = as.character(dat$module)
by_task = aceR:::subset_by_col(dat, "module")

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

# cleanup
clean = aceR:::replace_nas(all_tasks, "")
clean = aceR:::remove_empty_cols(clean)

setwd(RELEASE_DIRECTORY)
write.csv(clean, "first_block.csv")

# subset & add new demographics

clean_subset = clean[, c("pid", EFA_VARS)]
clean_concise = merge(clean_subset, demo_clean, by = "pid")
clean_concise = clean_concise[order(clean_concise$pid),]
write.csv(clean_concise, "first_block_concise.csv")

