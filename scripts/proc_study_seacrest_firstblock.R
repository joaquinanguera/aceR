# script for processing seacrest data

rm(list = ls())

library(aceR)
library(stringr)

# define constants

BASE_DIRECTORY = "~/Desktop/ace"
RAW_SEACREST_DIRECTORY = paste(BASE_DIRECTORY, "ACE Studies_Raw Data", "Sea Crest School", sep = "/")
WOODCOCK_DIRECTORY = paste(BASE_DIRECTORY, "woodcock", sep = "/")
DEMOGRAPHICS_FILE = paste(BASE_DIRECTORY, "ACE Participant Demographics.xlsx", sep = "/")
TRANSFORMED_AGE = paste(WOODCOCK_DIRECTORY, "wj_transform_age.csv", sep = "/")
TRANSFORMED_GRADE = paste(WOODCOCK_DIRECTORY, "wj_transform_grade.csv", sep = "/")
OUT_FILE = paste(BASE_DIRECTORY, "ace_processed_first_block", "seacrest_first_block_concise.csv", sep = "/")

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

# helper function 
standardize_pid = function(x) {
  standardized = sapply(x, function(y) { 
    id_split = stringr::str_split(y, "-")[[1]]
    id = id_split[3]
    return (paste0("ADMIN-UCSF-", gsub("b", "", id)))
  })
  return (as.vector(standardized))
}

# load demographics
demographics = load_ace_demographics(DEMOGRAPHICS_FILE)

# subset demographics
seacrest_demographics = subset(demographics, study_name == "Sea Crest Pilot")

# load transformed woodcock metrics
woodcock_age = aceR:::load_woodcock_transformed(TRANSFORMED_AGE, "age")
woodcock_grade = aceR:::load_woodcock_transformed(TRANSFORMED_GRADE, "grade")

# load raw data and process it
dat = load_ace_bulk(path = RAW_SEACREST_DIRECTORY, pattern = ".csv", recursive = FALSE)
proc = proc_by_module(dat, verbose = TRUE)

all_tasks = aceR:::subset_first_block_for_tasks(proc)

# standardize pids all around
seacrest_demographics$pid = standardize_pid(seacrest_demographics$pid)
all_tasks$pid = standardize_pid(all_tasks$pid)
woodcock_age$pid = standardize_pid(woodcock_age$pid)
woodcock_grade$pid = standardize_pid(woodcock_grade$pid)

# merge, clean, and export
clean_subset = all_tasks[, c("pid", EFA_VARS)]
clean_demo = merge(clean_subset, seacrest_demographics, by = "pid")
clean_woodcock = merge(clean_demo, woodcock_age, by = "pid")
clean_woodcock = merge(clean_woodcock, woodcock_grade, by = "pid")
write.csv(clean_woodcock, OUT_FILE, na = "")