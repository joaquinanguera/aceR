# script for processing seacrest data

rm(list = ls())

library(aceR)
library(stringr)
library(plyr)

# define constants

BASE_DIRECTORY = "~/Desktop/seacrest"
RAW_SEACREST_DIRECTORY = paste(BASE_DIRECTORY, "raw_email_files", sep = "/")
MISSING_FILES_DIRECTORY = paste(BASE_DIRECTORY, "remaining_files", sep = "/")
PARTICIPANT_INFO_DIRECTORY = paste(BASE_DIRECTORY, "participant_info_files", sep = "/")

DEMOGRAPHICS_FILE = paste(PARTICIPANT_INFO_DIRECTORY, "All Participant Demographics.xlsx", sep = "/")
AGE_FILE = paste(PARTICIPANT_INFO_DIRECTORY, "School Pilot WJ Data AGE.xlsx", sep = "/")
GRADE_FILE = paste(PARTICIPANT_INFO_DIRECTORY, "School Pilot WJ Data GRADE.xlsx", sep = "/")

OUT_FILE = paste(BASE_DIRECTORY, "seacrest_first_block_concise.csv", sep = "/")

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

load_missing_files = function(path) {
  files = list.files(path, pattern = ".csv")
  out = data.frame()
  for (file in files) {
    file_path = paste(path, file, sep = "/")
    dat = aceR:::load_ace_filtered_file(file_path)
    out = plyr::rbind.fill(out, dat)
  }
  return (out)
}

# load demographics
demographics = load_ace_demographics(DEMOGRAPHICS_FILE)

# subset demographics
seacrest_demographics = subset(demographics, study_name == "Sea Crest Pilot")
seacrest_demographics$pid = standardize_seacrest_pid(seacrest_demographics$pid)

# load & transform woodcock metrics
woodcock_age_transformed = aceR:::transform_woodcock(AGE_FILE)
woodcock_grade_transformed = aceR:::transform_woodcock(GRADE_FILE)

# grab woodcock variables
woodcock_age = aceR:::grab_woodcock_data(woodcock_age_transformed, "age")
woodcock_grade = aceR:::grab_woodcock_data(woodcock_grade_transformed, "grade")

# load raw data
dat = load_ace_bulk(path = RAW_SEACREST_DIRECTORY, pattern = ".csv", recursive = FALSE)

# load missing data
dat_missing = load_missing_files(path = MISSING_FILES_DIRECTORY)
dat_missing$pid = standardize_seacrest_pid(dat_missing$pid)

# merge missing files with 
dat = plyr::rbind.fill(dat, dat_missing)
dat$pid = standardize_seacrest_pid(dat$pid)
proc = proc_by_module(dat, verbose = TRUE)

all_tasks = aceR:::subset_first_block_for_tasks(proc)
all_tasks$pid = standardize_seacrest_pid(all_tasks$pid)

# merge, clean, and export
clean_subset = all_tasks[, c("pid", EFA_VARS)]
clean_demo = merge(clean_subset, seacrest_demographics, by = "pid")
clean_woodcock = merge(clean_demo, woodcock_age, by = "pid", all = TRUE)
clean_woodcock = merge(clean_woodcock, woodcock_grade, by = "pid", all = TRUE)
write.csv(clean_woodcock, OUT_FILE, na = "")