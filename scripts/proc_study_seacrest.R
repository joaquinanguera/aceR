# script for processing seacrest data

rm(list = ls())

library(aceR)

# define constants

BASE_DIRECTORY = "~/Desktop/ace"
SEACREST_DIRECTORY = paste(BASE_DIRECTORY, "ACE Studies_Raw Data", "Sea Crest School", sep = "/")
WOODCOCK_DIRECTORY = paste(BASE_DIRECTORY, "woodcock", sep = "/")
DEMOGRAPHICS_FILE = paste(BASE_DIRECTORY, "Sea Crest Demographics.xlsx", sep = "/")
TRANSFORMED_PATTERN = "transform"

EFA_VARIABLES = c(
  "BRT.rt_mean.left",
  "BRT.rt_mean.right",
  "BRT.object_count_span.overall",
  "STROOP.acc_mean.congruent", 
  "STROOP.acc_mean.incongruent", 
  "STROOP.acc_mean.cost", 
  "STROOP.rt_mean.correct.congruent", 
  "STROOP.rt_mean.correct.incongruent", 
  "STROOP.rt_mean.correct.cost",
  "FLANKER.acc_mean.congruent", 
  "FLANKER.acc_mean.incongruent",
  "FLANKER.acc_mean.cost", 
  "FLANKER.rt_mean.congruent", 
  "FLANKER.rt_mean.incongruent", 
  "FLANKER.rt_mean.cost",
  "SAAT.acc_mean.impulsive", 
  "SAAT.acc_mean.sustained", 
  "SAAT.rt_mean.impulsive", 
  "SAAT.rt_mean.sustained",
  "BOXED.acc_mean.overall", 
  "BOXED.rt_mean.overall",
  "TASKSWITCH.rt_mean.stay", 
  "TASKSWITCH.rt_mean.switch",
  "TNT.acc_mean.cost"
  )

# load demographics
demographics = load_ace_demographics(DEMOGRAPHICS_FILE)

# load transformed woodcock metrics
woodcock = aceR:::load_woodcock_transformed(WOODCOCK_DIRECTORY, TRANSFORMED_PATTERN)

# load & process dat
dat = load_ace_bulk()
proc = proc_by_module(dat, TRUE)

setwd("~/Desktop/process")
export_csv(proc)