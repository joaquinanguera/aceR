# script for processing ALL data.
# assumes all files are unprocessed

rm(list = ls())

library(aceR)

DATA_PATH = "~/Desktop/ACE Studies_Raw Data/Summer 2015 School Data"
RELEASE_PATH = "~/Desktop/ACE Processed"

# ACE Raw Data (*Processed Data Omitted)
setwd(DATA_PATH)

subdirs_to_ignore = c(
  "Brighten", 
  "Remaining _Jyoti", 
  "Remaining Raw Data (Brighten, BBT-MT, India, 16p-Adaptivity)", 
  "Dan's Raw ACE Data")

files_to_ignore = c(
  "AgileAcademic-IAN", 
  "AgileEnvironments_ACE_pre-assessment-rankings_only", 
  "final_ae_rankings",
  "FlankerAll", 
  "FlankerAllWithDate", 
  "FlankerCatchAllPost",
  "FlankerCatchAllPre",
  "SAATAll",
  "SAATAllWithDate",
  "SAATCatchAllPost",
  "SAATCatchAllPre",
  "SpatialSpanAll",
  "SpatialSpanAllWithDate",
  "SpatialSpanCatchAllPre",
  "SpatialSpanCatchAllPost",
  "AE/BRT-67.csv",
  "AE/TNT-1.csv",
  "AE/TNT.csv",
  "i3/BRT-94.csv",
  "i3/BRT-104.csv",
  "i3/i018/SpatialSpan.csv",
  "i3/BRTAll.csv")

to_ignore = c(subdirs_to_ignore, files_to_ignore)

raw_dat = load_ace_bulk(exclude = to_ignore)
proc_dat = proc_by_module(raw_dat, verbose = TRUE)

setwd(RELEASE_PATH)
export_csv(proc_dat)

# TODO: load "filtered" ace data