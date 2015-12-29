# script for processing ALL raw ACE data in specified directory
# assumes all files are unprocessed 'by hand'

rm(list = ls())

library(aceR)

DATA_PATH = "~/Desktop/ACE Studies_Raw Data"
RELEASE_PATH = "~/Desktop/ACE Processed"

setwd(DATA_PATH)

# exclude problematic subdirectories
subdirs_to_ignore = c(
  "Brighten", 
  "Remaining _Jyoti", 
  "Remaining Raw Data (Brighten, BBT-MT, India, 16p-Adaptivity)", 
  "Dan's Raw ACE Data")

# exclude problematic files
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
  "AE/Flanker-139.csv",
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