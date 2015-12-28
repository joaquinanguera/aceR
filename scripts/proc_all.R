# script for processing ALL data.
# assumes all files are unprocessed

rm(list = ls())

library(aceR)

# ACE Raw Data (*Processed Data Omitted)
setwd("~/Desktop/ACE Studies_Raw Data/Summer 2015 School Data")

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
  "AE/BRT-67.csv",
  "AE/TNT-1.csv",
  "AE/TNT.csv",
  "i3/BRT-94.csv",
  "i3/BRT-104.csv",
  "i3/i018/SpatialSpan.csv",
  "i3/BRTAll.csv")

to_ignore = c(subdirs_to_ignore, files_to_ignore)

standard_ace_dat = load_ace_bulk(exclude = to_ignore)

# TODO: load "filtered" ace data