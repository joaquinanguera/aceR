# load and process all files

rm(list = ls())

options(nwarnings = 500)

# load aceR
library(aceR)

# set paths
DATA_PATH = "~/Desktop/ACE Studies_Raw Data"
RELEASE_PATH = "~/Desktop/ace_process"
setwd(DATA_PATH)

# exclude problematic subdirectories
problematic_subdirectories = c(
  "Dan's Raw ACE Data", 
  "Original Files")

# exclude problematic files
problematic_files = c(
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
  "AE/SAAT-48.csv",
  "AE/SAAT-67.csv",
  "AE/SAAT-84.csv",
  "i3/BRT-94.csv",
  "i3/BRT-104.csv",
  "i3/i018/SpatialSpan.csv",
  "i3/BRTAll.csv")

problematic_brighten = c("brt_filtered.xlsx", "saat_filtered.xlsx")

ninety_nine_problems = c(problematic_subdirectories, problematic_files, problematic_brighten)

# load and process each subdirectory individually

# datasets = list.dirs(recursive = FALSE)
datasets = c("Brighten")
for (dset in datasets) {
  
  # load and process
  dat = load_ace_bulk(path = dset, exclude = ninety_nine_problems)
  proc = proc_by_module(dat, verbose = TRUE)
  
  # export 
  out_path = paste(RELEASE_PATH, dset, sep = "/")
  export_csv(proc, out_path)
  
}

