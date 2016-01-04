# script for processing ALL raw ACE data in specified directory
# assumes all files are unprocessed 'by hand'

rm(list = ls())

options(nwarnings = 500)

library(aceR)

DATA_PATH = "~/Desktop/ACE Studies_Raw Data"
RELEASE_PATH = "~/Desktop/ACE Processed"
PROC_ALL = "_all"

setwd(DATA_PATH)

load_proc_and_write = function(subdir, FUN, ...) {
  # reset base directory
  setwd(DATA_PATH)
  
  # define paths
  rel_path = paste(RELEASE_PATH, subdir, sep = "/")
  sub_path = paste(DATA_PATH, subdir, sep = "/")
  dir.create(file.path(RELEASE_PATH, subdir), showWarnings = FALSE)
  
  # load & process data
  setwd(sub_path)
  raw_dat = FUN(...)
  proc_dat = proc_by_module(raw_dat, verbose = TRUE)
  
  # export
  setwd(rel_path)
  export_csv(proc_dat)  
}

# exclude problematic subdirectories
subdirs_to_ignore = c("Dan's Raw ACE Data", "Original Files")

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
  "AE/SAAT-48.csv",
  "AE/SAAT-67.csv",
  "AE/SAAT-84.csv",
  "i3/BRT-94.csv",
  "i3/BRT-104.csv",
  "i3/i018/SpatialSpan.csv",
  "i3/BRTAll.csv")

to_ignore = c(subdirs_to_ignore, files_to_ignore)

# load and process each subdirectory individually
# b/c there's > 2000 files and it takes ~1 hour to load them all from scratch.

subdirs = c(
  "16 Person Study", 
  "Adaptivity", 
  "CanDo", 
  "Control Boys", 
  "Control Girls", 
  "Redbull", 
  "SPD Boys", 
  "SPD Girls", 
  "Summer 2015 School Data",
  "Test Retest")
  
subdirs_filtered = c(
  "Brighten", 
  "Remaining _Jyoti", 
  "Remaining Raw Data (Brighten, BBT-MT, India, 16p-Adaptivity)"
)


# load raw ace ("sent-by-email") files
sapply(subdirs, function(x) load_proc_and_write(x, load_ace_bulk, exclude = to_ignore))

# combine all data by task (TODO: clean this up)
setwd(RELEASE_PATH)
proc_files = load_files(recursive = TRUE)
proc_files$module = as.character(proc_files$module)
by_module = aceR:::subset_by_col(proc_files, "module")

dir.create(file.path(RELEASE_PATH, PROC_ALL), showWarnings = FALSE)
setwd(PROC_ALL)
export_csv(by_module)

