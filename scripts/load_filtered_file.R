
rm(list = ls())

library("aceR")
library("openxlsx")

DATA_PATH = "~/Desktop/ACE Studies_Raw Data/Brighten"
RELEASE_PATH = "~/Desktop/brighten_test"

setwd(DATA_PATH)
files = list.files(pattern = "xlsx", recursive = FALSE)

for (file in files) {
  
  setwd(DATA_PATH)
  
  dat = aceR:::load_ace_filtered_file(file)
  proc = proc_by_module(dat)
  
  setwd(RELEASE_PATH)
  out = paste0("proc-", file, ".csv")
  write.csv(proc, out)
}
