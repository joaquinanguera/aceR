# misc. script for transforming Woodcock Johnson Battery data

rm(list = ls())

library(aceR)

# load
WORKING_DIR = "~/Desktop"
file_name = "School Pilot WJ Data GRADE.xlsx"
file = paste(WORKING_DIR, file_name, sep = "/")
dat = openxlsx::read.xlsx(file, sheet = 1, colNames = FALSE)

# clean
dat = dat[-1,] # get rid of header (maybe)
dat = aceR:::remove_empty_rows(dat)

# group by student id
sids = which(!is.na(dat[, 1]))
num_sids = length(sids)
grows = seq(1:num_sids)
gdat = list()
for (i in grows) {
  first = sids[i]
  last = ifelse(i == num_sids, nrow(dat), sids[i + 1] - 1) 
  sdat = dat[first:last, ]
  sid = sdat[1, 1]
  gdat[[sid]] = sdat
}

