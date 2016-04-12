# misc. script for transforming Woodcock Johnson Battery data

rm(list = ls())

library(aceR)
library(reshape)

# helper functions

capitalize <- function(x) {
  cap = gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", x, perl = TRUE)
  return (cap)
}


# load
WORKING_DIR = "~/Desktop"
file_name = "School Pilot WJ Data GRADE.xlsx"
file = paste(WORKING_DIR, file_name, sep = "/")
dat = openxlsx::read.xlsx(file, sheet = 1, colNames = FALSE)

# clean
header = dat[1,]
dat = dat[-1,] # get rid of header
dat = aceR:::remove_empty_rows(dat)

# init output
out = data.frame()

# group by student id & transform
sids = which(!is.na(dat[, 1]))
num_sids = length(sids)
grows = seq(1:num_sids)
gdat = list()
for (i in grows) {
  
  # group
  first = sids[i]
  last = ifelse(i == num_sids, nrow(dat), sids[i + 1] - 1) 
  sdat = dat[first:last, ]
  sid = sdat[1, 1]
  names(sdat) = header
  
  # transform

  prefix = tolower(sdat[, 3 ])
  prefix = capitalize(prefix)
  prefix = aceR:::replace_spaces(prefix, "")
  
  wood_dat = sdat[, 3:length(sdat)]
  wood_dat[, 1] = prefix
  id = names(wood_dat)[1]
  wood_melt = reshape::melt(wood_dat, id=id)
  
  user_dat = data.frame(
    val = wood_melt$value
  )
  user_dat_t = data.frame(t(user_dat))
  names(user_dat_t) = paste(wood_melt[, 1], wood_melt[, 2], sep = "_")
  
  # merge

  wood = data.frame(
    sid = sdat[1, 1],
    date = sdat[1, 2]
  )
  wood = merge(wood, user_dat_t)
  out = plyr::rbind.fill(out, wood)
}


