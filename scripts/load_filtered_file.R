
library("aceR")
library("openxlsx")

DATA_PATH = "~/Desktop/ACE Studies_Raw Data/Brighten"

setwd(DATA_PATH)

file = "brt_filtered.xlsx" # has "key" column
# file = "saat_filtered.xlsx" # no "key" column

if (aceR:::is_excel(file)) {
  df = openxlsx::read.xlsx(file, sheet = 1)
} else {
  df = read.csv(file, header = TRUE, sep = ",")
}
cols = names(df)
pid_col = cols[grep("id", cols)[1]]
has_key = "key" %in% cols