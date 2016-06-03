
# one-off script to subset the seacrest data from those weird xlsm files in "Remaining Data Pulvinar ACE 03.18.16 (e.g., Rockon, MT_BBT)"
# manually 
#  - shifted columns
#  - removed "id" column
#  - saved as csv

rm(list = ls())

BASE_DIRECTORY = "~/Desktop/ace"
READ_DIRECTORY = paste(BASE_DIRECTORY, "ACE Studies_Raw Data", "Remaining Data Pulvinar ACE 03.18.16 (e.g., Rockon, MT_BBT)", sep = "/")
OUT_DIRECTORY = paste(BASE_DIRECTORY, "ACE Studies_Raw Data", "Sea Crest School Missing", sep = "/")

files = list.files(READ_DIRECTORY, pattern = ".csv")

for (file in files) {
  print(paste(file, "....", sep = ""))
  # load file
  file_path = paste(READ_DIRECTORY, file, sep = "/")
  out_path = paste0(OUT_DIRECTORY, "/missing-seacrest-", file)
  dat = read.csv(file_path, header = TRUE)
  # fix names
  names(dat) = gsub("[.]", "", names(dat))
  
  # subset seacrest data
  pids = dat$participant_id
  seacrest = sapply(pids, function (x) grepl("ADMIN", x, ignore.case = TRUE))
  out = dat[which(seacrest), ]
  
  # export
  write.csv(out, out_path)
}