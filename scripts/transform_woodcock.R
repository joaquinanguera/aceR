# misc. script for transforming Woodcock Johnson Battery data

rm(list = ls())

library(aceR)
library(reshape)
library(plyr)

# set directory 

BASE_DIRECTORY = "~/Desktop/ace"
DEMOGRAPHICS_FILE = paste(BASE_DIRECTORY, "All Participant Demographics.xlsx", sep = "/")

GRADE_FILE = paste(BASE_DIRECTORY, "woodcock", "School Pilot WJ Data GRADE.xlsx", sep = "/")
AGE_FILE = paste(BASE_DIRECTORY, "woodcock", "School Pilot WJ Data AGE.xlsx", sep = "/")

GRADE_OUT_FILE = paste(BASE_DIRECTORY, "woodcock", "wj_transform_grade.csv", sep = "/")
AGE_OUT_FILE = paste(BASE_DIRECTORY, "woodcock", "wj_transform_age.csv", sep = "/")

grade_transformed = aceR:::transform_woodcock(GRADE_FILE)
age_transformed = aceR:::transform_woodcock(AGE_FILE)

write.csv(grade_transformed, GRADE_OUT_FILE)
write.csv(age_transformed, AGE_OUT_FILE)
