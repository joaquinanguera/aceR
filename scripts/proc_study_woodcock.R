# some stats for woodcock data and plots

rm(list = ls())

library(aceR)
library(stringr)
library(ggplot2)

# set directory 

WORKING_DIR = "~/Desktop"
setwd(WORKING_DIR)

# load transformed woodcock data
woodcock = load_files(pattern = "transform")
cols = names(woodcock)

# subset SS data
woodcock_ss = woodcock[str_detect(cols, "SS")]
woodcock_ss_score = apply(woodcock_ss, 2, function(x) {
  score = ifelse(is.na(x), NA, str_extract(x, "(^|\\s)([0-9]+)($|\\s)"))
  score = ifelse(is.na(score), NA, gsub("\t\n\r\v\f", "", score))
  score = ifelse(is.na(score), NA, as.numeric(as.character(score)))
  return (score)
})
woodcock_ss_score = as.data.frame(woodcock_ss_score)

# re-add participant info

woodcock_ss_score = cbind(woodcock[, c("pid", "study_name", "cohort_group")], woodcock_ss_score)

# load ACE data
ace = load_files(path = "ace_process/Sea Crest School")
ace$module = as.character(ace$module)
ace$pid = as.character(ace$pid)
ace_by_task = aceR:::subset_by_col(ace, "module")

for (task in ace_by_task) {
  first_block = aceR:::subset_first_block(task)
  task_name = first_block$module[1]
  
  task_dat = merge(woodcock_ss_score, task, by = "pid")
  y_variables = aceR:::multi_filter_vec(names(task_dat), c("rt_mean.overall"))
  x_variables = aceR:::multi_filter_vec(names(task_dat), c("SS"))
  
  # TODO: move this all into aceR
  for (x in x_variables) {
    task_dat[, x] = as.numeric(as.character(task_dat[, x]))
    for (y in y_variables) {
      plot_desc = paste(task_name, ":", y, "by", x, sep = " ")
      print(plot_desc)
      task_dat[, y] = as.numeric(as.character(task_dat[, y]))
      mod = lm(task_dat[, y] ~ task_dat[, x], na.action = na.omit)
      mod_summary = summary(mod)$coefficients
      slope = mod_summary[2]
      intercept = mod_summary[1]

      ggplot() + 
        geom_point(aes(task_dat[, x], task_dat[, y]), task_dat) +
        ggtitle(plot_desc) + 
        xlab(x) + 
        ylab(y) +
        geom_abline(slope = slope, intercept = intercept, colour='#E41A1C')
      
      print(mod)
      print(mod_summary)
    }
  }
  
}

