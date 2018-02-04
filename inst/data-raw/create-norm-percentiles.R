# making norming dists

# proc_ace_t1 is "wide" output data from proc_by_module() called on Jessica Younger's prepared trialwise data

proc_ace_t3_wide = full_join(proc_ace_t3$ALL_OTHER_DATA,
                             filter(proc_ace_t3$FILTER, FILTER.distractors == 0)) %>%
  distinct() %>%
  mutate(BOXED.score = (((BOXED.rt_mean.conjunction_12 - BOXED.rt_mean.conjunction_4) / BOXED.rt_mean.conjunction_4) * 100) + 100)

proc_ace_t3_4 = proc_ace_t3_wide %>%
  filter(grade == 4)
proc_ace_t3_6 = proc_ace_t3_wide %>%
  filter(grade == 6)
proc_ace_t3_8 = proc_ace_t3_wide %>%
  filter(grade == 8)
ace_t3_norms = vector("list", 0)
for (this.grade in c("fourth", "sixth", "eighth")) {
  data = switch(this.grade,
         fourth = proc_ace_t3_4,
         sixth = proc_ace_t3_6,
         eighth = proc_ace_t3_8)
  
ace_t3_norms[[this.grade]] = data.frame(
  BACKWARDSSPATIALSPAN.object_count_span.overall = quantile(data$BACKWARDSSPATIALSPAN.object_count_span.overall, probs = seq(0, 1, .01), na.rm = TRUE),
  SPATIALSPAN.object_count_span.overall = quantile(data$SPATIALSPAN.object_count_span.overall, probs = seq(0, 1, .01), na.rm = TRUE),
  FILTER.k.2 = quantile(data$FILTER.k.2, probs = seq(0, 1, .01), na.rm = TRUE),
  FLANKER.rt_mean.cost = sort(quantile(data$FLANKER.rt_mean.cost, probs = seq(0, 1, .01), na.rm = TRUE), decreasing = TRUE),
  STROOP.rt_mean.cost = sort(quantile(data$STROOP.rt_mean.cost, probs = seq(0, 1, .01), na.rm = TRUE), decreasing = TRUE),
  SAAT.rt_mean.sustained = sort(quantile(data$SAAT.rt_mean.sustained, probs = seq(0, 1, .01), na.rm = TRUE), decreasing = TRUE),
  SAAT.rt_mean.impulsive = sort(quantile(data$SAAT.rt_mean.impulsive, probs = seq(0, 1, .01), na.rm = TRUE), decreasing = TRUE),
  TNT.rt_mean.cost = sort(quantile(data$TNT.rt_mean.cost, probs = seq(0, 1, .01), na.rm = TRUE), decreasing = TRUE),
  TASKSWITCH.rt_mean.cost = sort(quantile(data$TASKSWITCH.rt_mean.cost, probs = seq(0, 1, .01), na.rm = TRUE), decreasing = TRUE),
  BOXED.score = sort(quantile(data$BOXED.score, probs = seq(0, 1, .01), na.rm = TRUE), decreasing = TRUE))
}

